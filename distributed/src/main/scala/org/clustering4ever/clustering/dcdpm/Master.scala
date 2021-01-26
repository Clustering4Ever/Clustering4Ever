package org.clustering4ever.clustering.dcdpm

import org.apache.commons.math3.distribution.{BetaDistribution, GammaDistribution, MultivariateNormalDistribution}

import scala.collection.mutable

object DCDPMUtils extends Serializable {
  
  def fillσ1σ2(dim: Int, varianceInClusters: Double, varianceBetweenCenters: Double): (Array[Array[Double]], Array[Array[Double]]) = {
    val range = (0 until dim)
    val σ1 = Array.ofDim[Double](dim, dim)
    val σ2 = Array.ofDim[Double](dim, dim)
    range.foreach{ i =>
      range.foreach{ j =>
        if (i == j) {
          σ1(i)(j) = varianceInClusters
          σ2(i)(j) = varianceBetweenCenters
        }
        else {
          σ1(i)(j) = 0D
          σ2(i)(j) = 0D
        }
      }
    }
    (σ1, σ2)
  }

}

trait MasterWorkerCommons[Clust <: AbstractCluster] extends Serializable {

  val varianceInClusters: Double

  val varianceBetweenCenters: Double

  private[clustering4ever] def max(proba: scala.collection.Map[Int, Double]): Int = proba.maxBy(_._2)._1

  val clusterVector = new mutable.ArrayBuffer[Clust]

  private[clustering4ever] def getCluster(id: Int): Clust = {
    val index = clusterVector.indexWhere(_.id == id)
    clusterVector(index)
  }
  
  private[clustering4ever] def addCluster(index: Int, c: Clust): Unit = {
    clusterVector.insert(index, c)
  }
 
  private[clustering4ever] def removePointFromCluster(clusterId: Int): Unit = {
    val index = clusterVector.indexWhere(_.id == clusterId)
    clusterVector(index).size -= 1
  }

}

final case class Master(
  val varianceInClusters: Double,
  val varianceBetweenCenters: Double,
  val n: Int,
  var gamma: Double,
  val parameters: mutable.ArrayBuffer[(Array[Double], Array[Array[Double]])],
  val dim: Int
) extends MasterWorkerCommons[GlobalCluster] {
  
  val h = 1
  val c = mutable.Map.empty[Int, Int]  
  val individuals = mutable.ArrayBuffer.empty[GlobalCluster]
  
  val m = Array.fill(dim)(0D)

  val (σ1, σ2) = DCDPMUtils.fillσ1σ2(dim, varianceInClusters, varianceBetweenCenters)

  def createisNewCluster(y: GlobalCluster): mutable.Map[Int, Int] = {
    
    val id = clusterVector.size
    val superCluster = new GlobalCluster(mutable.ArrayBuffer.empty[(Int, Int)], id, y.size, y.yy, y.phi, y.mean, y.σ2)
    
    superCluster.updatePhi(σ1)
    clusterVector += superCluster
    c += y.id -> id
    
  }

  def removeCluster(id: Int): GlobalCluster = {
    clusterVector.remove(id)
  }
  
  def updateClusterId(id: Int, newId: Int): Unit = {
    val index = clusterVector.indexWhere(cluster => (cluster.id == id))
    clusterVector(index).updateId(newId)
  }
  
  def initialize(): Unit = {
    clusterVector.clear
    for (y <- individuals){
      createisNewCluster(y)
    }
  }
  
  def label(pointId: Int) = {
    
    val clusterId = c(pointId)
    
    //Update Clusters
    for (cluster <- clusterVector) {
      if (cluster.id > clusterId) {
        updateClusterId(cluster.id, cluster.id - 1)    
      }
    }
    //Update C
    for (j <- (c - pointId).keys) {
      if (c(j) > c(pointId)){
        val temp = c(j) - 1
        c += (j -> temp)  
      }
    }
    
  }

  def gibbsSampling() = {

    var indice = 0

    for(iter <- 0 to 39) {
    
      // for each data point
      for(y <- individuals) {
          
        val oldC = c(y.id)  
        val retire = if (getCluster(oldC).size == y.size) {
          //Delete it
          removeCluster(oldC)
          label(y.id)
          true
        }
        else {
          removePointFromCluster(c(y.id))
          false
        }
          
        val k = clusterVector.size
        // innovation          
        addAuxiliaryParameters(h, y)

        c += (y.id -> max(calculateProbaWithLog(y)))        
         
        if (c(y.id) >= k) {
     
          var cluster = getCluster(c(y.id))
          cluster.id = k
          cluster.size = y.size
          c += y.id -> k
          addCluster(k, cluster)
     
        }
        else {
          getCluster(c(y.id)).size += y.size
        }
        
        if (!retire) {
          val effective = y.size
          val index2 = clusterVector.indexWhere(cluster => cluster.id == oldC)
          clusterVector(index2).size -= effective - 1
        }
        
        for (j <- 0 until h) {
          val index = clusterVector.length - 1
          clusterVector.remove(index)
        }
   
      }
      
      for(cluster <- clusterVector) { 
        
        val dataOfCluster = mutable.ArrayBuffer.empty[GlobalCluster]
        // data of cluster
        for (y <- individuals)
          if (c(y.id) == cluster.id)
            dataOfCluster += y
 
       cluster.updateyy(dataOfCluster)
        

      }
      
      // update phis
      clusterVector.foreach{ cluster => 
        cluster.updatePhi(σ1)
      }

      gamma = gammaInference(gamma, 1, 0.5, clusterVector.size, n)
      
  }
    
    var count = 0
    var count2 = 0
    
    for (cluster <- clusterVector) { 
        // data of cluster
        for (y <- individuals.filter{ case p => c(p.id) == cluster.id }) {
          for (sub <- y.subIds) {
            cluster.subIds += sub
            count += 1
          }
        }
        
      count2 += cluster.subIds.size

    }
    
    clusterVector
    
  }
  
  def addAuxiliaryParameters(h: Int, y: GlobalCluster): Unit = {
    val m = Array.fill(dim)(0D)
    val a = Array.fill(dim)(0D)
    (0 until h).foreach{ _ =>
      val id = clusterVector.length
      val superCluster = new GlobalCluster(mutable.ArrayBuffer.empty[(Int, Int)], id, gamma, a, y.phi, m, σ2)  
      clusterVector += superCluster
    }
  }
  
  def calculateProbaWithLog(y: GlobalCluster): scala.collection.Map[Int, Double] = {
  
    val probaSeq = clusterVector.map{ cluster =>
      val value = (Math.log(cluster.size) + Math.log(normalLikelihood(y.yy, cluster.phi, σ1)) - Math.log(n - y.size + gamma))
      cluster.id -> value
    }

    val proba = mutable.Map(probaSeq:_*)

    val max = proba.maxBy(_._2)._2
   
    for ((cluster, p) <- proba) {
      proba += cluster -> Math.exp(p - max)
    }
    
    val sum = proba.foldLeft(0D){ case (a, (k, v)) => a + v } 
    
    for ((cluster, p) <- proba) {
      proba += (cluster -> p / sum)
    }

    proba
  }
  
  def normalLikelihood(data: Array[Double], mean: Array[Double], σ1: Array[Array[Double]]): Double ={
    // var vraisemblance: Double = 1
    val normalDist = new MultivariateNormalDistribution(mean, σ1)
    normalDist.density(data)
  }

  
  def updateExistingClasses(existingK: Int, cent: Array[ResumeCluster]): Unit = {
    
    val centers = mutable.ArrayBuffer[ResumeCluster](cent:_*)
   
    individuals.clear  
    
    var id = 0
    
    if (existingK != 0) {

      (0 until existingK).foreach{ i =>

        val (mergedClusters, subIds) = centers.collect{ case y if y.id == i =>
          (y, y.workerId -> i)
        }.unzip

        if (mergedClusters.nonEmpty) {

          mergedClusters.foreach(centers -= _)
          val a = Array.fill(dim)(0D)

          val sum = mergedClusters.foldLeft(0D){ case (a, b) => a + b.size } 
          val superCluster = GlobalCluster(subIds, id, sum, a, mergedClusters(0).phi, parameters(mergedClusters(0).id)._1, parameters(mergedClusters(0).id)._2)

          superCluster.updateyy(mergedClusters)
          individuals += superCluster
          id += 1

        }
      }
    }
    
    centers.foreach{ y =>
      val subIds = mutable.ArrayBuffer(y.workerId -> y.id)
      val superCluster = GlobalCluster(subIds, id, y.size, y.yy , y.phi, m, σ2)
      individuals += superCluster      
      id += 1
    }

  }
 
  def gammaInference(gamma: Double, a: Double, b: Double, k: Int, n: Int): Double = {
   
    val beta = new BetaDistribution(gamma + 1, n)
    val eta = beta.sample
    val pi = (a + k - 1)/(a + k - 1 + n * (b - Math.log(eta)))  
    val u = scala.util.Random.nextDouble
   
    if (u <= pi) {
      val gamma = new GammaDistribution(a + k, b - Math.log(eta))
      gamma.sample
    }
    else {
      val gamma = new GammaDistribution(a + k - 1, b - Math.log(eta))
      gamma.sample
    }    
  }
}
