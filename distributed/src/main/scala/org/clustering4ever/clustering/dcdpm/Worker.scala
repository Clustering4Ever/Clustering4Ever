package org.clustering4ever.clustering.dcdpm

import org.apache.commons.math3.distribution.{BetaDistribution, GammaDistribution, MultivariateNormalDistribution}

import scala.collection.mutable

final case class Worker(
  val workerId: Int,
  val data: Array[Point],
  val gamma: Double,
  val varianceInClusters: Double,
  val varianceBetweenCenters: Double,
  var betaU: Double
) extends MasterWorkerCommons[Cluster] {

  val dim: Int = data.head.vector.length
  val nbPoint: Int = data.length
  // assignements
  val clusterIDs = mutable.Map.empty[Int, Int]

  val (σ1, σ2) = DCDPMUtils.fillσ1σ2(dim, varianceInClusters, varianceBetweenCenters)

  val m = Array.fill(dim)(0D)
        
  var alpha: Double = 1
  
  val hh: Int = 3

  def startWithClustering(
    globalClusters: mutable.ArrayBuffer[((Array[Double], Double), mutable.ArrayBuffer[(Int, Int)])],
    u: Double
  ): this.type = {
   
    betaU = u
     
    clusterVector.clear
    val newC = mutable.Map.empty[Int, Int]
    
    for ((((phi, beta), subIds), id) <- globalClusters.zipWithIndex) {
        
      val a = Array.fill(dim)(0D)
      val clust = Cluster(id, 0, a, phi, beta, false)

      val range1: mutable.ArrayBuffer[(Int, Int)] = subIds.filter(_._1 == workerId)

      range1.foreach{ case (_, subC) =>
        val range2: mutable.Map[Int, Int] = clusterIDs.filter{ case (k, v) => v == subC }
        range2.foreach{ case (ci, _) =>
          newC += ci -> id
          clust.addPoint()
        }
      }

      clusterVector += clust
       
    }
    
    clusterIDs.clear
    clusterIDs ++= newC
        
    this
        
  }
  
  def initializeWithClustering(initial: mutable.ArrayBuffer[(Array[Double], Double)], u: Double): this.type = {
    
    betaU = u
    clusterVector.clear
    clusterIDs.clear
    
    initial.zipWithIndex.foreach{ case ((t1, t2), id) =>
      val a = Array.fill(dim)(0D)
      val clust = Cluster(id, 0, a, t1, t2, false)
      clusterVector += clust
    }
    
    data.foreach{ y =>  
      val clusterId: Int = max(calculateProbaWithLog(y))
      clusterIDs += (y.id -> clusterId)
      getCluster(clusterId).addPoint()
    }
        
    this

  }
  
  def gibbsSampling(): mutable.ArrayBuffer[ResumeCluster] = {
    // Why 9 should it be a param
    (0 to 9).foreach{ iter =>
            
      // for each data point
      data.foreach{ y =>
        // remove it from its cluster
        removePointFromCluster(clusterIDs(y.id))

        // the number of distinct cj for j != i  
        val k = clusterVector.size

        val yCluster = getCluster(clusterIDs(y.id))
        // innovation (new clusters)
        addAuxiliaryParameters(hh)
        // assign y 
        clusterIDs += y.id -> max(calculateProbaWithLog(y))

        if (clusterIDs(y.id) >= k) {
          val beta = new BetaDistribution(1, gamma)
          val b = beta.sample
          val cluster = getCluster(clusterIDs(y.id))
          cluster.id = k
          cluster.size = 1
          cluster.beta = b * betaU
          clusterIDs += y.id -> k
          addCluster(k, cluster)
          
          betaU = (1 - b) * betaU
          
        }
        else {
          getCluster(clusterIDs(y.id)).addPoint()
        }
                   
        
        (0 until hh).foreach{ j =>  
          val index = clusterVector.length - 1
          clusterVector.remove(index)
        }
        
      }

      alpha = alphaInference(alpha, 1, 0.5, clusterVector.size, nbPoint)
     
    }

    //calculate means of clusters
    clusterVector.foreach{ cluster =>
      val dataOfCluster = data.filter{ d => clusterIDs(d.id) == cluster.id }
      cluster.calculateMean(dataOfCluster)
    }

    clusterVector.collect{ case cluster if cluster.size > 0 =>
      ResumeCluster(workerId, cluster.id, cluster.size, cluster.yy , cluster.phi)
    }

  }
  
  def addAuxiliaryParameters(hh: Int): Unit = {

    val normalDist2 = new MultivariateNormalDistribution(m, σ2)

    (0 until hh).foreach{ j =>
      val auxiliaryPhi = normalDist2.sample()
      val id = clusterVector.length
      val a = Array.fill(dim)(0D)
      val cluster = Cluster(id, 0, a, auxiliaryPhi, betaU/hh, true)
      clusterVector += cluster
    }
    
  }
  
  def calculateProbaWithLog(y: Point): mutable.Map[Int, Double] ={
    
    val proba = mutable.Map.empty[Int, Double]
    clusterVector.foreach{ cluster =>
      val value = Math.log(cluster.size + alpha * cluster.beta) + Math.log(normalLikelihood(y.vector, cluster.phi, σ1)) - Math.log(nbPoint - 1 + alpha)
      proba += (cluster.id -> value)
    }
  
    val max = proba.values.max

    for ((cluster, p) <- proba) {
      proba += cluster -> Math.exp(p-max)
  
    }
    
    val sum = proba.foldLeft(0D){ case (a, (k, v)) => a + v } 
    
    proba.foreach{ case (cluster, p) =>
      proba += (cluster -> p/sum)
    }

    proba
  }
  
  def normalLikelihood(data: Array[Double], mean: Array[Double], σ1: Array[Array[Double]]): Double ={
    // var vraisemblance = 1D
    val normalDist = new MultivariateNormalDistribution(mean, σ1)
    normalDist.density(data)
  
  }
   
  def alphaInference(alpha: Double, a: Double, b: Double, k: Int, n: Int) = {
    
    val beta = new BetaDistribution(alpha + 1, n)
    val eta = beta.sample
    val pi = (a + k - 1) / (a + k - 1 + n * (b - Math.log(eta)))
    val rand = scala.util.Random
    val u = rand.nextDouble
   
    if (u <= pi) {
      val gamma = new GammaDistribution(a + k, b - Math.log(eta))
      gamma.sample
    }
    else {
      val gamma = new GammaDistribution(a + k - 1, b - Math.log(eta))
      gamma.sample
    }
  }
  
  def contingencyTable(nbGlobalClusters: Int, nbRealClusters: Int): Array[Array[Int]] = {
    val mat = Array.fill[Int](nbGlobalClusters, nbRealClusters)(0)        
    data.foreach( y => mat(clusterIDs(y.id))(y.realCluster - 1) += 1 )
    mat  
  }

  def dataCluster(clusterId: Int): Array[Point] = {
    data.filter( d => clusterIDs(d.id) == clusterId )    
  }
 
  def localRSS: Double = {
    val dimRange = (0 until dim)
    clusterVector.foldLeft(0D){ (sum1, clust) =>
      sum1 + dataCluster(clust.id).foldLeft(0D){ (sum2, y) =>
        sum2 + dimRange.foldLeft(0D){ (sum3, i) =>
          sum3 + Math.pow(y.vector(i) - clust.phi(i), 2) / varianceInClusters
        }
      }
    }
  }
 
}
