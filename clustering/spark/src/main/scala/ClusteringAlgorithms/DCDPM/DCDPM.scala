package org.clustering4ever.clustering.dcdpm

import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.SparkConf 
import org.apache.spark.rdd.RDD 
import scala.util.Try
import org.apache.commons.math3.distribution.GammaDistribution
import org.apache.spark.mllib.clustering.KMeans
import org.apache.spark.mllib.linalg.{Vectors, Vector}
import scala.collection.mutable
/**
 * DCDPM Algorithm
 */
final case class DCDPM(
  val varianceInClusters: Double,
  val varianceBetweenCenters: Double,
  val numRealClusters: Int,
  val nbWorkers: Int,
  val nbDist: Int,
  val numClusters: Int,
  val gamma: Double
) {

  
  def dirichletSample(alpha: Array[Double]): Array[Double] = {
    
    var sum = 0D

    val y = (0 until alpha.size).map{ i =>
      val gamma = new GammaDistribution(alpha(i), 1)
      val out = gamma.sample
      sum += out
      out
    }

    y.map(_ / sum).toArray
      
  }

  val withReal = false
  /**
   * @param data RDD[(ID, Vector, TrueClusterOr0)]
   * @return (RDD[Worker],GlobalClusters)
   */
  def fit(data: RDD[(Int, Array[Double], Int)]): (RDD[Worker], mutable.ArrayBuffer[((Array[Double], Double), mutable.ArrayBuffer[(Int, Int)])]) = {

    val dim = data.first._2.size
    val n = data.count.toInt
    // KMEANS step
    val parsedData = data.map{ case (_, v, _) => Vectors.dense(v) }
    val numKmeansIterations = 10
    // intialize by Kmeans
    val model = KMeans.train(parsedData, numClusters, numKmeansIterations)
    val centers: Array[Vector] = model.clusterCenters
    val clusterInd = model.predict(parsedData)
    val clusterSizes = clusterInd.countByValue.values
    parsedData.unpersist()
      



    val dataRDD = data.map{ case (i, vector, gtClusterID) => Point(i, vector, gtClusterID) }.persist
    

    val m = Array.fill(dim)(0D)
    val (σ1, σ2) = DCDPMUtils.fillσ1σ2(dim, varianceInClusters, varianceBetweenCenters)


    // create workers
    val workers: RDD[Worker] = dataRDD.mapPartitionsWithIndex{ (index, dataPartition) =>
      Iterator(Worker(index, dataPartition.toArray, gamma, varianceInClusters, varianceBetweenCenters, 1))
    }.persist
    
 
    val effectives = new Array[Double](numClusters + 1) 
    clusterSizes.zipWithIndex.foreach{ case (n, i) => effectives(i) = n.toDouble }
    effectives(clusterSizes.size) = gamma
    // sample beta
    val beta: Array[Double] = dirichletSample(effectives)
    val betaU: Double = beta(numClusters)

    val initial = new mutable.ArrayBuffer[(Array[Double], Double)](centers.size)
    
    (0 until centers.size).foreach{ i =>
      initial += centers(i).toArray -> beta(i)
    }

    // initialze mean and σ2 for each global cluster
    val parameters = mutable.ArrayBuffer.fill(numClusters)(m -> σ2)
      
    val initialClusters = mutable.ArrayBuffer.empty[GlobalCluster]
    
    val globalClusters = mutable.ArrayBuffer.empty[((Array[Double], Double), mutable.ArrayBuffer[(Int, Int)])]
    // initialize workers
    val workers2 = Some(
      workers.mapPartitions{ w =>
        Iterator(w.next.initializeWithClustering(initial, betaU))
      }.cache
    )
    

    @annotation.tailrec
    def mainRecursivLoop(j: Int, workersArg: Option[RDD[Worker]], numClustersArg: Int, gammaArg: Double): RDD[Worker] = {

      if (j < nbDist) {
      
        val workersIn = if (j != 0) {
               
          val effectives = new Array[Double](numClusters + 1) 
          initialClusters.zipWithIndex.foreach{ case (c, i) => effectives(i) = c.size.toDouble }
          effectives(initialClusters.size) = gamma

          // sample beta
          val betaIn = dirichletSample(effectives)
          val betaUIn = betaIn(numClusters)

          globalClusters.clear
          parameters.clear

          initialClusters.foreach{ cluster =>
            globalClusters += cluster.phi -> betaIn(cluster.id) -> cluster.subIds
            // post mean and σ2 for each global cluster
            parameters += cluster.mean -> cluster.σ2
          }
          // initialize workers with the new global clusters
          val workersIn = workersArg.get.mapPartitions{ w =>
            Iterator(w.next.startWithClustering(globalClusters, betaUIn))
          }.cache
          
          if (withReal) {
            val contingencyTables = workersIn.mapPartitions{ w =>
              Iterator(w.next.contingencyTable(globalClusters.size, numRealClusters))
            }.collect
            val ari = ARAND.adjustedRandIndex(contingencyTables, nbWorkers, globalClusters.size, numRealClusters, n)
            val doSomethingWithARI: Double => Unit = ???     
          }
          
          lazy val rss: Double = workersIn.mapPartitions{ w => Iterator(w.next.localRSS) }.sum

          Some(workersIn)

        }
        else None

        val resultClustering = workersIn.getOrElse(workersArg.get).mapPartitions{ w =>
          Iterator(w.next.gibbsSampling())
        }.collect
        
        val result: Array[ResumeCluster] = resultClustering.foldLeft(mutable.ArrayBuffer.empty[ResumeCluster])(_ ++= _).toArray
        
        val crp = Master(varianceInClusters, varianceBetweenCenters, n, gamma, parameters, dim)
        crp.updateExistingClasses(numClusters, result)
        crp.initialize      
        
        initialClusters.clear
        initialClusters ++= crp.gibbsSampling() 
         
        val numClustersIn: Int = initialClusters.size
        val gammaIn: Double = crp.gamma
        
        mainRecursivLoop(j + 1, workersArg, numClustersIn, gammaIn)       

      }
      else workersArg.get

    }

    val workersRDD = mainRecursivLoop(0, workers2, numClusters, gamma)

    (workersRDD, globalClusters)

  }

}
