package clustering4ever.spark.clustering.kmeans

import scala.collection.{immutable, mutable}
import scala.util.Random
import org.apache.spark.{SparkContext, HashPartitioner}
import org.apache.spark.rdd.RDD
import scala.annotation.meta.param
import scala.reflect.ClassTag
import scala.math.{min, max}
import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.clustering4ever.math.distances.scalar.Euclidean
import _root_.clustering4ever.clustering.ClusteringAlgorithms
import _root_.clustering4ever.util.SumArrays
import _root_.clustering4ever.spark.clustering.accumulators.{CentroidsScalarAccumulator, CardinalitiesAccumulator}
import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes

/**
 * @author Beck Gaël
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : an Array with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param iterMax : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding ContinuousDistances distance function
 **/
class KMeans(
	@transient val sc: SparkContext,
	data: RDD[Array[Double]],
	var k: Int,
	var epsilon: Double,
	var maxIter: Int,
	var metric: ContinuousDistances
) extends ClusteringAlgorithms[Long, Double]
{
	type CentroidsMap = mutable.HashMap[Int, Array[Double]]

	def obtainNearestModID(v: Array[Double], kModesCentroids: CentroidsMap): Int =
	{
		kModesCentroids.toArray.map{ case(clusterID, mod) => (clusterID, metric.d(mod, v)) }.sortBy(_._2).head._1
	}

	def run(): KMeansModel =
	{
		val dim = data.first.size
		
		def initializationModes() =
		{
			val vectorRange = (0 until dim).toArray

			def obtainMinMax(idx: Int, vminMax1: (Array[Double], Array[Double]), vminMax2: (Array[Double], Array[Double])) =
			{
				(
					min(vminMax1._1(idx), vminMax2._1(idx)),
					max(vminMax1._2(idx), vminMax2._2(idx))
				)
			}

			val (minv, maxv) = data.map( v => (v, v) ).reduce( (minMaxa, minMaxb) =>
			{
				val minAndMax = for( i <- vectorRange ) yield( obtainMinMax(i, minMaxa, minMaxb) )
				minAndMax.unzip
			})

			val ranges = minv.zip(maxv).map{ case (min, max) => (max - min, min) }
			val modes = mutable.HashMap((0 until k).map( clusterID => (clusterID, ranges.map{ case (range, min) => Random.nextDouble * range + min }) ):_*)
			modes
		}
		
		val centroids = initializationModes()
		val centroidsUpdated = centroids.clone
		val clustersCardinality = centroids.map{ case (clusterID, _) => (clusterID, 0L) }
		var cpt = 0
		var allModHaveConverged = false
		while( cpt < maxIter && ! allModHaveConverged )
		{
			if( metric.isInstanceOf[Euclidean] )
			{
				val info = data.map( v => (obtainNearestModID(v, centroids), (1L, v)) ).reduceByKey{ case ((sum1, v1), (sum2, v2)) => (sum1 + sum2, SumArrays.sumArraysNumerics(v1, v2)) }.map{ case (clusterID, (cardinality, preMean)) => (clusterID, preMean.map(_ / cardinality), cardinality) }.collect

				info.foreach{ case (clusterID, mean, cardinality) =>
				{
					centroidsUpdated(clusterID) = mean
					clustersCardinality(clusterID) = cardinality
				}}

				allModHaveConverged = centroidsUpdated.forall{ case (clusterID, uptMod) => metric.d(centroids(clusterID), uptMod) <= epsilon }
				
				centroidsUpdated.foreach{ case (clusterID, mod) => centroids(clusterID) = mod }	
			}
			else
			{
				println("Results will have no sense for the moment with another distance than Euclidean, but we're working on it")
			}
			cpt += 1
		}
		new KMeansModel(centroids, clustersCardinality, metric)
	}
}


object KMeans extends DataSetsTypes[Long, Double]
{
	def run(@(transient @param) sc: SparkContext, data: RDD[Array[Double]], k: Int, epsilon: Double, maxIter: Int, metric: ContinuousDistances): KMeansModel =
	{
		val kmeans = new KMeans(sc, data, k, epsilon, maxIter, metric)
		val kmeansModel = kmeans.run()
		kmeansModel
	}
}