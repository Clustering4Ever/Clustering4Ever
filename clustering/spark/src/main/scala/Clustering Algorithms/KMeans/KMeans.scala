package clustering4ever.spark.clustering.kmeans

import scala.collection.{immutable, mutable}
import scala.util.Random
import org.apache.spark.{SparkContext, HashPartitioner}
import org.apache.spark.rdd.RDD
import scala.annotation.meta.param
import scala.reflect.ClassTag
import scala.math.{min, max}
import clustering4ever.math.distances.ContinuousDistances
import clustering4ever.clustering.datasetstype.ClusteringTypes
import clustering4ever.clustering.ClusteringAlgorithms
import _root_.clustering4ever.util.SumArrays

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
	val data: RDD[(Long, Array[Double])],
	var k: Int,
	var epsilon: Double,
	var maxIter: Int,
	var metric: ContinuousDistances
) extends ClusteringAlgorithms[Long, Double, RDD[(Int, (Long, Array[Double]))]]
{
	type CentroidsMap = mutable.HashMap[Int, Array[Double]]

	def obtainNearestModID(v: Array[Double], kModesCentroids: CentroidsMap): Int = kModesCentroids.toArray.map{ case(clusterID, mod) => (clusterID, metric.d(mod, v)) }.sortBy(_._2).head._1

	def run(): ClusterizedData =
	{
		val dim = data.first._2.size
		
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

			val (minv, maxv) = data.map{ case (_, v) => (v, v) }.reduce( (minMaxa, minMaxb) =>
			{
				val minAndMax = for( i <- vectorRange ) yield( obtainMinMax(i, minMaxa, minMaxb) )
				minAndMax.unzip
			})

			val ranges = minv.zip(maxv).map{ case (min, max) => (max - min, min) }
			val modes = mutable.HashMap((0 until k).map( clusterID => (clusterID, ranges.map{ case (range, min) => Random.nextDouble * range + min }) ):_*)
			modes
		}
		
		var centroids = initializationModes()
		var cpt = 0
		var allModHaveConverged = false
		val centroidsAccumulator = new CentroidsAccumulator(centroids.map{ case (k, v) => (k, Array.fill(dim)(0D)) }, k, dim)
		val cardinalitiesAccumulator = new CardinalitiesAccumulator(centroids.map{ case (k, _) => (k, 0L) }, k)
		sc.register(centroidsAccumulator, "centroidsAccumulator")
		sc.register(cardinalitiesAccumulator, "cardinalitiesAccumulator")

		while( cpt < maxIter && ! allModHaveConverged )
		{
			val labeled = data.foreach{ case (id, v) =>
			{
				val clusterID = obtainNearestModID(v, centroids)
				centroidsAccumulator.addOne(clusterID, v)
				cardinalitiesAccumulator.addOne(clusterID, 1L)
			}}
			centroids = centroidsAccumulator.value.map{ case (clusterID, centroid) => (clusterID, centroid.map(_ / cardinalitiesAccumulator.value(clusterID))) }

			centroidsAccumulator.reset
			cardinalitiesAccumulator.reset

			allModHaveConverged = centroids.forall{ case (clusterID, uptMod) => metric.d(centroids(clusterID), uptMod) <= epsilon }
			
			cpt += 1
		}

		val finalClustering =  data.map{ case (id, v) =>
		{
			val clusterID = obtainNearestModID(v, centroids)
			(clusterID, (id, v))
		}}

		finalClustering
	}
}


object KMeans extends ClusteringTypes[Long, Double, RDD[(Int, (Long, Array[Double]))]]
{
	def run(@(transient @param) sc: SparkContext, data: RDD[(ID, Array[Double])], k: Int, epsilon: Double, maxIter: Int, metric: ContinuousDistances): ClusterizedData =
	{
		val kmodes = new KMeans(sc, data, k, epsilon, maxIter, metric)
		kmodes.run()
	}
}