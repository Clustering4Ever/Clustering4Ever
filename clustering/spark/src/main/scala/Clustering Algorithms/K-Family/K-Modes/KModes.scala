package clustering4ever.spark.clustering.kmodes

import scala.collection.{immutable, mutable}
import scala.util.Random
import org.apache.spark.{SparkContext, HashPartitioner}
import org.apache.spark.rdd.RDD
import scala.annotation.meta.param
import scala.reflect.ClassTag
import scala.math.{min, max}
import clustering4ever.math.distances.BinaryDistance
import clustering4ever.clustering.datasetstype.ClusteringTypes
import clustering4ever.clustering.ClusteringAlgorithms
import _root_.clustering4ever.util.SumArrays
import _root_.clustering4ever.spark.clustering.accumulators.{CentroidsBinaryAccumulator, CardinalitiesAccumulator}


/**
 * @author Beck Gaël
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : an Array with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param iterMax : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding BinaryDistance distance function
 **/
class KModes(
	@transient val sc: SparkContext,
	val data: RDD[(Long, Array[Int])],
	var k: Int,
	var epsilon: Double,
	var maxIter: Int,
	var metric: BinaryDistance
) extends ClusteringAlgorithms[Long, Int, RDD[(Int, (Long, Array[Int]))]]
{
	type CentroidsMap = mutable.HashMap[Int, Array[Int]]

	def obtainNearestModID(v: Array[Int], kModesCentroids: CentroidsMap): Int = kModesCentroids.toArray.map{ case(clusterID, mod) => (clusterID, metric.d(mod, v)) }.sortBy(_._2).head._1

	def run(): ClusterizedData =
	{
		val dim = data.first._2.size
		var centroids = mutable.HashMap((for( clusterID <- 0 until k ) yield( (clusterID, Array.fill(dim)(Random.nextInt(2))) )):_*)
		var cpt = 0
		var allModHaveConverged = false
		val centroidsAccumulator = new CentroidsBinaryAccumulator(centroids.map{ case (k, v) => (k, Array.fill(dim)(0)) }, k, dim)
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
			centroids = centroidsAccumulator.value.map{ case (clusterID, centroid) => (clusterID, centroid.map( v => if( 2 * v >= cardinalitiesAccumulator.value(clusterID) ) 1 else 0)) }

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

object KModes extends ClusteringTypes[Long, Int, RDD[(Int, (Long, Array[Int]))]]
{
	def run(@(transient @param) sc: SparkContext, data: RDD[(ID, Array[Int])], k: Int, epsilon: Double, maxIter: Int, metric: BinaryDistance): ClusterizedData =
	{
		val kmodes = new KModes(sc, data, k, epsilon, maxIter, metric)
		kmodes.run()
	}
}