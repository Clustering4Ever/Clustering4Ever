package clustering4ever.spark.clustering.kmodes

import scala.collection.{immutable, mutable}
import scala.util.Random
import org.apache.spark.{SparkContext, HashPartitioner}
import org.apache.spark.rdd.RDD
import scala.annotation.meta.param
import scala.reflect.ClassTag
import scala.math.{min, max}
import clustering4ever.math.distances.BinaryDistance
import _root_.clustering4ever.math.distances.binary.Hamming
import _root_.clustering4ever.clustering.ClusteringAlgorithms
import _root_.clustering4ever.util.SumArrays
import _root_.clustering4ever.spark.clustering.accumulators.{CentroidsBinaryAccumulator, CardinalitiesAccumulator}
import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes

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
	data: RDD[(Long, Array[Int])],
	var k: Int,
	var epsilon: Double,
	var maxIter: Int,
	var metric: BinaryDistance
) extends ClusteringAlgorithms[Long, Int]
{
	type CentroidsMap = mutable.HashMap[Int, Array[Int]]

	def obtainNearestModID(v: Array[Int], kModesCentroids: CentroidsMap): Int = kModesCentroids.toArray.map{ case(clusterID, mod) => (clusterID, metric.d(mod, v)) }.sortBy(_._2).head._1

	def run(): KModesModel =
	{
		val dim = data.first._2.size
		val modes = mutable.HashMap((for( clusterID <- 0 until k ) yield( (clusterID, Array.fill(dim)(Random.nextInt(2))) )):_*)
		val modesCardinality = modes.map{ case (clusterID, _) => (clusterID, 0L) }
		val modesUpdated = modes.clone
		var cpt = 0
		var allModHaveConverged = false
		while( cpt < maxIter && ! allModHaveConverged )
		{
			if( metric.isInstanceOf[Hamming] )
			{
				val info = data.map{ case (id, v) => (obtainNearestModID(v, modes), (1L, v)) }.reduceByKey{ case ((sum1, v1), (sum2, v2)) => (sum1 + sum2, SumArrays.sumArraysNumerics(v1, v2)) }.map{ case (clusterID, (cardinality, preMode)) => (clusterID, preMode.map( x => if( x * 2 >= cardinality ) 1 else 0 ), cardinality) }.collect

				info.foreach{ case (clusterID, mode, cardinality) =>
				{
					modesUpdated(clusterID) = mode
					modesCardinality(clusterID) = cardinality
				}}

				allModHaveConverged = modes.forall{ case (clusterID, uptMod) => metric.d(modes(clusterID), uptMod) <= epsilon }
				
				modesUpdated.foreach{ case (clusterID, mod) => modes(clusterID) = mod }
			}
			else
			{
				println("Results have no sense for the moment")
			}

			cpt += 1
		}

		new KModesModel(modes, modesCardinality, metric)
	}
}

object KModes extends DataSetsTypes[Long, Int]
{
	def run(@(transient @param) sc: SparkContext, data: RDD[(ID, Array[Int])], k: Int, epsilon: Double, maxIter: Int, metric: BinaryDistance): KModesModel =
	{
		val kmodes = new KModes(sc, data, k, epsilon, maxIter, metric)
		val kModesModel = kmodes.run()
		kModesModel
	}
}