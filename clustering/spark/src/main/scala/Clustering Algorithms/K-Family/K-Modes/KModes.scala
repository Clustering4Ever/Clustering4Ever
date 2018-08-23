package clustering4ever.spark.clustering.kmodes

import scala.collection.{immutable, mutable}
import scala.util.Random
import scala.annotation.meta.param
import scala.reflect.ClassTag
import scala.math.{min, max}
import org.apache.spark.{SparkContext, HashPartitioner}
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import clustering4ever.math.distances.BinaryDistance
import clustering4ever.math.distances.binary.Hamming
import clustering4ever.clustering.ClusteringAlgorithms
import clustering4ever.util.SumArrays
import clustering4ever.spark.clustering.accumulators.{CentroidsBinaryAccumulator, CardinalitiesAccumulator}
import clustering4ever.clustering.datasetstype.DataSetsTypes
import clustering4ever.scala.clusterizables.BinaryClusterizable

/**
 * @author Beck Gaël
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : an Seq with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param iterMax : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding BinaryDistance distance function
 **/
class KModes[ID: Numeric, Obj <: Serializable](
	@transient val sc: SparkContext,
	data: RDD[BinaryClusterizable[ID, Obj]],
	var k: Int,
	var epsilon: Double,
	var maxIter: Int,
	var metric: BinaryDistance[Seq[Int]],
	val initializedCenters: mutable.HashMap[Int, Seq[Int]] = mutable.HashMap.empty[Int, Seq[Int]],
	var persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY
) extends ClusteringAlgorithms[ID]
{
	type CentersMap = mutable.HashMap[Int, Seq[Int]]

	private[this] val binaryDS = data.map(_.vector).persist(persistanceLVL)

	private[this] def obtainNearestModeID(v: Seq[Int], kModesCenters: CentersMap): Int = kModesCenters.minBy{ case(clusterID, mode) => metric.d(mode, v) }._1

	def run(): KModesModel =
	{
		val dim = binaryDS.first.size
		val centers = if( initializedCenters.isEmpty ) mutable.HashMap((for( clusterID <- 0 until k ) yield( (clusterID, Seq.fill(dim)(Random.nextInt(2))) )):_*) else initializedCenters
		val centersCardinality = centers.map{ case (clusterID, _) => (clusterID, 0L) }
		val centersUpdated = centers.clone
		var cpt = 0
		var allModHaveConverged = false
		while( cpt < maxIter && ! allModHaveConverged )
		{
			if( metric.isInstanceOf[Hamming] )
			{
				val info = binaryDS.map( v => (obtainNearestModeID(v, centers), (1L, v)) ).reduceByKey{ case ((sum1, v1), (sum2, v2)) => (sum1 + sum2, SumArrays.sumArraysNumerics[Int](v1, v2)) }.map{ case (clusterID, (cardinality, preMode)) => (clusterID, preMode.map( x => if( x * 2 >= cardinality ) 1 else 0 ), cardinality) }.collect

				info.foreach{ case (clusterID, mode, cardinality) =>
				{
					centersUpdated(clusterID) = mode
					centersCardinality(clusterID) = cardinality
				}}

				allModHaveConverged = centers.forall{ case (clusterID, uptMod) => metric.d(centers(clusterID), uptMod) <= epsilon }
				
				centersUpdated.foreach{ case (clusterID, mode) => centers(clusterID) = mode }
			}
			else println("Results will have no sense or cost O(n²) for the moment with another distance than Hamming, but we're working on it")
			cpt += 1
		}
		new KModesModel(centers, metric)
	}
}

object KModes
{
	def run[ID: Numeric, Obj <: Serializable](
		@(transient @param) sc: SparkContext,
		data: RDD[BinaryClusterizable[ID, Obj]],
		k: Int,
		epsilon: Double,
		maxIter: Int,
		metric: BinaryDistance[Seq[Int]],
		initializedCenters: mutable.HashMap[Int, Seq[Int]] = mutable.HashMap.empty[Int, Seq[Int]],
		persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY): KModesModel =
	{
		val kmodes = new KModes[ID, Obj](sc, data, k, epsilon, maxIter, metric, initializedCenters, persistanceLVL)
		val kModesModel = kmodes.run()
		kModesModel
	}
}