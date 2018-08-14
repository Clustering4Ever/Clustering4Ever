package clustering4ever.spark.clustering.kmeans

import scala.collection.{immutable, mutable}
import scala.util.Random
import scala.annotation.meta.param
import scala.reflect.ClassTag
import scala.math.{min, max}
import org.apache.spark.{SparkContext, HashPartitioner}
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import clustering4ever.math.distances.ContinuousDistances
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.clustering.ClusteringAlgorithms
import clustering4ever.util.SumArrays
import clustering4ever.spark.clustering.accumulators.{CentroidsScalarAccumulator, CardinalitiesAccumulator}
import clustering4ever.clustering.datasetstype.DataSetsTypes
import clustering4ever.stats.Stats
import clustering4ever.scala.clusterizables.RealClusterizable
import clustering4ever.scala.vectorizables.RealVectorizable

/**
 * @author Beck Gaël
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : an Array with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param iterMax : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding ContinuousDistances distance function
 **/
class KMeans[ID: Numeric, Obj <: Serializable](
	@transient val sc: SparkContext,
	data: RDD[RealClusterizable[ID, Obj]],
	var k: Int,
	var epsilon: Double,
	var maxIter: Int,
	var metric: ContinuousDistances,
	var initializedCenters: mutable.HashMap[Int, immutable.Seq[Double]] = mutable.HashMap.empty[Int, immutable.Seq[Double]],
	var persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY
) extends ClusteringAlgorithms[Long, immutable.Seq[Double]]
{
	type CentersMap = mutable.HashMap[Int, immutable.Seq[Double]]

	val realDS = data.map(_.vector).persist(persistanceLVL)

	def obtainNearestModID(v: immutable.Seq[Double], kMeansCenters: CentersMap): Int =
	{
		kMeansCenters.minBy{ case(clusterID, mode) => metric.d(mode, v) }._1
	}

	def run(): KMeansModel =
	{
		val dim = realDS.first.size
		
		def initializationCenters() =
		{
			def obtainMinAndMax(data: RDD[Vector]) =
			{
				val vectorRange = (0 until dim).toVector

				val (minValues, maxValues) = data.map( v =>
				{
					val vector = v.toVector
					(vector, vector)
				}).reduce( (minMaxa, minMaxb) =>
				{
					val minAndMax = for( i <- vectorRange ) yield Stats.obtainIthMinMax(i, minMaxa, minMaxb)
					minAndMax.unzip
				})
				(minValues, maxValues)
			}

			val (minv, maxv) = obtainMinAndMax(realDS)

			val ranges = minv.zip(maxv).toSeq.map{ case (min, max) => (max - min, min) }
			val centers = mutable.HashMap((0 until k).map( clusterID => (clusterID, ranges.map{ case (range, min) => Random.nextDouble * range + min }) ):_*)
			centers
		}
		
		val centers = if( initializedCenters.isEmpty ) initializationCenters() else initializedCenters
		val centersUpdated = centers.clone
		val clustersCardinality = centers.map{ case (clusterID, _) => (clusterID, 0L) }
		var cpt = 0
		var allModHaveConverged = false
		while( cpt < maxIter && ! allModHaveConverged )
		{
			if( metric.isInstanceOf[Euclidean] )
			{
				val info = realDS.map( v => (obtainNearestModID(v, centers), (1L, v)) ).reduceByKey{ case ((sum1, v1), (sum2, v2)) => (sum1 + sum2, SumArrays.sumArraysNumerics[Double](v1, v2)) }.map{ case (clusterID, (cardinality, preMean)) => (clusterID, preMean.map(_ / cardinality), cardinality) }.collect

				info.foreach{ case (clusterID, mean, cardinality) =>
				{
					centersUpdated(clusterID) = mean
					clustersCardinality(clusterID) = cardinality
				}}

				allModHaveConverged = centersUpdated.forall{ case (clusterID, uptMod) => metric.d(centers(clusterID), uptMod) <= epsilon }
				centersUpdated.foreach{ case (clusterID, mode) => centers(clusterID) = mode }	
			}
			else
			{
				println("Results will have no sense or cost O(n²) for the moment with another distance than Euclidean, but we're working on it")
			}
			cpt += 1
		}
		new KMeansModel(centers, metric)
	}
}


object KMeans extends DataSetsTypes[Long, immutable.Seq[Double]]
{
	def run[ID: Numeric, Obj <: Serializable](
		@(transient @param) sc: SparkContext,
		data: RDD[RealClusterizable[ID, Obj]],
		k: Int,
		epsilon: Double,
		maxIter: Int,
		metric: ContinuousDistances,
		initializedCenters: mutable.HashMap[Int, immutable.Seq[Double]] = mutable.HashMap.empty[Int, immutable.Seq[Double]],
		persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY
	): KMeansModel =
	{
		val kmeans = new KMeans[ID, Obj](sc, data, k, epsilon, maxIter, metric, initializedCenters, persistanceLVL)
		val kmeansModel = kmeans.run()
		kmeansModel
	}
}