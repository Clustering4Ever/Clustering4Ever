package clustering4ever.spark.clustering.kmeans

import scala.collection.{immutable, mutable}
import scala.util.Random
import scala.annotation.meta.param
import scala.reflect.ClassTag
import scala.math.{min, max}
import org.apache.spark.{SparkContext, HashPartitioner}
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.clustering.ClusteringAlgorithms
import clustering4ever.util.SumArrays
import clustering4ever.spark.clustering.accumulators.{CentroidsScalarAccumulator, CardinalitiesAccumulator}
import clustering4ever.clustering.DataSetsTypes
import clustering4ever.stats.Stats
import clustering4ever.scala.clusterizables.RealClusterizable
import clustering4ever.scala.vectorizables.RealVectorizable
import clustering4ever.spark.clustering.KCommonsSpark

/**
 * @author Beck Gaël
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : an Array with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param iterMax : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding ContinuousDistance distance function
 **/
class KMeans[ID: Numeric, Obj](
	@transient val sc: SparkContext,
	data: RDD[RealClusterizable[ID, Obj, Seq[Double]]],
	k: Int,
	var epsilon: Double,
	var maxIter: Int,
	metric: Euclidean[Seq[Double]],
	initializedCenters: mutable.HashMap[Int, Seq[Double]] = mutable.HashMap.empty[Int, Seq[Double]],
	persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY
) extends KCommonsSpark[ID, Double, Seq[Double], Euclidean[Seq[Double]], RealClusterizable[ID, Obj, Seq[Double]]](data, metric, k, initializedCenters, persistanceLVL)
{
	def run(): KMeansModel =
	{		
		def initializationCenters() =
		{
			def obtainMinAndMax(data: RDD[Seq[Double]]) =
			{
				val vectorRange = (0 until dim).toVector

				val (minValues, maxValues) = data.map( v =>
				{
					val vector = v.toVector
					(vector, vector)
				}).reduce( (minMaxa, minMaxb) => vectorRange.map( i => Stats.obtainIthMinMax(i, minMaxa, minMaxb) ).unzip )
				(minValues, maxValues)
			}

			val (minv, maxv) = obtainMinAndMax(vectorizedDataset)

			val ranges = Seq(minv.zip(maxv):_*).map{ case (min, max) => (max - min, min) }
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
			val info = vectorizedDataset.map( v => (obtainNearestCenterID(v, centers), (1L, v)) ).reduceByKey{ case ((sum1, v1), (sum2, v2)) => (sum1 + sum2, SumArrays.sumArraysNumericsGen[Double, Seq[Double]](v1, v2)) }
				.map{ case (clusterID, (cardinality, preMean)) => (clusterID, preMean.map(_ / cardinality), cardinality) }.collect

			info.foreach{ case (clusterID, mean, cardinality) =>
			{
				centersUpdated(clusterID) = mean
				clustersCardinality(clusterID) = cardinality
			}}

			allModHaveConverged = centersUpdated.forall{ case (clusterID, uptMod) => metric.d(centers(clusterID), uptMod) <= epsilon }
			centersUpdated.foreach{ case (clusterID, mode) => centers(clusterID) = mode }	
			cpt += 1
		}
		new KMeansModel(centers, metric)
	}
}


object KMeans extends DataSetsTypes[Long]
{
	def run[ID: Numeric, Obj](
		@(transient @param) sc: SparkContext,
		data: RDD[RealClusterizable[ID, Obj, Seq[Double]]],
		k: Int,
		epsilon: Double,
		maxIter: Int,
		metric: Euclidean[Seq[Double]],
		initializedCenters: mutable.HashMap[Int, Seq[Double]] = mutable.HashMap.empty[Int, Seq[Double]],
		persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY
	): KMeansModel =
	{
		val kmeans = new KMeans[ID, Obj](sc, data, k, epsilon, maxIter, metric, initializedCenters, persistanceLVL)
		val kmeansModel = kmeans.run()
		kmeansModel
	}
}