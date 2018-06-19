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
import _root_.clustering4ever.stats.Stats

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
	data: RDD[immutable.Vector[Double]],
	var k: Int,
	var epsilon: Double,
	var maxIter: Int,
	var metric: ContinuousDistances
) extends ClusteringAlgorithms[Long, immutable.Vector[Double]]
{
	type CentersMap = mutable.HashMap[Int, Vector]

	def obtainNearestModID(v: Vector, kModesCenters: CentersMap): Int =
	{
		kModesCenters.toArray.map{ case(clusterID, mod) => (clusterID, metric.d(mod, v)) }.minBy(_._2)._1
	}

	def run(): KMeansModel =
	{
		val dim = data.first.size
		
		def initializationCenters() =
		{
			def obtainMinAndMax(data: RDD[Vector]) =
			{
				val dim = data.first.size
				val vectorRange = (0 until dim).toVector

				val (minValues, maxValues) = data.map( v => (v, v) ).reduce( (minMaxa, minMaxb) =>
				{
					val minAndMax = for( i <- vectorRange ) yield (Stats.obtainIthMinMax(i, minMaxa, minMaxb))
					minAndMax.unzip
				})
				(minValues, maxValues)
			}

			val (minv, maxv) = obtainMinAndMax(data)

			val ranges = minv.zip(maxv).map{ case (min, max) => (max - min, min) }
			val centers = mutable.HashMap((0 until k).map( clusterID => (clusterID, ranges.map{ case (range, min) => Random.nextDouble * range + min }) ):_*)
			centers
		}
		
		val centers = initializationCenters()
		val centersUpdated = centers.clone
		val clustersCardinality = centers.map{ case (clusterID, _) => (clusterID, 0L) }
		var cpt = 0
		var allModHaveConverged = false
		while( cpt < maxIter && ! allModHaveConverged )
		{
			if( metric.isInstanceOf[Euclidean] )
			{
				val info = data.map( v => (obtainNearestModID(v, centers), (1L, v)) ).reduceByKey{ case ((sum1, v1), (sum2, v2)) => (sum1 + sum2, SumArrays.sumArraysNumerics(v1, v2)) }.map{ case (clusterID, (cardinality, preMean)) => (clusterID, preMean.map(_ / cardinality), cardinality) }.collect

				info.foreach{ case (clusterID, mean, cardinality) =>
				{
					centersUpdated(clusterID) = mean
					clustersCardinality(clusterID) = cardinality
				}}

				allModHaveConverged = centersUpdated.forall{ case (clusterID, uptMod) => metric.d(centers(clusterID), uptMod) <= epsilon }
				centersUpdated.foreach{ case (clusterID, mod) => centers(clusterID) = mod }	
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


object KMeans extends DataSetsTypes[Long, immutable.Vector[Double]]
{
	def run(@(transient @param) sc: SparkContext, data: RDD[Vector], k: Int, epsilon: Double, maxIter: Int, metric: ContinuousDistances): KMeansModel =
	{
		val kmeans = new KMeans(sc, data, k, epsilon, maxIter, metric)
		val kmeansModel = kmeans.run()
		kmeansModel
	}
}