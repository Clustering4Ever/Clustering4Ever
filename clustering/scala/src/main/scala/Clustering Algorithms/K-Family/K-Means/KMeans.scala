package clustering4ever.scala.clustering.kmeans

import org.apache.commons.math3.distribution.EnumeratedDistribution
import org.apache.commons.math3.util.Pair
import scala.collection.JavaConverters._
import scala.math.{min, max, pow}
import scala.collection.{immutable, mutable, GenSeq}
import scala.util.Random
import scala.reflect.ClassTag
import clustering4ever.math.distances.{ContinuousDistance, Distance}
import clustering4ever.math.distances.scalar.{Euclidean, FastEuclideanLowD, FastEuclideanHighD}
import clustering4ever.util.SumArrays
import clustering4ever.stats.Stats
import clustering4ever.scala.clusterizables.RealClusterizable
import clustering4ever.scala.vectorizables.RealVectorizable
import clustering4ever.scala.clustering.KCommonsVectors
import clustering4ever.util.CommonTypes

/**
 * @author Beck Gaël
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : an Array with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param maxIterations : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding ContinuousDistance distance function
 */
class KMeans[ID: Numeric, Obj, V <: GenSeq[Double] : ClassTag, Rc <: RealClusterizable[ID, Obj, V], D <: ContinuousDistance[V]](
	data: GenSeq[Rc],
	k: Int,
	epsilon: Double,
	maxIterations: Int,
	metric: D = new FastEuclideanLowD(squareRoot = true),
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
) extends KCommonsVectors[ID, Double, V, D, Rc](data, metric, k, initializedCenters)
{
	/**
	 * Run the K-Means
	 */
	def run(): KMeansModel[ID, Obj, V, Rc, D] =
	{
		/**
		 * Run the K-Means with Euclidean metric
		 */
		def runEuclidean(): KMeansModel[ID, Obj, V, Rc, D] =
		{
			var cpt = 0
			var allCentersHaveConverged = false
			while( cpt < maxIterations && ! allCentersHaveConverged )
			{
				val (clusterized, kCentersBeforeUpdate) = clusterizedAndSaveCentersWithResetingCentersCardinalities(vectorizedDataset, centers, centersCardinality)
				clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregate) =>
				{
					centers(clusterID) = SumArrays.obtainMean(aggregate.map(_._1)).asInstanceOf[V]
					centersCardinality(clusterID) += aggregate.size
				}}
				allCentersHaveConverged = removeEmptyClustersAndCheckIfallCentersHaveConverged(centers, kCentersBeforeUpdate, centersCardinality, epsilon)
				cpt += 1
			}
			new KMeansModel[ID, Obj, V, Rc, D](centers, metric)
		}
		/**
		 * Run the K-Means with Custom metric
		 */
		def runCustom(): KMeansModel[ID, Obj, V, Rc, D] =
		{
			runKAlgorithmWithCustomMetric(maxIterations, epsilon)
			new KMeansModel[ID, Obj, V, Rc, D](centers, metric)
		}

		if( metric.isInstanceOf[Euclidean[V]] ) runEuclidean() else runCustom()
	}
}

object KMeans extends CommonTypes
{
	/**
	 * Run the K-Means with any continuous distance
	 */
	def run[ID: Numeric, Obj, V <: GenSeq[Double] : ClassTag, Rc <: RealClusterizable[ID, Obj, V], D <: ContinuousDistance[V]](
		data: GenSeq[Rc],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D
		): KMeansModel[ID, Obj, V, Rc, D] =
	{
		val initializedCenters = mutable.HashMap.empty[Int, V]
		val kMeans = new KMeans[ID, Obj, V, Rc, D](data, k, epsilon, maxIterations, metric, initializedCenters)
		val kmeansModel = kMeans.run()
		kmeansModel
	}
	/**
	 * Run the K-Means with Euclidean distance in a fast way on low/medium dimensional datasets
	 */
	def runLD[Rc <: RealClusterizable[Long, MB[Double], MB[Double]]](
		data: GenSeq[Rc],
		k: Int,
		epsilon: Double,
		maxIterations: Int
		): KMeansModel[Long, MB[Double], MB[Double], Rc, FastEuclideanLowD] =
	{
		val metric = new FastEuclideanLowD(true)
		val initializedCenters = mutable.HashMap.empty[Int, MB[Double]]
		val kMeans = new KMeans[Long, MB[Double], MB[Double], Rc, FastEuclideanLowD](data, k, epsilon, maxIterations, metric, initializedCenters)
		val kmeansModel = kMeans.run()
		kmeansModel
	}

	/**
	 * Run the K-Means with Euclidean distance in a fast on very very high dimensional datasets
	 */
	def runHD[Rc <: RealClusterizable[Long, PA[Double], PA[Double]]](
		data: GenSeq[Rc],
		k: Int,
		epsilon: Double,
		maxIterations: Int
		): KMeansModel[Long, PA[Double], PA[Double], Rc, FastEuclideanHighD] =
	{
		val metric = new FastEuclideanHighD(true)
		val initializedCenters = mutable.HashMap.empty[Int, PA[Double]]
		val kMeans = new KMeans[Long, PA[Double], PA[Double], Rc, FastEuclideanHighD](data, k, epsilon, maxIterations, metric, initializedCenters)
		val kmeansModel = kMeans.run()
		kmeansModel
	}
}
