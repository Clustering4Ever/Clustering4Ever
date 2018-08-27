package clustering4ever.scala.clustering.kmeans

import org.apache.commons.math3.distribution.EnumeratedDistribution
import org.apache.commons.math3.util.Pair
import scala.collection.JavaConverters._
import scala.math.{min, max, pow}
import scala.collection.{immutable, mutable, GenSeq}
import scala.util.Random
import scala.reflect.ClassTag
import clustering4ever.math.distances.{ContinuousDistance, Distance}
import clustering4ever.math.distances.scalar.{Euclidean, FastEuclidean}
import clustering4ever.util.SumArrays
import clustering4ever.stats.Stats
import clustering4ever.scala.clusterizables.RealClusterizable
import clustering4ever.scala.vectorizables.RealVectorizable
import clustering4ever.scala.clustering.KCommonsVectors

/**
 * @author Beck Gaël
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : an Array with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param iterMax : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding ContinuousDistance distance function
 */
class KMeans[ID: Numeric, Obj, V <: Seq[Double] : ClassTag, Rc <: RealClusterizable[ID, Obj, V], D <: ContinuousDistance[V]](
	data: GenSeq[Rc],
	k: Int,
	epsilon: Double,
	iterMax: Int,
	metric: D = new FastEuclidean(squareRoot = true),
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
) extends KCommonsVectors[ID, Double, V, D, Rc](data, metric, k, initializedCenters)
{
	/**
	 * Run the K-Means
	 */
	def run(): KMeansModel[ID, Obj, V, Rc, D] =
	{
		var cpt = 0
		var allCentersHaveConverged = false
		/**
		 * Run the K-Means with Euclidean metric
		 */
		def runEuclidean(): KMeansModel[ID, Obj, V, Rc, D] =
		{
			while( cpt < iterMax && ! allCentersHaveConverged )
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
			while( cpt < iterMax && ! allCentersHaveConverged )
			{
				val (clusterized, kCentersBeforeUpdate) = clusterizedAndSaveCentersWithResetingCentersCardinalities(vectorizedDataset, centers, centersCardinality)
				updateCentersAndCardinalitiesCustom(clusterized, centers, centersCardinality)
				allCentersHaveConverged = removeEmptyClustersAndCheckIfallCentersHaveConverged(centers, kCentersBeforeUpdate, centersCardinality, epsilon)
				cpt += 1
			}
			new KMeansModel[ID, Obj, V, Rc, D](centers, metric)
		}

		if( metric.isInstanceOf[Euclidean[V]] ) runEuclidean() else runCustom()
	}
}

object KMeans
{
	/**
	 * Run the K-Means with Euclidean distance
	 */
	def run[ID: Numeric, Obj, V <: Seq[Double] : ClassTag, Rc <: RealClusterizable[ID, Obj, V], D <: ContinuousDistance[V]](
		data: GenSeq[Rc],
		k: Int,
		epsilon: Double,
		iterMax: Int,
		metric: D = new FastEuclidean(squareRoot = true)
		): KMeansModel[ID, Obj, V, Rc, D] =
	{
		val initializedCenters = mutable.HashMap.empty[Int, V]
		val kMeans = new KMeans[ID, Obj, V, Rc, D](data, k, epsilon, iterMax, metric, initializedCenters)
		val kmeansModel = kMeans.run()
		kmeansModel
	}
}
