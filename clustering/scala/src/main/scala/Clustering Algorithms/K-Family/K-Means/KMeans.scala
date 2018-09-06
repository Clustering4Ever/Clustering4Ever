package clustering4ever.scala.clustering.kmeans

import org.apache.commons.math3.distribution.EnumeratedDistribution
import org.apache.commons.math3.util.Pair
import scala.collection.JavaConverters._
import scala.math.{min, max, pow}
import scala.collection.{immutable, mutable, GenSeq}
import scala.util.Random
import scala.reflect.ClassTag
import clustering4ever.math.distances.{ContinuousDistance, Distance}
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.util.SumVectors
import clustering4ever.stats.Stats
import clustering4ever.scala.clusterizables.RealClusterizable
import clustering4ever.scala.vectorizables.RealVectorizable
import clustering4ever.scala.clustering.KCommonsVectors
import clustering4ever.util.CommonTypes

/**
 * @author Beck Gaël
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : preferably and ArrayBuffer or ParArray of Clusterizable
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param maxIterations : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding ContinuousDistance distance function
 */
class KMeans[ID: Numeric, Obj, S <: Seq[Double] : ClassTag, Rc <: RealClusterizable[ID, Obj, S], D <: ContinuousDistance[S]](
	data: GenSeq[Rc],
	k: Int,
	epsilon: Double,
	maxIterations: Int,
	metric: D = new Euclidean[S](squareRoot = true),
	initializedCenters: mutable.HashMap[Int, S] = mutable.HashMap.empty[Int, S]
) extends KCommonsVectors[ID, Double, S, D, Rc](data, metric, k, initializedCenters)
{
	/**
	 * Run the K-Means
	 */
	def run(): KMeansModel[ID, Obj, S, Rc, D] =
	{
		/**
		 * Run the K-Means with Euclidean metric
		 */
		def runEuclidean(): KMeansModel[ID, Obj, S, Rc, D] =
		{
			var cpt = 0
			var allCentersHaveConverged = false
			while( cpt < maxIterations && ! allCentersHaveConverged )
			{
				val (clusterized, kCentersBeforeUpdate) = clusterizedAndSaveCentersWithResetingCentersCardinalities(centers, centersCardinality)
				clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregate) =>
				{
					centers(clusterID) = SumVectors.obtainMean[S](aggregate.map(_._1))
					centersCardinality(clusterID) += aggregate.size
				}}
				allCentersHaveConverged = removeEmptyClustersAndCheckIfallCentersHaveConverged(centers, kCentersBeforeUpdate, centersCardinality, epsilon)
				cpt += 1
			}
			new KMeansModel[ID, Obj, S, Rc, D](centers, metric)
		}
		/**
		 * Run the K-Means with Custom metric
		 */
		def runCustom(): KMeansModel[ID, Obj, S, Rc, D] =
		{
			runKAlgorithmWithCustomMetric(maxIterations, epsilon)
			new KMeansModel[ID, Obj, S, Rc, D](centers, metric)
		}

		if( metric.isInstanceOf[Euclidean[S]] ) runEuclidean() else runCustom()
	}
}

object KMeans
{
	/**
	 * Run the K-Means with any continuous distance
	 */
	def run[ID: Numeric, Obj, S <: Seq[Double] : ClassTag, Rc <: RealClusterizable[ID, Obj, S], D <: ContinuousDistance[S]](
		data: GenSeq[Rc],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D,
		initializedCenters: mutable.HashMap[Int, S] = mutable.HashMap.empty[Int, S]
		): KMeansModel[ID, Obj, S, Rc, D] =
	{
		val kMeans = new KMeans[ID, Obj, S, Rc, D](data, k, epsilon, maxIterations, metric, initializedCenters)
		val kmeansModel = kMeans.run()
		kmeansModel
	}
}
