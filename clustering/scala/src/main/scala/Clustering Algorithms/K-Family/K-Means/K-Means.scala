package clustering4ever.scala.clustering.kmeans
/**
 * @author Beck Gaël
 */
import scala.collection.{immutable, mutable, GenSeq}
import scala.util.Random
import scala.reflect.ClassTag
import clustering4ever.math.distances.{ContinuousDistance, Distance}
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.util.ClusterBasicOperations
import clustering4ever.stats.Stats
import clustering4ever.scala.clusterizables.RealClusterizable
import clustering4ever.scala.vectorizables.RealVectorizable
import clustering4ever.scala.clustering.KCommonsVectors
import clustering4ever.util.CommonTypes
import scala.language.higherKinds
/**
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : preferably and ArrayBuffer or ParArray of Clusterizable
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param maxIterations : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding ContinuousDistance distance function
 */
class KMeans[
	ID: Numeric,
	O,
	V <: Seq[Double] : ClassTag,
	Cz[ID, O, V <: Seq[Double]] <: RealClusterizable[ID, O, V, Cz[ID, O, V]],
	D <: ContinuousDistance[V]
](
	data: GenSeq[Cz[ID, O, V]],
	k: Int,
	epsilon: Double,
	maxIterations: Int,
	metric: D = new Euclidean[V](squareRoot = true),
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
) extends KCommonsVectors[ID, Double, V, D, Cz[ID, O, V]](data, metric, k, initializedCenters) {
	/**
	 * Run the K-Means
	 */
	def run(): KMeansModel[ID, O, V, Cz[ID, O, V], D] =
	{
		/**
		 * Run the K-Means with Euclidean metric
		 */
		def runEuclidean(): KMeansModel[ID, O, V, Cz[ID, O, V], D] =
		{
			var cpt = 0
			var allCentersHaveConverged = false
			while( cpt < maxIterations && ! allCentersHaveConverged ) {
				val (clusterized, kCentersBeforeUpdate) = clusterizedAndSaveCentersWithResetingCentersCardinalities(centers, centersCardinality)
				clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregate) =>
					centers(clusterID) = ClusterBasicOperations.obtainMean[V](aggregate.map(_._1))
					centersCardinality(clusterID) += aggregate.size
				}
				allCentersHaveConverged = removeEmptyClustersAndCheckIfallCentersHaveConverged(centers, kCentersBeforeUpdate, centersCardinality, epsilon)
				cpt += 1
			}
			new KMeansModel[ID, O, V, Cz[ID, O, V], D](centers, metric)
		}
		/**
		 * Run the K-Means with Custom metric
		 */
		def runCustom(): KMeansModel[ID, O, V, Cz[ID, O, V], D] =
		{
			runKAlgorithmWithCustomMetric(maxIterations, epsilon)
			new KMeansModel[ID, O, V, Cz[ID, O, V], D](centers, metric)
		}

		if( metric.isInstanceOf[Euclidean[V]] ) runEuclidean() else runCustom()
	}
}

object KMeans {
	/**
	 * Run the K-Means with any continuous distance
	 */
	def run[
		ID: Numeric,
		O,
		V <: Seq[Double] : ClassTag,
		Cz[ID, O, V <: Seq[Double]] <: RealClusterizable[ID, O, V, Cz[ID, O, V]],
		D <: ContinuousDistance[V]
	](
		data: GenSeq[Cz[ID, O, V]],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D,
		initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
		): KMeansModel[ID, O, V, Cz[ID, O, V], D] = {
		val kMeans = new KMeans[ID, O, V, Cz, D](data, k, epsilon, maxIterations, metric, initializedCenters)
		val kmeansModel = kMeans.run()
		kmeansModel
	}
}
