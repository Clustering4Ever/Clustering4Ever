package clustering4ever.scala.clustering.kmeans

import org.apache.commons.math3.distribution.EnumeratedDistribution
import org.apache.commons.math3.util.Pair
import scala.collection.JavaConverters._
import scala.math.{min, max, pow}
import scala.collection.{immutable, mutable, GenSeq, parallel}
import scala.util.Random
import scala.reflect.ClassTag
import clustering4ever.math.distances.{ContinuousDistance, Distance}
import clustering4ever.math.distances.scalar.{Euclidean, ClassicEuclidean}
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
class KMeans[ID: Numeric, Obj](
	data: GenSeq[RealClusterizable[ID, Obj, Seq[Double]]],
	k: Int,
	var epsilon: Double,
	var iterMax: Int,
	metric: Euclidean[Seq[Double]] = new ClassicEuclidean,
	initializedCenters: mutable.HashMap[Int, Seq[Double]] = mutable.HashMap.empty[Int, Seq[Double]]
) extends KCommonsVectors[ID, Double, Seq[Double], Euclidean[Seq[Double]], RealClusterizable[ID, Obj, Seq[Double]]](data, metric, k, initializedCenters)
{
	/**
	 * Run the K-Means with Euclidean metric
	 */
	def run(): KMeansModelSeq[ID, Obj] =
	{
		var cpt = 0
		var allCentersHaveConverged = false
		while( cpt < iterMax && ! allCentersHaveConverged )
		{
			val (clusterized, kCentersBeforeUpdate) = clusterizedAndSaveCenters(vectorizedDataset, centers)
			resetCentersCardinality(centersCardinality)
			// Update Center and Cardinalities
			clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregate) =>
			{
				centers(clusterID) = SumArrays.obtainMean(aggregate.map(_._1))
				centersCardinality(clusterID) += aggregate.size
			}}
			removeEmptyClusters(centers, kCentersBeforeUpdate, centersCardinality)
			allCentersHaveConverged = areCentersMovingEnough(kCentersBeforeUpdate, centers, epsilon)
			cpt += 1
		}
		new KMeansModelSeq[ID, Obj](centers, metric)
	}
}

object KMeans
{
	/**
	 * Run the K-Means with Euclidean distance
	 */
	def run[ID: Numeric, Obj](
		data: GenSeq[RealClusterizable[ID, Obj, Seq[Double]]],
		k: Int,
		epsilon: Double,
		iterMax: Int,
		metric: Euclidean[Seq[Double]]
		): KMeansModelSeq[ID, Obj] =
	{
		val initializedCenters = mutable.HashMap.empty[Int, Seq[Double]]
		val kMeans = new KMeans[ID, Obj](data, k, epsilon, iterMax, metric, initializedCenters)
		val kmeansModel = kMeans.run()
		kmeansModel
	}
}

class KMeansCustom[ID: Numeric, Obj, S <: Seq[Double] : ClassTag](
	data: GenSeq[RealClusterizable[ID, Obj, S]],
	k: Int,
	var epsilon: Double,
	var iterMax: Int,
	metric: ContinuousDistance[S],
	initializedCenters: mutable.HashMap[Int, S] = mutable.HashMap.empty[Int, S]
) extends KCommonsVectors[ID, Double, S, ContinuousDistance[S], RealClusterizable[ID, Obj, S]](data, metric, k, initializedCenters)
{
	/**
	 * Run the K-Means with Custom metric
	 */
	def run(): KMeansModelCustom[ID, S, Obj] =
	{
		var cpt = 0
		var allCentersHaveConverged = false
		while( cpt < iterMax && ! allCentersHaveConverged )
		{
			val (clusterized, kCentersBeforeUpdate) = clusterizedAndSaveCenters(vectorizedDataset, centers)
			resetCentersCardinality(centersCardinality)
			updateCentersAndCardinalitiesCustom(clusterized, centers, centersCardinality)
			removeEmptyClusters(centers, kCentersBeforeUpdate, centersCardinality)
			allCentersHaveConverged = areCentersMovingEnough(kCentersBeforeUpdate, centers, epsilon)
			cpt += 1
		}
		new KMeansModelCustom[ID, S, Obj](centers, metric)
	}
}

object KMeansCustom
{
	/**
	 * Run the K-Means with a custom distance
	 */
	def run[ID: Numeric, Obj, S <: Seq[Double] : ClassTag](
		data: GenSeq[RealClusterizable[ID, Obj, S]],
		k: Int,
		epsilon: Double,
		iterMax: Int,
		metric: ContinuousDistance[S]
	): KMeansModelCustom[ID, S, Obj] =
	{
		val initializedCenters = mutable.HashMap.empty[Int, S]
		val kMeans = new KMeansCustom[ID, Obj, S](data, k, epsilon, iterMax, metric, initializedCenters)
		val kmeansModel = kMeans.run()
		kmeansModel
	}
}