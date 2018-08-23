package clustering4ever.scala.clustering.kmeans

import org.apache.commons.math3.distribution.EnumeratedDistribution
import org.apache.commons.math3.util.Pair
import scala.collection.JavaConverters._
import scala.math.{min, max, pow}
import scala.collection.{immutable, mutable, GenSeq, parallel}
import scala.util.Random
import scala.reflect.ClassTag
import clustering4ever.clustering.datasetstype.DataSetsTypes
import clustering4ever.clustering.ClusteringAlgorithms
import clustering4ever.math.distances.{ContinuousDistance, Distance}
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.util.SumArrays
import clustering4ever.stats.Stats
import clustering4ever.scala.clusterizables.RealClusterizable
import clustering4ever.scala.vectorizables.RealVectorizable
import clustering4ever.scala.clustering.KppInitialization

/**
 * @author Beck Gaël
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : an Array with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param iterMax : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding ContinuousDistance distance function
 */
class KMeans[ID: Numeric, Obj, S <: Seq[Double] : ClassTag](
	val data: GenSeq[RealClusterizable[ID, Obj, S]],
	var k: Int,
	var epsilon: Double,
	var iterMax: Int,
	var metric: ContinuousDistance = new Euclidean(true),
	var initializedCenters: mutable.HashMap[Int, S] = mutable.HashMap.empty[Int, S]
) extends ClusteringAlgorithms[ID]
{
	/**
	 * Check if there are empty centers and remove them
	 */
	private[this] def removeEmptyClusters[T <: Seq[Double]](centers: mutable.HashMap[Int, T], kCentersBeforeUpdate: mutable.HashMap[Int, T], centersCardinality: mutable.HashMap[Int, Int]): Unit =
	{
		val emptyCenterIDs = centersCardinality.filter(_._2 == 0).map(_._1)
		if( ! emptyCenterIDs.isEmpty )
		{
			centers --= emptyCenterIDs
			kCentersBeforeUpdate --= emptyCenterIDs
		}
	}
	private[this] def obtainNearestCenterID[T <: Seq[Double]](v: T, centers: mutable.HashMap[Int, T]): ClusterID = centers.minBy{ case (clusterID, center) => metric.d(center, v) }._1

	private[this] def clusterizedAndSaveCenters[T <: Seq[Double]](realDS: parallel.ParSeq[T], centers: mutable.HashMap[Int, T]) =
	{
		// Allocation to nearest centroid
		val clusterized = realDS.map( v => (v, obtainNearestCenterID[T](v, centers)) )
		// Keep old position of centroids
		val kCentersBeforeUpdate = centers.clone
		(clusterized, kCentersBeforeUpdate)
	}
	/**
	 * Reinitialization of cardinalities
	 */
	private[this] def resetCentersCardinality(centersCardinality: mutable.HashMap[Int, Int]) = centersCardinality.foreach{ case (clusterID, _) => centersCardinality(clusterID) = 0 }
	/**
	 * Check if centers move enough
	 * @return true if every centers move less than epsilon
	 */
	private[this] def areCentersMovingEnough[T <: Seq[Double]](kCentersBeforeUpdate: mutable.HashMap[Int, T], centers: mutable.HashMap[Int, T]) = kCentersBeforeUpdate.forall{ case (clusterID, previousCenter) => metric.d(previousCenter, centers(clusterID)) <= epsilon }
	/**
	 * Run the K-Means
	 * Obtain model has to be casted into right one
	 */
	def run(): KMeansModel[_] = if( metric.isInstanceOf[Euclidean] ) runEuclideanDistance() else runCustomDistance()
	/**
	 * Run the K-Means with euclidean distance
	 */
	def runEuclideanDistance() =
	{
		val realDS = data.par.map(_.vector.toSeq)
		val dim = realDS.head.size		
		val centers: mutable.HashMap[Int, Seq[Double]] = if( initializedCenters.isEmpty ) KppInitialization.kmppInitialization[Double, Seq[Double], parallel.ParSeq[Seq[Double]]](realDS, k, metric) else initializedCenters.asInstanceOf[mutable.HashMap[Int, Seq[Double]]]
		val centersCardinality = centers.map{ case (clusterID, _) => (clusterID, 0) }

		var cpt = 0
		var allCentersHaveConverged = false
		while( cpt < iterMax && ! allCentersHaveConverged )
		{
			val (clusterized, kCentersBeforeUpdate) = clusterizedAndSaveCenters[Seq[Double]](realDS, centers)
			resetCentersCardinality(centersCardinality)
			// Update Center and Cardinalities
			clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregate) =>
			{
				centers(clusterID) = SumArrays.obtainMean(aggregate.map(_._1))
				centersCardinality(clusterID) += aggregate.size
			}}
			removeEmptyClusters[Seq[Double]](centers, kCentersBeforeUpdate, centersCardinality)
			allCentersHaveConverged = areCentersMovingEnough[Seq[Double]](kCentersBeforeUpdate, centers)
			cpt += 1
		}
		new KMeansModelSeq(centers, metric)
	}
	/**
	 * Run the K-Means with custom distance
	 */
	def runCustomDistance() =
	{
		val realDS = data.par.map(_.vector)
		val dim = realDS.head.size
		val centers: mutable.HashMap[Int, S] = if( initializedCenters.isEmpty ) KppInitialization.kmppInitialization[Double, S, parallel.ParSeq[S]](realDS, k, metric) else initializedCenters
		val centersCardinality = centers.map{ case (clusterID, _) => (clusterID, 0) }
		/**
		 * Compute the similarity matrix and extract point which is the closest from all other point according to its dissimilarity measure
		 */
		def obtainMedoid(gs: GenSeq[S]): S = gs.minBy( v1 => gs.map( v2 => metric.d(v1, v2) ).sum )

		var cpt = 0
		var allCentersHaveConverged = false
		while( cpt < iterMax && ! allCentersHaveConverged )
		{
			val (clusterized, kCentersBeforeUpdate) = clusterizedAndSaveCenters[S](realDS, centers)
			resetCentersCardinality(centersCardinality)
			// Update Center and Cardinalities
			clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregate) =>
			{
				val centroid = obtainMedoid(aggregate.map(_._1))
				centers(clusterID) = centroid
				centersCardinality(clusterID) += aggregate.size
			}}
			removeEmptyClusters[S](centers, kCentersBeforeUpdate, centersCardinality)
			allCentersHaveConverged = areCentersMovingEnough[S](kCentersBeforeUpdate, centers)
			cpt += 1
		}
		new KMeansModelCustom[S](centers, metric)
	}
}

object KMeans
{
	/**
	 * Run the K-Means
	 * Obtained model has to be casted into right one
	 */
	def run[ID: Numeric, Obj, S <: Seq[Double] : ClassTag](
		data: GenSeq[RealClusterizable[ID, Obj, S]],
		k: Int,
		epsilon: Double,
		iterMax: Int,
		metric: ContinuousDistance,
		initializedCenters: mutable.HashMap[Int, S] = mutable.HashMap.empty[Int, S]
		): KMeansModel[_] =
		(new KMeans[ID, Obj, S](data, k, epsilon, iterMax, metric, initializedCenters)).run()
	/**
	 * Run the K-Means with Euclidean distance
	 */
	def run[ID: Numeric, Obj](
		data: GenSeq[RealClusterizable[ID, Obj, Seq[Double]]],
		k: Int,
		epsilon: Double,
		iterMax: Int,
		metric: Euclidean
		): KMeansModelSeq =
	{
		val initializedCenters = mutable.HashMap.empty[Int, Seq[Double]]
		val kMeans = new KMeans[ID, Obj, Seq[Double]](data, k, epsilon, iterMax, metric, initializedCenters)
		val kmeansModel = kMeans.runEuclideanDistance()
		kmeansModel
	}
	/**
	 * Run the K-Means with a custom distance
	 */
	def run[ID: Numeric, Obj, S <: Seq[Double] : ClassTag](
		data: GenSeq[RealClusterizable[ID, Obj, S]],
		k: Int,
		epsilon: Double,
		iterMax: Int,
		metric: ContinuousDistance
	): KMeansModelCustom[S] =
	{
		val initializedCenters = mutable.HashMap.empty[Int, S]
		val kMeans = new KMeans[ID, Obj, S](data, k, epsilon, iterMax, metric, initializedCenters)
		val kmeansModel = kMeans.runCustomDistance()
		kmeansModel
	}
}