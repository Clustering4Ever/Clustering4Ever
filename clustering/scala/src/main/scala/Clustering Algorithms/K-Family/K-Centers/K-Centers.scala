package org.clustering4ever.scala.clustering.kcenters
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{mutable, GenSeq}
import scala.util.Random
import scala.reflect.ClassTag
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.stats.Stats
import org.clustering4ever.scala.clusterizables.Clusterizable
import org.clustering4ever.clustering.{ClusteringCommons, LocalClusteringAlgorithm}
import org.clustering4ever.util.ClusterBasicOperations
/**
 *
 */
abstract class KCommons[
	V : ClassTag,
	D <: Distance[V]
](
	k: Int,
	epsilon: Double,
	maxIterations: Int,
	metric: D,
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
) extends ClusteringCommons {
	/**
	 * Check if there are empty centers and remove them
	 */
	protected def removeEmptyClusters(centers: mutable.HashMap[Int, V], kCentersBeforeUpdate: mutable.HashMap[Int, V], centersCardinality: mutable.HashMap[Int, Int]): Unit = {
		val emptyCenterIDs = centersCardinality.collect{ case (clusterID, cardinality) if(cardinality == 0) => clusterID }
		if(!emptyCenterIDs.isEmpty) {
			centers --= emptyCenterIDs
			kCentersBeforeUpdate --= emptyCenterIDs
		}
	}
	/**
	 * Reinitialization of cardinalities
	 */
	protected def resetCentersCardinality(centersCardinality: mutable.HashMap[Int, Int]) = (0 until k).foreach( clusterID => centersCardinality(clusterID) = 0 )
	/**
	 *
	 */
	protected def obtainNearestCenterID(v: V, centers: mutable.HashMap[Int, V]): ClusterID = centers.minBy{ case (clusterID, center) => metric.d(center, v) }._1
	/**
	 * Check if centers move enough
	 * @return true if every centers move less than epsilon
	 */
	protected def areCentersMovingEnough(kCentersBeforeUpdate: mutable.HashMap[Int, V], centers: mutable.HashMap[Int, V], epsilon: Double) = kCentersBeforeUpdate.forall{ case (clusterID, previousCenter) => metric.d(previousCenter, centers(clusterID)) <= epsilon }
}
/**
 * The famous K-Centers using a user-defined dissmilarity measure.
 * @param data : preferably and ArrayBuffer or ParArray of Clusterizable
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param maxIterations : maximal number of iteration
 * @param metric : a defined dissimilarity measure
 */
class KCenters[V: ClassTag, D <: Distance[V]](
	k: Int,
	epsilon: Double,
	maxIterations: Int,
	metric: D,
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
) extends KCommons[V, D](k, epsilon, maxIterations, metric, initializedCenters) with LocalClusteringAlgorithm[V] {
	/**
	 * Run the K-Centers
	 */
	def run[
		ID: Numeric,
		O,
		Cz[ID, O, V] <: Clusterizable[ID, O, V, Cz[ID, O, V]],
		GS[X] <: GenSeq[X]
	](data: GS[Cz[ID, O, V]])(workingVector: Int = 0): KCentersModel[ID, O, V, Cz[ID, O, V], D] =	{
		/**
		 *
		 */
		val centers: mutable.HashMap[Int, V] = if(initializedCenters.isEmpty) KPPInitializer.kppInit[ID, O, V, Cz, D](data, metric, k)(workingVector) else initializedCenters
		/**
		 *
		 */
		val centersCardinality: mutable.HashMap[Int, Int] = centers.map{ case (clusterID, _) => (clusterID, 0) }
		/**
		 * KMeans heart in tailrec style
		 */
		@annotation.tailrec
		def go(cpt: Int, allCentersHaveConverged: Boolean): mutable.HashMap[Int, V] = {
			if(cpt < maxIterations && ! allCentersHaveConverged) {
				// Keep old position of centroids
				val kCentersBeforeUpdate = centers.clone
				// Compute centers and cardinality of each cluster
				data.groupBy( cz => obtainNearestCenterID(cz.vector(workingVector), centers) ).foreach{ case (clusterID, aggregate) =>
					centers(clusterID) = ClusterBasicOperations.obtainCenter(aggregate.map(_.vector(workingVector)), metric)
					centersCardinality(clusterID) = aggregate.size
				}
				removeEmptyClusters(centers, kCentersBeforeUpdate, centersCardinality)

				go(cpt + 1, areCentersMovingEnough(kCentersBeforeUpdate, centers, epsilon))
			}
			else {
				centers
			}
		}
		new KCentersModel[ID, O, V, Cz[ID, O, V], D](go(0, false), metric, workingVector)
	}
}
/**
 * The general KCenters helper
 */
object KCenters {
	/**
	 *
	 */
	def run[
		ID: Numeric,
		O,
		V: ClassTag,
		Cz[ID, O, V] <: Clusterizable[ID, O, V, Cz[ID, O, V]],
		D <: Distance[V]
	](
		data: GenSeq[Cz[ID, O, V]],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D,
		workingVector: Int = 0,
		initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
	): KCentersModel[ID, O, V, Cz[ID, O, V], D] = {
		val kCenter = new KCenters[V, D](k, epsilon, maxIterations, metric, initializedCenters)
		val kCentersModel = kCenter.run(data)(workingVector)
		kCentersModel
	}
}