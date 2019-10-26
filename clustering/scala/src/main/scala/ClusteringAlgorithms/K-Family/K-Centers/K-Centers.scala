package org.clustering4ever.clustering.kcenters.scala
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{immutable, mutable, GenSeq}
import scala.util.Random
import scala.reflect.ClassTag
import shapeless._
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.stats.Stats
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.clustering.{ClusteringSharedTypes, ClusteringAlgorithmLocal}
import org.clustering4ever.util.ClusterBasicOperations
import org.clustering4ever.clustering.ClusteringModel
import org.clustering4ever.vectors.GVector
import org.clustering4ever.clustering.arguments.{MinShiftArgs, MaxIterationsArgs, KArgs, MetricArgs}
/**
 *
 */
trait KCommonsArgs[V <: GVector[V], D <: Distance[V]] extends MinShiftArgs with MaxIterationsArgs with KArgs with MetricArgs[V, D] {
	/**
	 * The number of cluster seeked
	 */
	val k: Int
	/**
	 * The minimal shift under which centers are considered stationary
	 */
	val minShift: Double
}
/**
 *
 */
trait KCommons[V <: GVector[V], D <: Distance[V]] extends KCommonsArgs[V, D] with ClusteringSharedTypes {
	/**
	 * A custom centers initialiazation indexed from 0 until k - 1, empty HashMap by default which result usage of K++ initialization
	 */
	val customCenters: immutable.HashMap[Int, V]
	/**
	 * Require centers indices from 0 until k
	 */
	require(customCenters.map(_._1).toSeq.sorted.zip(0 until customCenters.size).map{ case (a, b) => a - b }.sum == 0, println("Please aligned your centers with indices from 0 until k"))
	/**
	 *
	 */
	private[kcenters] final def obtainNearestCenterID(v: V, centers: mutable.ArrayBuffer[(Int, V)], metric: D): ClusterID = {
		centers.minBy{ case (clusterID, center) => metric.d(center, v) }._1
	}
	/**
	 * Check if centers move enough
	 * @return true if every centers move less than minShift
	 */
	private[kcenters] final def areCentersNotMovingEnough(updatedCenters: mutable.ArrayBuffer[(Int, V)], previousCenters: mutable.ArrayBuffer[(Int, V)], minShift: Double, metric: D) = {
		updatedCenters.forall{ case (clusterID, previousCenter) => metric.d(previousCenter, previousCenters(clusterID)._2) <= minShift }
	}
}
/**
 * The famous K-Centers using a user-defined dissmilarity measure.
 * @param data preferably and ArrayBuffer or ParArray of Clusterizable
 * @param k number of clusters
 * @param minShift minimal threshold under which we consider a centroid has converged
 * @param maxIterations maximal number of iteration
 * @param metric a defined dissimilarity measure
 */
trait KCentersAncestor[V <: GVector[V], D <: Distance[V], CM <: KCentersModelAncestor[V, D]] extends KCommons[V, D] with ClusteringAlgorithmLocal[V, CM] {
	/**
	 * Run the K-Centers
	 */
	private[kcenters] final def obtainCenters[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): immutable.HashMap[Int, V] = {
		/**
		 *
		 */
		val unSortedCenters: mutable.ArrayBuffer[(Int, V)] = if(customCenters.isEmpty) mutable.ArrayBuffer(KPPInitializer.kppInit(data, metric, k).toSeq:_*) else mutable.ArrayBuffer(customCenters.toSeq:_*)
		val centers = unSortedCenters.sortBy(_._1)
		/**
		 * KCenters heart in tailrec style
		 */
		@annotation.tailrec
		def go(cpt: Int, haveAllCentersConverged: Boolean, centers: mutable.ArrayBuffer[(Int, V)]): mutable.ArrayBuffer[(Int, V)] = {
			val preUpdatedCenters = mutable.ArrayBuffer(
				data.groupBy( cz => obtainNearestCenterID(cz.v, centers, metric) )
					.map{ case (clusterID, aggregate) =>
						(clusterID, ClusterBasicOperations.obtainCenter(aggregate.map(_.v), metric))
					}.seq.toSeq
			:_*).sortBy(_._1)
			val alignedOldCenters = preUpdatedCenters.map{ case (oldClusterID, _) => centers(oldClusterID) }
			val updatedCenters = preUpdatedCenters.zipWithIndex.map{ case ((oldClusterID, center), newClusterID) => (newClusterID, center) }
			val shiftingEnough = areCentersNotMovingEnough(updatedCenters, alignedOldCenters, minShift, metric)
			if(cpt < maxIterations && !shiftingEnough) {
				go(cpt + 1, shiftingEnough, updatedCenters)
			}
			else {
				updatedCenters
			}
		}
		immutable.HashMap(go(0, false, centers):_*)
	}


}
/**
 *
 */
final case class KCenters[V <: GVector[V], D[X <: GVector[X]] <: Distance[X]](val k: Int, val metric: D[V], val minShift: Double, val maxIterations: Int, val customCenters: immutable.HashMap[Int, V] = immutable.HashMap.empty[Int, V]) extends KCentersAncestor[V, D[V], KCentersModel[V, D]] {

	val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KCenters

	def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): KCentersModel[V, D] = KCentersModel(k, metric, minShift, maxIterations, obtainCenters(data))
}