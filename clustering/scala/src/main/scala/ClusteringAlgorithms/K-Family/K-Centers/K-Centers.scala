package org.clustering4ever.clustering.kcenters.scala
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{immutable, mutable, GenSeq}
import scala.util.Random
import scala.reflect.ClassTag
// import shapeless._
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
	private[kcenters] final def obtainNearestCenterID(v: V, centers: List[(Int, V)], metric: D): ClusterID = {

			@annotation.tailrec
			def go(centers: List[(Int, V)], min: (Double, Int)): Int = {
				centers match {
					case (clusterID, center) :: xs => {
						val d = metric.d(center, v)
						if (d <= min._1) go(xs, (d, clusterID))
						else go(xs, min)
					}
					case Nil => min._2
				}
			}

			go(centers, (Double.MaxValue, Int.MaxValue))


	}
	/**
	 * Check if centers move enough
	 * @return true if every centers move less than minShift
	 */
	private[kcenters] final def areCentersNotMovingEnough(updatedCenters: List[(Int, V)], previousCenters: List[(Int, V)], minShift: Double, metric: D) = {

		@annotation.tailrec
		def go(updatedCenters: List[(Int, V)], previousCenters: List[(Int, V)], bool: Boolean): Boolean = {
			
			if (bool) {
				updatedCenters match {
					case (_, center1) :: xs1 => {
						val (_, center2) :: xs2 = previousCenters
						val newBool = metric.d(center1, center2) <= minShift
						go(xs1, xs2, newBool)
					} 
					case Nil => bool
				}
			}
			else {
				bool
			}

		}

		go(updatedCenters, previousCenters, true)

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

		val unSortedCenters: List[(Int, V)] = if (customCenters.isEmpty) {
			KPPInitializer.kppInit(data, metric, k).toList
		}
		else {
			customCenters.toList
		}

		val centers = unSortedCenters.sortBy(_._1)
		/**
		 * KCenters heart in tailrec style
		 */
		@annotation.tailrec
		def go(cpt: Int, haveAllCentersConverged: Boolean, centers: List[(Int, V)]): List[(Int, V)] = {
			val preUpdatedCenters = data.groupBy( cz => obtainNearestCenterID(cz.v, centers, metric) )
				.map{ case (clusterID, aggregate) =>
					(clusterID, ClusterBasicOperations.obtainCenter(aggregate.map(_.v), metric))
				}
				.toList
				.sortBy(_._1)
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