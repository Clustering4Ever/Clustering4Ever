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
/**
 *
 */
trait KCommons[V <: GVector[V], D <: Distance[V]] extends ClusteringSharedTypes {
	/**
	 * The number of cluster seeked
	 */
	val k: Int
	/**
	 * The employed metric on the GVector descendant
	 */
	val metric: D
	/**
	 * The stopping criteria, ie the distance under which centers are mooving from their previous position 
	 */
	val epsilon: Double
	/**
	 * The maximum number of iterations
	 */
	val maxIterations: Int
	/**
	 * A custom centers initialiazation, empty HashMap by default which result usage of K++ initialization
	 */
	val customCenters: immutable.HashMap[Int, V]
	/**
	 *
	 */
	protected def obtainNearestCenterID(v: V, centers: immutable.HashMap[Int, V], metric: D): ClusterID = {
		centers.minBy{ case (clusterID, center) => metric.d(center, v) }._1
	}
	/**
	 * Check if centers move enough
	 * @return true if every centers move less than epsilon
	 */
	protected def areCentersNotMovingEnough(kCentersBeforeUpdate: immutable.HashMap[Int, V], centers: immutable.HashMap[Int, V], epsilon: Double, metric: D) = {
		kCentersBeforeUpdate.forall{ case (clusterID, previousCenter) => metric.d(previousCenter, centers(clusterID)) <= epsilon }
	}
	/**
	 * Check if there are empty centers and remove them
	 */
	protected def removeEmptyClusters(centers: immutable.HashMap[Int, V], kCentersBeforeUpdate: immutable.HashMap[Int, V], centersCardinality: immutable.HashMap[Int, Long]): (immutable.HashMap[Int, V], immutable.HashMap[Int, V]) = {
		val emptyCenterIDs = centersCardinality.collect{ case (clusterID, cardinality) if(cardinality == 0) => clusterID }
		if(!emptyCenterIDs.isEmpty) {
			(
				centers -- emptyCenterIDs,
				kCentersBeforeUpdate -- emptyCenterIDs
			)
		}
		else {
			(
				centers,
				kCentersBeforeUpdate
			)
		}
	}
}
/**
 * The famous K-Centers using a user-defined dissmilarity measure.
 * @param data preferably and ArrayBuffer or ParArray of Clusterizable
 * @param k number of clusters
 * @param epsilon minimal threshold under which we consider a centroid has converged
 * @param maxIterations maximal number of iteration
 * @param metric a defined dissimilarity measure
 */
trait KCentersAncestor[V <: GVector[V], D <: Distance[V], CM <: KCentersModelAncestor[V, D]] extends KCommons[V, D] with ClusteringAlgorithmLocal[V, CM] {
	/**
	 * Run the K-Centers
	 */
	protected def obtainCenters[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): immutable.HashMap[Int, V] = {
		/**
		 *
		 */
		val centers: immutable.HashMap[Int, V] = if(customCenters.isEmpty) KPPInitializer.kppInit(data, metric, k) else customCenters
		/**
		 * KCenters heart in tailrec style
		 */
		@annotation.tailrec
		def go(cpt: Int, haveAllCentersConverged: Boolean, centers: immutable.HashMap[Int, V]): immutable.HashMap[Int, V] = {
			val centersInfo = data.groupBy( cz => obtainNearestCenterID(cz.v, centers, metric) ).map{ case (clusterID, aggregate) =>
				(clusterID, ClusterBasicOperations.obtainCenter(aggregate.map(_.v), metric), aggregate.size)
			}.toSeq.seq
			val newCenters = immutable.HashMap(centersInfo.map{ case (clusterID, center, _) => (clusterID, center) }:_*)
			val newCardinalities = immutable.HashMap(centersInfo.map{ case (clusterID, _, cardinality) => (clusterID, cardinality.toLong) }:_*)
			val (newCentersPruned, newKCentersBeforUpdatePruned) = removeEmptyClusters(newCenters, centers, newCardinalities)
			val shiftingEnough = areCentersNotMovingEnough(newKCentersBeforUpdatePruned, newCentersPruned, epsilon, metric)
			if(cpt < maxIterations && !shiftingEnough) {
				go(cpt + 1, shiftingEnough, newCentersPruned)
			}
			else {
				newCentersPruned.zipWithIndex.map{ case ((oldClusterID, center), newClusterID) => (newClusterID, center) }
			}
		}
		go(0, false, centers)
	}


}
/**
 *
 */
case class KCenters[V <: GVector[V], D[X <: GVector[X]] <: Distance[X]](val k: Int, val metric: D[V], val epsilon: Double, val maxIterations: Int, val customCenters: immutable.HashMap[Int, V] = immutable.HashMap.empty[Int, V]) extends KCentersAncestor[V, D[V], KCentersModel[V, D]] {

	val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KCenters

	def run[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): KCentersModel[V, D] = KCentersModel(k, metric, epsilon, maxIterations, obtainCenters(data))
}