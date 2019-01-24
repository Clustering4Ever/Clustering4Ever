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
import org.clustering4ever.clustering.{ClusteringCommons, ClusteringAlgorithmLocal}
import org.clustering4ever.util.ClusterBasicOperations
import org.clustering4ever.clustering.ClusteringArgs
import org.clustering4ever.vectors.GVector
/**
 *
 */
trait KCommons[V <: GVector[V]] extends ClusteringCommons {
	/**
	 *
	 */
	protected def obtainNearestCenterID[D <: Distance[V]](v: V, centers: immutable.HashMap[Int, V], metric: D): ClusterID = centers.minBy{ case (clusterID, center) => metric.d(center, v) }._1
	/**
	 * Check if centers move enough
	 * @return true if every centers move less than epsilon
	 */
	protected def areCentersMovingEnough[D <: Distance[V]](kCentersBeforeUpdate: immutable.HashMap[Int, V], centers: immutable.HashMap[Int, V], epsilon: Double, metric: D) = kCentersBeforeUpdate.forall{ case (clusterID, previousCenter) => metric.d(previousCenter, centers(clusterID)) <= epsilon }
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
 * @param data : preferably and ArrayBuffer or ParArray of Clusterizable
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param maxIterations : maximal number of iteration
 * @param metric : a defined dissimilarity measure
 */
trait KCentersAncestor[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D <: Distance[V], GS[X] <: GenSeq[X], +Args <: KCentersArgsAncestor[V, D], +Model <: KCentersModelAncestor[ID, O, V, Cz, D, GS, Args]] extends KCommons[V] with ClusteringAlgorithmLocal[ID, O, V, Cz, GS, Args, Model] {
	/**
	 *
	 */
	val args: Args
	/**
	 *
	 */
	protected implicit val ct: ClassTag[Cz[ID, O, V]]
	/**
	 * Run the K-Centers
	 */
	protected def obtainCenters(data: GS[Cz[ID, O, V]]): immutable.HashMap[Int, V] = {
		/**
		 *
		 */
		val centers: immutable.HashMap[Int, V] = if(args.initializedCenters.isEmpty) KPPInitializer.kppInit[ID, O, V, Cz, D](data, args.metric, args.k) else args.initializedCenters
		/**
		 * KCenters heart in tailrec style
		 */
		@annotation.tailrec
		def go(cpt: Int, allCentersHaveConverged: Boolean, centers: immutable.HashMap[Int, V]): immutable.HashMap[Int, V] = {
			if(cpt < args.maxIterations && !allCentersHaveConverged) {
				val centersInfo = data.groupBy( cz => obtainNearestCenterID(cz.v, centers, args.metric) ).map{ case (clusterID, aggregate) =>
					(clusterID, ClusterBasicOperations.obtainCenter(aggregate.map(_.v), args.metric), aggregate.size)
				}.toSeq.seq

				val newCenters = immutable.HashMap(centersInfo.map{ case (clusterID, center, _) => (clusterID, center) }:_*)
				val newCardinalities = immutable.HashMap(centersInfo.map{ case (clusterID, _, cardinality) => (clusterID, cardinality.toLong) }:_*)
				val (newCentersPruned, newKCentersBeforUpdatePruned) = removeEmptyClusters(newCenters, centers, newCardinalities)

				go(cpt + 1, areCentersMovingEnough(newKCentersBeforUpdatePruned, newCentersPruned, args.epsilon, args.metric), newCentersPruned)
			}
			else {
				centers
			}
		}
		go(0, false, centers)
	}


}
/**
 *
 */
case class KCenters[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: GVector[X]] <: Distance[X], GS[X] <: GenSeq[X]](val args: KCentersArgs[V, D])(protected implicit val ct: ClassTag[Cz[ID, O, V]]) extends KCentersAncestor[ID, O, V, Cz, D[V], GS, KCentersArgs[V, D], KCentersModel[ID, O, V, Cz, D, GS]] {
	def run(data: GS[Cz[ID, O, V]]): KCentersModel[ID, O, V, Cz, D, GS] = KCentersModel(obtainCenters(data), args.metric, args)
}
/**
 * The general KCenters compagnion object helper
 */
object KCenters {
	/**
	 *
	 */
	def run[
		ID,
		O,
		V <: GVector[V] : ClassTag,
		Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
		D[X <: GVector[X]] <: Distance[X],
		GS[X] <: GenSeq[X]
	](
		data: GS[Cz[ID, O, V]],
		k: Int,
		metric: D[V],
		epsilon: Double,
		maxIterations: Int,
		initializedCenters: immutable.HashMap[Int, V] = immutable.HashMap.empty[Int, V]
	)(implicit ct: ClassTag[Cz[ID, O, V]]): KCentersModel[ID, O, V, Cz, D, GS] = {
		(KCenters[ID, O, V, Cz, D, GS](KCentersArgs(k, metric, epsilon, maxIterations, initializedCenters))).run(data)
	}
}