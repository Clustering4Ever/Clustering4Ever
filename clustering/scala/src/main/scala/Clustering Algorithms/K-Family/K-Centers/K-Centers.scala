package org.clustering4ever.scala.clustering.kcenters
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{mutable, GenSeq}
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
	 *
	 */
	protected def obtainNearestCenterID[D <: Distance[V]](v: V, centers: mutable.HashMap[Int, V], metric: D): ClusterID = centers.minBy{ case (clusterID, center) => metric.d(center, v) }._1
	/**
	 * Check if centers move enough
	 * @return true if every centers move less than epsilon
	 */
	protected def areCentersMovingEnough[D <: Distance[V]](kCentersBeforeUpdate: mutable.HashMap[Int, V], centers: mutable.HashMap[Int, V], epsilon: Double, metric: D) = kCentersBeforeUpdate.forall{ case (clusterID, previousCenter) => metric.d(previousCenter, centers(clusterID)) <= epsilon }
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
	implicit val ct: ClassTag[Cz[ID, O, V]]
	/**
	 * Run the K-Centers
	 */
	def obtainCenters(data: GS[Cz[ID, O, V]]): mutable.HashMap[Int, V] = {
		/**
		 *
		 */
		val centers: mutable.HashMap[Int, V] = if(args.initializedCenters.isEmpty) KPPInitializer.kppInit[ID, O, V, Cz, D](data, args.metric, args.k) else args.initializedCenters
		/**
		 *
		 */
		val centersCardinality: mutable.HashMap[Int, Int] = centers.map{ case (clusterID, _) => (clusterID, 0) }
		/**
		 * KCenters heart in tailrec style
		 */
		@annotation.tailrec
		def go(cpt: Int, allCentersHaveConverged: Boolean): mutable.HashMap[Int, V] = {
			if(cpt < args.maxIterations && !allCentersHaveConverged) {
				// Keep old position of centroids
				val kCentersBeforeUpdate = centers.clone
				// Compute centers and cardinality of each cluster
				data.groupBy( cz => obtainNearestCenterID(cz.v, centers, args.metric) ).foreach{ case (clusterID, aggregate) =>
					centers(clusterID) = ClusterBasicOperations.obtainCenter(aggregate.map(_.v), args.metric)
					centersCardinality(clusterID) = aggregate.size
				}
				removeEmptyClusters(centers, kCentersBeforeUpdate, centersCardinality)

				go(cpt + 1, areCentersMovingEnough(kCentersBeforeUpdate, centers, args.epsilon, args.metric))
			}
			else {
				centers
			}
		}
		go(0, false)
	}
}
/**
 *
 */
case class KCenters[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: GVector[X]] <: Distance[X], GS[X] <: GenSeq[X]](val args: KCentersArgs[V, D])(implicit val ct: ClassTag[Cz[ID, O, V]]) extends KCentersAncestor[ID, O, V, Cz, D[V], GS, KCentersArgs[V, D], KCentersModel[ID, O, V, Cz, D, GS]] {
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
		initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
	)(implicit ct: ClassTag[Cz[ID, O, V]]): KCentersModel[ID, O, V, Cz, D, GS] = {
		(KCenters[ID, O, V, Cz, D, GS](KCentersArgs(k, metric, epsilon, maxIterations, initializedCenters))).run(data)
	}
}