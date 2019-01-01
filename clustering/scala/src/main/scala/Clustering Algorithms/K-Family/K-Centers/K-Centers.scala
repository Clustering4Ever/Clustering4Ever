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
import org.clustering4ever.scala.clusterizables.Clusterizable
import org.clustering4ever.clustering.{ClusteringCommons, LocalClusteringAlgorithm}
import org.clustering4ever.util.ClusterBasicOperations
import org.clustering4ever.clustering.ClusteringArgs
import org.clustering4ever.scala.vectors.GVector
/**
 *
 */
trait KCommons[V <: GVector] extends ClusteringCommons {
	/**
	 *
	 */
	val k: Int
	/**
	 *
	 */
	val epsilon: Double
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
class KCenters[
	V <: GVector : ClassTag,
	D <: Distance[V],
	GS[X] <: GenSeq[X]
](
	val k: Int,
	metric: D,
	val epsilon: Double,
	maxIterations: Int,
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
) extends KCommons[V] with LocalClusteringAlgorithm[V, GS] {
	/**
	 * Run the K-Centers
	 */
	def run[
		ID,
		O,
		Cz[X, Y, Z <: GVector] <: Clusterizable[X, Y, Z, Cz]
	](data: GS[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): KCentersModel[V, D] = {
		/**
		 *
		 */
		val centers: mutable.HashMap[Int, V] = if(initializedCenters.isEmpty) KPPInitializer.kppInit[ID, O, V, Cz, D](data, metric, k) else initializedCenters
		/**
		 *
		 */
		val centersCardinality: mutable.HashMap[Int, Int] = centers.map{ case (clusterID, _) => (clusterID, 0) }
		/**
		 * KMeans heart in tailrec style
		 */
		@annotation.tailrec
		def go(cpt: Int, allCentersHaveConverged: Boolean): mutable.HashMap[Int, V] = {
			if(cpt < maxIterations && !allCentersHaveConverged) {
				// Keep old position of centroids
				val kCentersBeforeUpdate = centers.clone
				// Compute centers and cardinality of each cluster
				data.groupBy( cz => obtainNearestCenterID(cz.workingVector, centers, metric) ).foreach{ case (clusterID, aggregate) =>
					centers(clusterID) = ClusterBasicOperations.obtainCenter(aggregate.map(_.workingVector), metric)
					centersCardinality(clusterID) = aggregate.size
				}
				removeEmptyClusters(centers, kCentersBeforeUpdate, centersCardinality)

				go(cpt + 1, areCentersMovingEnough(kCentersBeforeUpdate, centers, epsilon, metric))
			}
			else {
				centers
			}
		}

		object KCentersArgs extends ClusteringArgs {
			val kValue = k
			val epsilonValue = epsilon
			val maxIterationsValue = maxIterations
			val metricValue = metric.toString
		}

		new KCentersModel[V, D](go(0, false), metric, KCentersArgs)
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
		ID,
		O,
		V <: GVector : ClassTag,
		Cz[X, Y, Z <: GVector] <: Clusterizable[X, Y, Z, Cz],
		D <: Distance[V],
		GS[X] <: GenSeq[X]
	](
		data: GS[Cz[ID, O, V]],
		k: Int,
		metric: D,
		epsilon: Double,
		maxIterations: Int,
		initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
	)(implicit ct: ClassTag[Cz[ID, O, V]]): KCentersModel[V, D] = {
		val kCenter = new KCenters[V, D, GS](k, metric, epsilon, maxIterations, initializedCenters)
		val kCentersModel = kCenter.run(data)
		kCentersModel
	}
}