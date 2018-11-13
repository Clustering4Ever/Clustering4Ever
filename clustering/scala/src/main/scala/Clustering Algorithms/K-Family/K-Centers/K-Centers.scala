package clustering4ever.scala.clustering.kcenters
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{mutable, GenSeq}
import scala.util.Random
import scala.reflect.ClassTag
import clustering4ever.math.distances.Distance
import clustering4ever.stats.Stats
import clustering4ever.scala.clusterizables.Clusterizable
import clustering4ever.clustering.{ClusteringCommons, LocalClusteringAlgorithm}
import clustering4ever.util.ClusterBasicOperations
/**
 *
 */
abstract class KCommons[
	@specialized(Int, Long) ID: Numeric,
	O,
	V: ClassTag,
	Cz <: Clusterizable[ID, O, V, Cz],
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
		val emptyCenterIDs = centersCardinality.collect{ case (clusterID, cardinality) if( cardinality == 0 ) => clusterID }
		if( ! emptyCenterIDs.isEmpty ) {
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
class KCenters[
	ID: Numeric,
	O,
	V: ClassTag,
	Cz <: Clusterizable[ID, O, V, Cz],
	D <: Distance[V]
](
	k: Int,
	epsilon: Double,
	maxIterations: Int,
	metric: D,
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
) extends KCommons[ID, O, V, Cz, D](k, epsilon, maxIterations, metric, initializedCenters) with LocalClusteringAlgorithm[GenSeq[Cz]] {
	/**
	 * Run the K-Centers
	 */
	def run(data: GenSeq[Cz]): KCentersModel[ID, O, V, Cz, D] =	{
		/**
		 *
		 */
		val centers: mutable.HashMap[Int, V] = if( initializedCenters.isEmpty ) {
			def obtainNearestCenter(v: V, centers: mutable.ArrayBuffer[V]): V = centers.minBy(metric.d(_, v))
			
			val centersBuff = mutable.ArrayBuffer(data(Random.nextInt(data.size)).vector)

			(1 until k).foreach( i => centersBuff += Stats.obtainCenterFollowingWeightedDistribution[V](data.map{ cz =>
				val toPow2 = metric.d(cz.vector, obtainNearestCenter(cz.vector, centersBuff))
				(cz.vector, toPow2 * toPow2)
			}.toBuffer) )

			val centers = mutable.HashMap(centersBuff.zipWithIndex.map{ case (center, clusterID) => (clusterID, center) }:_*)
			centers
		}
		else {
			initializedCenters
		}
		/**
		 *
		 */
		val centersCardinality: mutable.HashMap[Int, Int] = centers.map{ case (clusterID, _) => (clusterID, 0) }
		var cpt = 0
		var allCentersHaveConverged = false
		while( cpt < maxIterations && ! allCentersHaveConverged ) {
			// Allocation to nearest centroid
			val clusterized = data.map( cz => (obtainNearestCenterID(cz.vector, centers), cz.vector) )
			// Keep old position of centroids
			val kCentersBeforeUpdate = centers.clone
			// Compute centers and cardinality of each cluster
			clusterized.groupBy{ case (clusterID, _) => clusterID }.foreach{ case (clusterID, aggregate) =>
				centers(clusterID) = ClusterBasicOperations.obtainCenter(aggregate.map(_._2), metric)
				centersCardinality(clusterID) = aggregate.size
			}
			removeEmptyClusters(centers, kCentersBeforeUpdate, centersCardinality)
			// Check if all Centers Have Converged
			allCentersHaveConverged = areCentersMovingEnough(kCentersBeforeUpdate, centers, epsilon)
			cpt += 1
		}
		new KCentersModel[ID, O, V, Cz, D](centers, metric)
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
		initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]): KCentersModel[ID, O, V, Cz[ID, O, V], D] = {
		val kCenter = new KCenters[ID, O, V, Cz[ID, O, V], D](k, epsilon, maxIterations, metric, initializedCenters)
		val kCentersModel = kCenter.run(data)
		kCentersModel
	}
}