package org.clustering4ever.spark.clustering.kcenters

import scala.language.higherKinds
import org.apache.spark.SparkContext
import scala.collection.JavaConverters._
import scala.math.pow
import scala.collection.mutable
import scala.util.Random
import scala.reflect.ClassTag
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.stats.Stats
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.scala.clustering.kcenters.KCommons
import org.clustering4ever.util.{SumVectors, ClusterBasicOperations}
import org.clustering4ever.clustering.DistributedClusteringAlgorithm
import org.clustering4ever.vectors.GVector
import org.clustering4ever.clustering.ClusteringArgs
/**
 *
 */
case class KCentersArgs[V <: GVector[V], D <: Distance[V]](val k: Int, val metric: D, val epsilon: Double, val maxIterations: Int, val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, val initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]) extends ClusteringArgs {
	val algorithm = org.clustering4ever.extensibleAlgorithmNature.KCenters
}
/**
 *
 */
class KCenters[V <: GVector[V] : ClassTag, D <: Distance[V]](val args: KCentersArgs[V, D]) extends KCommons[V] with DistributedClusteringAlgorithm[V, KCentersArgs[V, D], KCentersModel[V, D]] {

	@deprecated("surely slower", "surely slower")
	private val emptyValue = mutable.ArrayBuffer.empty[V]
	@deprecated("surely slower", "surely slower")
	private def mergeValue(combiner: mutable.ArrayBuffer[V], comb: V): mutable.ArrayBuffer[V] = combiner += comb
	@deprecated("surely slower", "surely slower")
	private def mergeCombiners(combiner1: mutable.ArrayBuffer[V], combiner2: mutable.ArrayBuffer[V]): mutable.ArrayBuffer[V] = combiner1 ++= combiner2
	/**
	 * To upgrade
	 * Kmeans++ initialization
	 * <h2>References</h2>
	 * <ol>
	 * <li> Tapas Kanungo, David M. Mount, Nathan S. Netanyahu, Christine D. Piatko, Ruth Silverman, and Angela Y. Wu. An Efficient k-Means Clustering Algorithm: Analysis and Implementation. IEEE TRANS. PAMI, 2002.</li>
	 * <li> D. Arthur and S. Vassilvitskii. "K-means++: the advantages of careful seeding". ACM-SIAM symposium on Discrete algorithms, 1027-1035, 2007.</li>
	 * <li> Anna D. Peterson, Arka P. Ghosh and Ranjan Maitra. A systematic evaluation of different methods for initializing the K-means clustering algorithm. 2010.</li>
	 * </ol>
	 */
	private def kmppInitializationRDD[D <: Distance[V]](vectorizedDataset: RDD[V], k: Int, metric: D): mutable.HashMap[Int, V] = {

		def obtainNearestCenter(v: V, centers: mutable.ArrayBuffer[V]): V = centers.minBy(metric.d(_, v))
		
		val centersBuff = mutable.ArrayBuffer(vectorizedDataset.first)

		(1 until k).foreach( i =>
			centersBuff += Stats.obtainCenterFollowingWeightedDistribution[V](
				vectorizedDataset.map{ v =>
					val toPow2 = metric.d(v, obtainNearestCenter(v, centersBuff))
					(v, toPow2 * toPow2)
				}.sample(false, 0.01, 8L).collect.toBuffer
			)
		)

		val centers = mutable.HashMap(centersBuff.zipWithIndex.map{ case (center, clusterID) => (clusterID, center) }:_*)
		centers
	}
	/**
	 *
	 */
	def run[
		ID,
		O,
		Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]
	](data: RDD[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): KCentersModel[V, D] = {
		
		data.persist(args.persistanceLVL)
		/**
		 * To upgrade
		 */
		val centers: mutable.HashMap[Int, V] = if(args.initializedCenters.isEmpty) kmppInitializationRDD(data.map(_.v), args.k, args.metric) else args.initializedCenters
		val kCentersBeforeUpdate: mutable.HashMap[Int, V] = centers.clone
		val clustersCardinality: mutable.HashMap[Int, Long] = centers.map{ case (clusterID, _) => (clusterID, 0L) }
		
		def updateCentersAndCardinalities(centersInfo: Iterable[(Int, Long, V)]) = {
			centersInfo.foreach{ case (clusterID, cardinality, center) =>
				kCentersBeforeUpdate(clusterID) = center
				clustersCardinality(clusterID) = cardinality
			}
		}

		def checkIfConvergenceAndUpdateCenters(centersInfo: Iterable[(Int, Long, V)], epsilon: Double) = {
			updateCentersAndCardinalities(centersInfo)
			val allModHaveConverged = areCentersMovingEnough(kCentersBeforeUpdate, centers, epsilon, args.metric)
			kCentersBeforeUpdate.foreach{ case (clusterID, mode) => centers(clusterID) = mode }	
			allModHaveConverged
		}
		@deprecated("surely slower", "surely slower")
		def obtainCentersInfo = {
			data.map( cz => (obtainNearestCenterID(cz.v, centers, args.metric), cz.v) )
				.aggregateByKey(emptyValue)(mergeValue, mergeCombiners)
				.map{ case (clusterID, aggregate) => 
					(
						clusterID,
						aggregate.size.toLong,
						ClusterBasicOperations.obtainCenter(aggregate.par, args.metric)
					)
				}.collect
			}
		var cpt = 0
		var allModHaveConverged = false
		while(cpt < args.maxIterations && !allModHaveConverged) {
			// val centersInfo = obtainCentersInfo
			val centersInfo = data.map( cz => (obtainNearestCenterID(cz.v, centers, args.metric), (1L, cz.v)) )
				.reduceByKeyLocally{ case ((card1, v1), (card2, v2)) => ((card1 + card2), ClusterBasicOperations.obtainCenter(List(v1, v2), args.metric)) }
				.map{ case (clusterID, (cardinality, center)) => (clusterID, cardinality, center) }
				.toArray

			allModHaveConverged = checkIfConvergenceAndUpdateCenters(centersInfo, args.epsilon)
			cpt += 1
		}
		new KCentersModel[V, D](centers, args.metric)
	}
}