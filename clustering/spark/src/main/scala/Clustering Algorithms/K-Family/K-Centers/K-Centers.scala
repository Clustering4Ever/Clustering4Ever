package org.clustering4ever.spark.clustering.kcenters

import scala.language.higherKinds
import org.apache.commons.math3.distribution.EnumeratedDistribution
import org.apache.commons.math3.util.Pair
import scala.collection.JavaConverters._
import scala.math.pow
import scala.collection.mutable
import scala.util.Random
import scala.reflect.ClassTag
import spire.math.{Numeric => SNumeric}
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.stats.Stats
import org.clustering4ever.scala.clusterizables.Clusterizable
import org.clustering4ever.scala.clustering.kcenters.KCommons
import org.clustering4ever.clustering.FusionedModelsDistributed
import org.clustering4ever.util.{SumVectors, ClusterBasicOperations}
import org.clustering4ever.clustering.DistributedClusteringAlgorithm
import org.clustering4ever.scala.vectors.GVector
/**
 *
 */
class KCenters[
	V <: GVector : ClassTag,
	D <: Distance[V]
](
	val k: Int,
	metric: D,
	val epsilon: Double,
	maxIterations: Int,
	persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY,
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
) extends KCommons[V] with DistributedClusteringAlgorithm[V] {

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
	private def kmppInitializationRDD(vectorizedDataset: RDD[V], k: Int): mutable.HashMap[Int, V] = {

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
		Cz[X, Y, Z <: GVector] <: Clusterizable[X, Y, Z, Cz]
	](data: RDD[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): KCentersModel[ID, O, V, Cz, D] = {
		
		data.persist(persistanceLVL)
		/**
		 * To upgrade
		 */
		val centers: mutable.HashMap[Int, V] = if(initializedCenters.isEmpty) kmppInitializationRDD(data.map(_.workingVector), k) else initializedCenters
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
			val allModHaveConverged = areCentersMovingEnough(kCentersBeforeUpdate, centers, epsilon, metric)
			kCentersBeforeUpdate.foreach{ case (clusterID, mode) => centers(clusterID) = mode }	
			allModHaveConverged
		}
		@deprecated("surely slower", "surely slower")
		def obtainCentersInfo = {
			data.map( cz => (obtainNearestCenterID(cz.workingVector, centers, metric), cz.workingVector) )
				.aggregateByKey(emptyValue)(mergeValue, mergeCombiners)
				.map{ case (clusterID, aggregate) => 
					(
						clusterID,
						aggregate.size.toLong,
						ClusterBasicOperations.obtainCenter(aggregate.par, metric)
					)
				}.collect
			}
		var cpt = 0
		var allModHaveConverged = false
		while(cpt < maxIterations && !allModHaveConverged) {
			// val centersInfo = obtainCentersInfo
			val centersInfo = data.map( cz => (obtainNearestCenterID(cz.workingVector, centers, metric), (1L, cz.workingVector)) )
				.reduceByKeyLocally{ case ((card1, v1), (card2, v2)) => ((card1 + card2), ClusterBasicOperations.obtainCenter(List(v1, v2), metric)) }
				.map{ case (clusterID, (cardinality, center)) => (clusterID, cardinality, center) }
				.toArray

			allModHaveConverged = checkIfConvergenceAndUpdateCenters(centersInfo, epsilon)
			cpt += 1
		}
		new KCentersModel[ID, O, V, Cz, D](centers, metric)
	}
}