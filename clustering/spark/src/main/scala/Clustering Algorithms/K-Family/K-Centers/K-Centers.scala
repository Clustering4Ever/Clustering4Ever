package clustering4ever.spark.clustering.kcenters

import scala.language.higherKinds
import org.apache.commons.math3.distribution.EnumeratedDistribution
import org.apache.commons.math3.util.Pair
import scala.collection.JavaConverters._
import scala.math.pow
import scala.collection.{immutable, mutable, parallel}
import scala.util.Random
import scala.reflect.ClassTag
import spire.math.{Numeric => SNumeric}
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import clustering4ever.math.distances.Distance
import clustering4ever.stats.Stats
import clustering4ever.scala.clusterizables.Clusterizable
import clustering4ever.scala.clustering.kcenters.KCommons
import clustering4ever.clustering.CenterOrientedModelDistributed
import clustering4ever.util.{SumVectors, ClusterBasicOperations}
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.clustering.DistributedClusteringAlgorithm
/**
 *
 */
class KCenters[
	ID: Numeric,
	O,
	V: ClassTag,
	Cz <: Clusterizable[ID, O, V, Cz] : ClassTag,
	D <: Distance[V]
	](
	k: Int,
	epsilon: Double,
	maxIterations: Int,
	metric: D,
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V],
	persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY
) extends KCommons[ID, O, V, Cz, D](k, epsilon, maxIterations, metric, initializedCenters) with DistributedClusteringAlgorithm[RDD[Cz]] {

	private val emptyValue = mutable.ArrayBuffer.empty[V]
	private def mergeValue(combiner: mutable.ArrayBuffer[V], comb: V): mutable.ArrayBuffer[V] = combiner += comb
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

		(1 until k).foreach( i => centersBuff += Stats.obtainCenterFollowingWeightedDistribution[V](vectorizedDataset.map{ v =>
			val toPow2 = metric.d(v, obtainNearestCenter(v, centersBuff))
			(v, toPow2 * toPow2)
		}.sample(false, 0.01, 8L).collect.toBuffer) )

		val centers = mutable.HashMap(centersBuff.zipWithIndex.map{ case (center, clusterID) => (clusterID, center) }:_*)
		centers
	}
	/**
	 *
	 */
	def run(data: RDD[Cz]): KCentersModel[ID, O, V, Cz, D] = {
		/**
		 * To upgrade
		 */
		val centers: mutable.HashMap[Int, V] = if( initializedCenters.isEmpty ) kmppInitializationRDD(data.map(_.vector).persist(persistanceLVL), k) else initializedCenters
		val kCentersBeforeUpdate: mutable.HashMap[Int, V] = centers.clone
		val clustersCardinality: mutable.HashMap[Int, Long] = centers.map{ case (clusterID, _) => (clusterID, 0L) }
		
		def updateCentersAndCardinalities(centersInfo: Iterable[(Int, Long, V)]) = {
			centersInfo.foreach{ case (clusterID, cardinality, center) =>
				kCentersBeforeUpdate(clusterID) = center.asInstanceOf[V]
				clustersCardinality(clusterID) = cardinality
			}
		}

		def checkIfConvergenceAndUpdateCenters(centersInfo: Iterable[(Int, Long, V)], epsilon: Double) = {
			updateCentersAndCardinalities(centersInfo)
			val allModHaveConverged = areCentersMovingEnough(kCentersBeforeUpdate, centers, epsilon)
			kCentersBeforeUpdate.foreach{ case (clusterID, mode) => centers(clusterID) = mode }	
			allModHaveConverged
		}
		def obtainCentersInfo = {
				data.map( cz => (obtainNearestCenterID(cz.vector, centers), cz.vector) ).aggregateByKey(emptyValue)(mergeValue, mergeCombiners)
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
		while( cpt < maxIterations && ! allModHaveConverged ) {
			val centersInfo = obtainCentersInfo
			allModHaveConverged = checkIfConvergenceAndUpdateCenters(centersInfo, epsilon)
			cpt += 1
		}
		new KCentersModel[ID, O, V, Cz, D](centers, metric)
	}
}