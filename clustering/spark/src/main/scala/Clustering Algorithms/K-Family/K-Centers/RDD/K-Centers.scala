package org.clustering4ever.clustering.kcenters.rdd
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import org.apache.spark.SparkContext
import scala.collection.JavaConverters._
import scala.math.pow
import scala.collection.{immutable, mutable}
import scala.util.Random
import scala.reflect.ClassTag
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.stats.Stats
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.clustering.kcenters.scala.{KCommons, KCentersModelCommons}
import org.clustering4ever.util.ClusterBasicOperations
import org.clustering4ever.clustering.rdd.ClusteringAlgorithmDistributed
import org.clustering4ever.vectors.GVector
/**
 *
 */
trait KCommonsSpark[V <: GVector[V], D <: Distance[V]] extends KCommons[V, D] {
	/**
	 *
	 */
	val persistanceLVL: StorageLevel
	/**
	 *
	 */
	protected implicit val ctV: ClassTag[V]
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
	protected def kmppInitializationRDD(vectorizedDataset: RDD[V], k: Int, metric: D): immutable.HashMap[Int, V] = {

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

		val centers = immutable.HashMap(centersBuff.zipWithIndex.map{ case (center, clusterID) => (clusterID, center) }:_*)
		centers
	}
}
/**
 *
 */
trait KCentersAncestor[V <: GVector[V], D <: Distance[V], CA <: KCentersModelAncestor[V, D]] extends KCommonsSpark[V, D] with ClusteringAlgorithmDistributed[V, CA] {
	/**
	 *
	 */
	protected def obtainCenters[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: RDD[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): immutable.HashMap[Int, V] = {
		
		data.persist(persistanceLVL)

		val centers: immutable.HashMap[Int, V] = if(customCenters.isEmpty) kmppInitializationRDD(data.map(_.v), k, metric) else customCenters
		/**
		 * KCenters heart in tailrec style
		 */
		@annotation.tailrec
		def go(cpt: Int, haveAllCentersConverged: Boolean, centers: immutable.HashMap[Int, V]): immutable.HashMap[Int, V] = {
			val centersInfo = data.map( cz => (obtainNearestCenterID(cz.v, centers, metric), (1L, cz.v)) )
				.reduceByKeyLocally{ case ((card1, v1), (card2, v2)) => ((card1 + card2), ClusterBasicOperations.obtainCenter(Seq(v1, v2), metric)) }
				.map{ case (clusterID, (cardinality, center)) => (clusterID, center, cardinality) }
				.toArray
			val newCenters = immutable.HashMap(centersInfo.map{ case (clusterID, center, _) => (clusterID, center) }:_*)
			val newCardinalities = immutable.HashMap(centersInfo.map{ case (clusterID, _, cardinality) => (clusterID, cardinality) }:_*)
			val (newCentersPruned, newKCentersBeforUpdatePruned) = removeEmptyClusters(newCenters, centers, newCardinalities)
			val shiftingEnough = areCentersNotMovingEnough(newKCentersBeforUpdatePruned, newCentersPruned, epsilon, metric)
			if(cpt < maxIterations && !shiftingEnough) {
				go(cpt + 1, shiftingEnough, newCentersPruned)
			}
			else {
				centers.zipWithIndex.map{ case ((oldClusterID, center), newClusterID) => (newClusterID, center) }
			}
		}
		go(0, false, centers)
	}
}
/**
 *
 */
case class KCenters[V <: GVector[V], D[X <: GVector[X]] <: Distance[X]](val k: Int, val metric: D[V], val epsilon: Double, val maxIterations: Int, val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, val customCenters: immutable.HashMap[Int, V])(implicit val ctV: ClassTag[V]) extends KCentersAncestor[V, D[V], KCentersModel[V, D]] {

	def run[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: RDD[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): KCentersModel[V, D] = KCentersModel[V, D](k, metric, epsilon, maxIterations, persistanceLVL, obtainCenters(data))
}