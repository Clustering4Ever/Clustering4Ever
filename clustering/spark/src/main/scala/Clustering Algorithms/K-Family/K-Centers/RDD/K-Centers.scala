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
import org.clustering4ever.clustering.kcenters.scala.KCommons
import org.clustering4ever.util.ClusterBasicOperations
import org.clustering4ever.clustering.{ClusteringCommons, ClusteringAlgorithmDistributed}
import org.clustering4ever.vectors.GVector
import org.clustering4ever.clustering.ClusteringArgsDistributed
/**
 *
 */
trait KCommonsSpark[V <: GVector[V]] extends KCommons[V] {
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
	protected def kmppInitializationRDD[D <: Distance[V]](vectorizedDataset: RDD[V], k: Int, metric: D): immutable.HashMap[Int, V] = {

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
trait KCentersAncestor[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D <: Distance[V], +Args <: KCentersArgsAncestor[V, D], +Model <: KCentersModelAncestor[ID, O, V, Cz, D, Args]] extends KCommonsSpark[V] with ClusteringAlgorithmDistributed[ID, O, V, Cz, Args, Model] {
	/**
	 *
	 */
	protected def obtainCenters(data: RDD[Cz[ID, O, V]]): immutable.HashMap[Int, V] = {
		
		data.persist(args.persistanceLVL)

		val centers: immutable.HashMap[Int, V] = if(args.initializedCenters.isEmpty) kmppInitializationRDD(data.map(_.v), args.k, args.metric) else args.initializedCenters
		/**
		 * KCenters heart in tailrec style
		 */
		@annotation.tailrec
		def go(cpt: Int, allCentersHaveConverged: Boolean, centers: immutable.HashMap[Int, V]): immutable.HashMap[Int, V] = {
			if(cpt < args.maxIterations && !allCentersHaveConverged) {
				val centersInfo = data.map( cz => (obtainNearestCenterID(cz.v, centers, args.metric), (1L, cz.v)) )
					.reduceByKeyLocally{ case ((card1, v1), (card2, v2)) => ((card1 + card2), ClusterBasicOperations.obtainCenter(Seq(v1, v2), args.metric)) }
					.map{ case (clusterID, (cardinality, center)) => (clusterID, center, cardinality) }
					.toArray

				val newCenters = immutable.HashMap(centersInfo.map{ case (clusterID, center, _) => (clusterID, center) }:_*)
				val newCardinalities = immutable.HashMap(centersInfo.map{ case (clusterID, _, cardinality) => (clusterID, cardinality) }:_*)
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
case class KCenters[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: GVector[X]] <: Distance[X]](val args: KCentersArgs[V, D])(protected implicit val ct: ClassTag[Cz[ID, O, V]],protected val ctV: ClassTag[V]) extends KCentersAncestor[ID, O, V, Cz, D[V], KCentersArgs[V, D], KCentersModel[ID, O, V, Cz, D]] {
	/**
	 *
	 */
	def run(data: RDD[Cz[ID, O, V]]): KCentersModel[ID, O, V, Cz, D] = KCentersModel[ID, O, V, Cz, D](obtainCenters(data), args.metric, args)
}