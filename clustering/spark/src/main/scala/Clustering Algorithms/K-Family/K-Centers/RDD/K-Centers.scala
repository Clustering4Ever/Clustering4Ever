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
import org.clustering4ever.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.clustering.kcenters.scala.{KCommons, KCentersModelCommons}
import org.clustering4ever.util.{SumVectors, ClusterBasicOperations}
import org.clustering4ever.clustering.rdd.ClusteringAlgorithmDistributed
import org.clustering4ever.vectors.GVector
import org.clustering4ever.clustering.kcenters.scala.KPPInitializer
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
	protected final def kmppInitializationRDD(vectorizedDataset: RDD[V], k: Int, metric: D): immutable.HashMap[Int, V] = {

		def obtainNearestCenter(v: V, centers: mutable.ArrayBuffer[V]): V = centers.minBy(metric.d(_, v))
		
		val centersBuff = mutable.ArrayBuffer(vectorizedDataset.first)

		(1 until k).foreach( _ =>
			centersBuff += Stats.obtainMedianFollowingWeightedDistribution[V](
				vectorizedDataset.map{ v =>
					val toPow2 = metric.d(v, obtainNearestCenter(v, centersBuff))
					(v, toPow2 * toPow2)
				}.sample(false, 0.01, 8L).collect.toBuffer
			)
		)

		val centers = immutable.HashMap(centersBuff.zipWithIndex.map{ case (center, clusterID) => (clusterID, center) }:_*)
		centers
	}
	/**
	 *
	 */
	final def parallelKmPPInitialization(vectorizedDataset: RDD[V], k: Int, metric: D, l: Int = 10, numIter: Int = 5): immutable.HashMap[Int, V] = {
			val centers = mutable.ArrayBuffer(vectorizedDataset.sample(false, 0.001, 8L).collect.head)
			val firstCenter = centers.head
			def obtainNearestCenter(v: V, centers: mutable.ArrayBuffer[V]): V = centers.minBy(metric.d(_, v))
			def firstAcc(agg: Double, v: V): Double = {
				val toPow2 = metric.d(v, firstCenter)
				agg + toPow2 * toPow2				
			}
			val phi = vectorizedDataset.aggregate(0D)(firstAcc, _ + _)

			// Each partition will generate a new prototypes
			val vectorizedDatasetRepartitionned = vectorizedDataset.repartition(k * l)

			val logPhiBound = math.log(phi).toInt
			val iterMax = if (numIter <= 0) logPhiBound else numIter
			@annotation.tailrec
			def go(phi: Double, centers: mutable.ArrayBuffer[V], i: Int): mutable.ArrayBuffer[V] = {
				if (i < iterMax) {
					
					val preprocessed = vectorizedDatasetRepartitionned.map{ v =>
						val toPow2 = metric.d(v, obtainNearestCenter(v, centers))
						(v, toPow2 * toPow2)
					}
					
					val newPhi = preprocessed.aggregate(0D)((agg, e) => agg + e._2, _ + _)

					val preCenters = preprocessed.mapPartitions{ it =>
						val probabilities = it.map{ case (v, toPow2) =>
							val toPow2 = metric.d(v, obtainNearestCenter(v, centers))
							(v, (l * toPow2) / newPhi)
						}
						val preCenter = Stats.obtainMedianFollowingWeightedDistribution(probabilities.toVector)
						Iterator(preCenter)
					}.collect

					centers ++= preCenters
					
					go(newPhi, centers, i + 1)
				}
				else centers
			}

			val preFinalCenters = go(phi, centers, 0)
			val formatedPreFinalCenters = preFinalCenters.zipWithIndex.map{ case (v, id) => EasyClusterizable(id, v) }

			val kInitCenters = KPPInitializer.kppInit(formatedPreFinalCenters, metric, k)
			kInitCenters
	}
	/**
	 * Select randomly k points which will becomes k centers itinialization.
	 */
	protected final def randomSelectedInitializationRDD(vectorizedDataset: RDD[V], k: Int): immutable.HashMap[Int, V] = {
		immutable.HashMap(vectorizedDataset.takeSample(false, k).zipWithIndex.map(_.swap):_*)
	}
}
/**
 *
 */
trait KCentersAncestor[V <: GVector[V], D <: Distance[V], CA <: KCentersModelAncestor[V, D]] extends KCommonsSpark[V, D] with ClusteringAlgorithmDistributed[V, CA] {

	implicit val sumVector: (V, V) => V
	implicit val getCenter: (V, Long) => V 
	/**
	 *
	 */
	protected final def obtainCenters[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, V]])(implicit ct: ClassTag[Cz[O, V]]): immutable.HashMap[Int, V] = {
		
		data.persist(persistanceLVL)

		val unSortedCenters = if (customCenters.isEmpty) randomSelectedInitializationRDD(data.map(_.v), k) else customCenters
		val centers = unSortedCenters.toList.sortBy(_._1)

		/**
		 * KCenters heart in tailrec style
		 */
		@annotation.tailrec
		def go(cpt: Int, haveAllCentersConverged: Boolean, centers: List[(Int, V)]): List[(Int, V)] = {
			val preUpdatedCenters = data.map( cz => (obtainNearestCenterID(cz.v, centers, metric), (cz.v, 1)) )
				.reduceByKeyLocally{ case ((v1, c1), (v2, c2)) => (SumVectors.sumVectors(v1, v2) : @inline, (c1 + c2)) }
				.toList
				.map{ case (clusterID, (centroid, size)) => (clusterID, getCenter(centroid, size)) }
				.sortBy(_._1)
			val alignedOldCenters = preUpdatedCenters.map{ case (oldClusterID, _) => centers(oldClusterID) }
			val updatedCenters = preUpdatedCenters.zipWithIndex.map{ case ((oldClusterID, center), newClusterID) => (newClusterID, center) }
			val shiftingEnough = areCentersNotMovingEnough(updatedCenters, alignedOldCenters, minShift, metric)
			if(cpt < maxIterations && !shiftingEnough) {
				go(cpt + 1, shiftingEnough, updatedCenters)
			}
			else {
				updatedCenters
			}
		}
		immutable.HashMap(go(0, false, centers):_*)
	}
}
/**
 *
 */
final case class KCenters[V <: GVector[V], D[X <: GVector[X]] <: Distance[X]](final val k: Int, final val metric: D[V], final val minShift: Double, final val maxIterations: Int, final val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, final val customCenters: immutable.HashMap[Int, V])(implicit final val ctV: ClassTag[V], val sumVector: (V, V) => V, val getCenter: (V, Long) => V) extends KCentersAncestor[V, D[V], KCentersModel[V, D]] {

	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KCenters

	final def fit[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, V]])(implicit ct: ClassTag[Cz[O, V]]): KCentersModel[V, D] = KCentersModel[V, D](k, metric, minShift, maxIterations, persistanceLVL, obtainCenters(data))
}