package org.clustering4ever.clustering.kfamily.kcenters.dataset

/**
 * @author Beck Gaël
 */
import org.apache.spark.sql.{Dataset, Encoders}
import org.clustering4ever.clustering.kfamily.kcenters.rdd.KCommonsSpark
import org.clustering4ever.distances.Distance
import org.clustering4ever.roottraits.{Clusterizable, GVector}
import org.clustering4ever.sparkcoreextension.ClusteringAlgorithmDistributedDS
import org.clustering4ever.util.ClusterBasicOperations

import scala.collection.immutable
import scala.language.higherKinds
import scala.reflect.ClassTag
/**
 *
 */
trait KCentersAncestor[V <: GVector[V], D <: Distance[V], CA <: KCentersModelAncestor[V, D]] extends KCommonsSpark[V, D] with ClusteringAlgorithmDistributedDS[V, CA] {
	/**
	 *
	 */
	val kryoSerialization: Boolean
	/**
	 * kryo Serialization if true, java one else
	 */
	final val encoder = if(kryoSerialization) Encoders.kryo[(Int, V)] else Encoders.javaSerialization[(Int, V)]
	/**
	 * kryo Serialization if true, java one else
	 */
	private final val encoderInt = if(kryoSerialization) Encoders.kryo[Int] else Encoders.javaSerialization[Int]
	/**
	 *
	 */
	protected final def obtainCenters[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: Dataset[Cz[O, V]])(implicit ct: ClassTag[Cz[O, V]]): immutable.HashMap[Int, V] = {

		data.persist(persistanceLVL)

		def computeCenters(key: ClusterID, values: Iterator[Cz[O, V]]): (ClusterID, V) = {
				val agg = values.toBuffer
				val s = agg.size.toLong
				if(s <= 20000) (key, ClusterBasicOperations.obtainCenter(agg.map(_.v), metric))
				else (key, ClusterBasicOperations.obtainCenter(agg.par.map(_.v), metric))
		}

		val unSortedCenters = if (customCenters.isEmpty) {
			kmppInitializationRDD(data.rdd.map(_.v), k, metric)
		}
		else {
			customCenters
		}

		val centers = unSortedCenters.toList.sortBy(_._1)
		/**
		 * KCenters heart in tailrec style
		 */
		@annotation.tailrec
		def go(cpt: Int, haveAllCentersConverged: Boolean, centers: List[(Int, V)]): List[(Int, V)] = {
			val preUpdatedCenters = data.groupByKey( cz => obtainNearestCenterID(cz.v, centers, metric) )(encoderInt)
				.mapGroups(computeCenters)(encoder)
				.collect
				.sortBy(_._1)
				.toList
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
