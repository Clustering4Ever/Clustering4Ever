package org.clustering4ever.clustering.kcenters.dataset
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
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.Encoders
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.stats.Stats
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.clustering.kcenters.scala.KCommons
import org.clustering4ever.util.ClusterBasicOperations
import org.clustering4ever.clustering.dataset.ClusteringAlgorithmDistributedDS
import org.clustering4ever.vectors.GVector
import org.clustering4ever.clustering.kcenters.rdd.KCommonsSpark
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
	val encoder = if(kryoSerialization) Encoders.kryo[(Int, V)] else Encoders.javaSerialization[(Int, V)]
	/**
	 * kryo Serialization if true, java one else
	 */
	private val encoderInt = if(kryoSerialization) Encoders.kryo[Int] else Encoders.javaSerialization[Int]
	/**
	 *
	 */
	protected def obtainCenters[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: Dataset[Cz[O, V]])(implicit ct: ClassTag[Cz[O, V]]): immutable.HashMap[Int, V] = {
		/**
		 * kryo Serialization if true, java one else
		 */
		// val encoder = if(kryoSerialization) Encoders.kryo[Cz[O, V]] else Encoders.javaSerialization[Cz[O, V]]

		data.persist(persistanceLVL)

		def computeCenters(key: ClusterID, values: Iterator[Cz[O, V]]): (ClusterID, V) = {
				val agg = values.toBuffer
				val s = agg.size.toLong
				if(s <= 20000) (key, ClusterBasicOperations.obtainCenter(agg.map(_.v), metric))
				else (key, ClusterBasicOperations.obtainCenter(agg.par.map(_.v), metric))
		}

		val centers: immutable.HashMap[Int, V] = if(customCenters.isEmpty) kmppInitializationRDD(data.rdd.map(_.v), k, metric) else customCenters
		/**
		 * KCenters heart in tailrec style
		 */
		@annotation.tailrec
		def go(cpt: Int, haveAllCentersConverged: Boolean, centers: immutable.HashMap[Int, V]): immutable.HashMap[Int, V] = {
			val updatedCenters = immutable.HashMap(
				data.groupByKey( cz => obtainNearestCenterID(cz.v, centers, metric) )(encoderInt)
					.mapGroups(computeCenters)(encoder)
					.collect
				:_*)
			val shiftingEnough = areCentersNotMovingEnough(updatedCenters, centers, epsilon, metric)
			if(cpt < maxIterations && !shiftingEnough) {
				go(cpt + 1, shiftingEnough, updatedCenters)
			}
			else {
				updatedCenters.zipWithIndex.map{ case ((oldClusterID, center), newClusterID) => (newClusterID, center) }
			}
		}
		go(0, false, centers)
	}
}
