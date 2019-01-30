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
	val encoder = if(kryoSerialization) Encoders.kryo[(Int, Long, V)] else Encoders.javaSerialization[(Int, Long, V)]
	/**
	 * kryo Serialization if true, java one else
	 */
	private val encoderInt = if(kryoSerialization) Encoders.kryo[Int] else Encoders.javaSerialization[Int]
	/**
	 *
	 */
	protected def obtainCenters[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: Dataset[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): immutable.HashMap[Int, V] = {
		/**
		 * kryo Serialization if true, java one else
		 */
		// val encoder = if(kryoSerialization) Encoders.kryo[Cz[ID, O, V]] else Encoders.javaSerialization[Cz[ID, O, V]]

		data.persist(persistanceLVL)

		def computeCenters(key: ClusterID, values: Iterator[Cz[ID, O, V]]): (ClusterID, Long, V) = {
				val agg = values.toBuffer
				val s = agg.size.toLong
				if(s <= 20000) (key, s, ClusterBasicOperations.obtainCenter(agg.map(_.v), metric))
				else (key, s, ClusterBasicOperations.obtainCenter(agg.par.map(_.v), metric))
		}

		val centers: immutable.HashMap[Int, V] = if(customCenters.isEmpty) kmppInitializationRDD(data.rdd.map(_.v), k, metric) else customCenters
		/**
		 * KCenters heart in tailrec style
		 */
		@annotation.tailrec
		def go(cpt: Int, haveAllCentersConverged: Boolean, centers: immutable.HashMap[Int, V]): immutable.HashMap[Int, V] = {
			val centersInfo = data.groupByKey( cz => obtainNearestCenterID(cz.v, centers, metric) )(encoderInt)
				.mapGroups(computeCenters)(encoder)
				.collect
			val newCenters = immutable.HashMap(centersInfo.map{ case (clusterID, _, center) => (clusterID, center) }:_*)
			val newCardinalities = immutable.HashMap(centersInfo.map{ case (clusterID, cardinality, _) => (clusterID, cardinality) }:_*)
			val (newCentersPruned, newKCentersBeforUpdatePruned) = removeEmptyClusters(newCenters, centers, newCardinalities)
			val shiftingEnough = areCentersNotMovingEnough(newKCentersBeforUpdatePruned, newCentersPruned, epsilon, metric)
			if(cpt < maxIterations && !shiftingEnough) {
				go(cpt + 1, shiftingEnough, newCentersPruned)
			}
			else {
				newCentersPruned.zipWithIndex.map{ case ((oldClusterID, center), newClusterID) => (newClusterID, center) }
			}
		}
		go(0, false, centers)
	}
}
