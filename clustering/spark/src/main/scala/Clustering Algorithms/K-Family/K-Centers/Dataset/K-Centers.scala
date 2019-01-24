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
trait KCentersAncestor[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D <: Distance[V], +Args <: KCentersArgsAncestor[V, D], +Model <: KCentersModelAncestor[ID, O, V, Cz, D, Args]] extends KCommonsSpark[V] with ClusteringAlgorithmDistributedDS[ID, O, V, Cz, Args, Model] {
	/**
	 * kryo Serialization if true, java one else
	 */
	val kryoSerialization: Boolean
	/**
	 * kryo Serialization if true, java one else
	 */
	private val encoder = if(kryoSerialization) Encoders.kryo[Cz[ID, O, V]] else Encoders.javaSerialization[Cz[ID, O, V]]
	/**
	 * kryo Serialization if true, java one else
	 */
	private val encoderInt = if(kryoSerialization) Encoders.kryo[Int] else Encoders.javaSerialization[Int]
	/**
	 *
	 */
	protected def obtainCenters(data: Dataset[Cz[ID, O, V]]): immutable.HashMap[Int, V] = {

		data.persist(args.persistanceLVL)

		def computeCenters(key: ClusterID, values: Iterator[Cz[ID, O, V]]): (ClusterID, Long, V) = {
				val agg = values.toBuffer
				val s = agg.size.toLong
				if(s <= 20000) (key, s, ClusterBasicOperations.obtainCenter(agg.map(_.v), args.metric))
				else (key, s, ClusterBasicOperations.obtainCenter(agg.par.map(_.v), args.metric))
		}

		val centers: immutable.HashMap[Int, V] = if(args.initializedCenters.isEmpty) kmppInitializationRDD(data.rdd.map(_.v), args.k, args.metric) else args.initializedCenters
		/**
		 * KCenters heart in tailrec style
		 */
		@annotation.tailrec
		def go(cpt: Int, allCentersHaveConverged: Boolean, centers: immutable.HashMap[Int, V]): immutable.HashMap[Int, V] = {
			if(cpt < args.maxIterations && !allCentersHaveConverged) {
				val centersInfo = data.groupByKey( cz => obtainNearestCenterID(cz.v, centers, args.metric) )(encoderInt)
					.mapGroups(computeCenters)(args.encoder)
					.collect

				val newCenters = immutable.HashMap(centersInfo.map{ case (clusterID, _, center) => (clusterID, center) }:_*)
				val newCardinalities = immutable.HashMap(centersInfo.map{ case (clusterID, cardinality, _) => (clusterID, cardinality) }:_*)
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
