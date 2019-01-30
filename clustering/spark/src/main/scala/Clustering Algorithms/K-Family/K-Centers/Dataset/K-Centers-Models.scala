package org.clustering4ever.clustering.kcenters.dataset

import scala.language.higherKinds
import scala.math.pow
import scala.collection.immutable
import scala.util.Random
import scala.reflect.ClassTag
import org.apache.spark.sql.Dataset
import org.apache.spark.storage.StorageLevel
import org.apache.spark.sql.Encoders
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixtVector}
import org.clustering4ever.stats.Stats
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.clustering.kcenters.scala.KCommons
import org.clustering4ever.util.ClusterBasicOperations
import org.clustering4ever.clustering.dataset.ClusteringModelDistributedDS
import org.clustering4ever.clustering.kcenters.rdd.KCentersModelSuperAncestor
/**
 *
 */
trait KCentersModelAncestor[V <: GVector[V], D <: Distance[V]] extends KCentersModelSuperAncestor[V, D] with ClusteringModelDistributedDS[V] {
	/**
	 *
	 */
	def obtainClustering[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: Dataset[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): Dataset[Cz[ID, O, V]] = {
		/**
		 * kryo Serialization if true, java one else
		 */
		val czEncoder = if(kryoSerialization) Encoders.kryo[Cz[ID, O, V]] else Encoders.javaSerialization[Cz[ID, O, V]]
	
		data.map( cz => cz.addClusterIDs(centerPredict(cz.v)) )(czEncoder)
	}
	/**
	 *
	 */
	def prototypesDistancePerPoint[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: Dataset[Cz[ID, O, V]]): Dataset[(Cz[ID, O, V], immutable.HashMap[ClusterID, Double])] = {
		
		val speEncoder = if(kryoSerialization) Encoders.kryo[(Cz[ID, O, V], immutable.HashMap[ClusterID, Double])] else Encoders.javaSerialization[(Cz[ID, O, V], immutable.HashMap[ClusterID, Double])]

		data.map( cz => (cz, centers.map{ case (clusterID, center) => (clusterID, metric.d(cz.v, center)) } ) )(speEncoder)
	}
}
/**
 *
 */
case class KMeansModel[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](val k: Int, val metric: D[V], val epsilon: Double, val maxIterations: Int, val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, val kryoSerialization: Boolean = false, val centers: immutable.HashMap[Int, ScalarVector[V]] = immutable.HashMap.empty[Int, ScalarVector[V]]) extends KCentersModelAncestor[ScalarVector[V], D[V]]