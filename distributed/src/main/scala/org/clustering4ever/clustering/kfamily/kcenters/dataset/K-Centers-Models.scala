package org.clustering4ever.clustering.kfamily.kcenters.dataset

import org.apache.spark.sql.{Dataset, Encoders}
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.clustering.kfamily.kcenters.rdd.KCentersModelSuperAncestor
import org.clustering4ever.distances.{ContinuousDistance, Distance}
import org.clustering4ever.roottraits.{Clusterizable, GVector, ScalarVector}
import org.clustering4ever.sparkcoreextension.ClusteringModelDistributedDS

import scala.collection.immutable
import scala.language.higherKinds
import scala.reflect.ClassTag
/**
 *
 */
trait KCentersModelAncestor[V <: GVector[V], D <: Distance[V]] extends KCentersModelSuperAncestor[V, D] with ClusteringModelDistributedDS[V] {
	/**
	 *
	 */
	protected[clustering4ever] final def obtainClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: Dataset[Cz[O, V]])(implicit ct: ClassTag[Cz[O, V]]): Dataset[Cz[O, V]] = {
		/**
		 * kryo Serialization if true, java one else
		 */
		val czEncoder = if(kryoSerialization) Encoders.kryo[Cz[O, V]] else Encoders.javaSerialization[Cz[O, V]]
	
		data.map( cz => cz.addClusterIDs(centerPredict(cz.v)) )(czEncoder)
	}
	/**
	 *
	 */
	final def prototypesDistancePerPoint[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: Dataset[Cz[O, V]]): Dataset[(Cz[O, V], immutable.HashMap[ClusterID, Double])] = {
		
		val speEncoder = if(kryoSerialization) Encoders.kryo[(Cz[O, V], immutable.HashMap[ClusterID, Double])] else Encoders.javaSerialization[(Cz[O, V], immutable.HashMap[ClusterID, Double])]

		data.map( cz => (cz, centers.map{ case (clusterID, center) => (clusterID, metric.d(cz.v, center)) } ) )(speEncoder)
	}
}
/**
 *
 */
final case class KMeansModel[D <: ContinuousDistance](final val k: Int, final val metric: D, final val minShift: Double, final val maxIterations: Int, final val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, final val kryoSerialization: Boolean = false, final val centers: immutable.HashMap[Int, ScalarVector] = immutable.HashMap.empty[Int, ScalarVector]) extends KCentersModelAncestor[ScalarVector, D] {
	final val algorithmID = org.clustering4ever.roottraits.KMeans
}