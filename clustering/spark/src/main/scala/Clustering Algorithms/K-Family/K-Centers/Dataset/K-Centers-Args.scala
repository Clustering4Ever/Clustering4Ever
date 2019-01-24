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
import org.clustering4ever.clustering.dataset.ClusteringArgsDistributedDS
import org.clustering4ever.clustering.kcenters.rdd.KCentersArgsSuperAncestor
/**
 *
 */
trait KCentersArgsAncestor[V <: GVector[V], D <: Distance[V]] extends KCentersArgsSuperAncestor[V, D] with ClusteringArgsDistributedDS[V] {
	/**
	 * kryo Serialization if true, java one else
	 */
	val encoder = if(kryoSerialization) Encoders.kryo[(Int, Long, V)] else Encoders.javaSerialization[(Int, Long, V)]
}
/**
 *
 */
case class KMeansArgs[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](val k: Int, val metric: D[V], val epsilon: Double, val maxIterations: Int, val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, val kryoSerialization: Boolean = false, val initializedCenters: immutable.HashMap[Int, ScalarVector[V]] = immutable.HashMap.empty[Int, ScalarVector[V]]) extends KCentersArgsAncestor[ScalarVector[V], D[V]] {
	
	val algorithm = org.clustering4ever.extensibleAlgorithmNature.KMeans
	/**
	 *
	 */
	def obtainAlgorithm[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: Dataset[Cz[ID, O, ScalarVector[V]]])(implicit ct: ClassTag[Cz[ID, O, ScalarVector[V]]]): KMeans[ID, O, V, Cz, D] = {
		KMeans[ID, O, V, Cz, D](this)
	}
}