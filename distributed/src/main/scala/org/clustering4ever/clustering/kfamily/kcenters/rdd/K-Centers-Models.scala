package org.clustering4ever.clustering.kfamily.kcenters.rdd

import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.clustering.kfamily.kcenters.KCentersModelCommons
import org.clustering4ever.distances.{BinaryDistance, ContinuousDistance, Distance, MixedDistance}
import org.clustering4ever.roottraits._
import org.clustering4ever.sparkcoreextension.{CenterModelDistributedCz, ClusteringModelDistributed}

import scala.collection.immutable
import scala.language.higherKinds
import scala.reflect.ClassTag
/**
 *
 */
trait KCentersModelSuperAncestor[V <: GVector[V], D <: Distance[V]] extends KCentersModelCommons[V, D] {
	/**
	 *
	 */
	val persistanceLVL: StorageLevel
}
/**
 *
 */
trait KCentersModelAncestor[V <: GVector[V], D <: Distance[V]] extends KCentersModelSuperAncestor[V, D] with ClusteringModelDistributed[V] with CenterModelDistributedCz[V, D] {
	/**
	 *
	 */
	protected[clustering4ever] final def obtainClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, V]])(implicit ct: ClassTag[Cz[O, V]]): RDD[Cz[O, V]] = centerPredictCz(data)
	/**
	 *
	 */
	final def prototypesDistancePerPoint[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, V]]): RDD[(Cz[O, V], immutable.HashMap[ClusterID, Double])] = {
		data.map( cz => (cz, centers.map{ case (clusterID, center) => (clusterID, metric.d(cz.v, center)) } ) )
	}
}
/**
 *
 */
final case class KCentersModel[V <: GVector[V] : ClassTag, D[X <: GVector[X]] <: Distance[X]](final val k: Int, final val metric: D[V], final val minShift: Double, final val maxIterations: Int, final val persistanceLVL: StorageLevel, final val centers: immutable.HashMap[Int, V]) extends KCentersModelAncestor[V, D[V]] {
	final val algorithmID = org.clustering4ever.roottraits.KCenters
}
/**
 *
 */
final case class KMeansModel[D <: ContinuousDistance](final val k: Int, final val metric: D, final val minShift: Double, final val maxIterations: Int, final val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, final val centers: immutable.HashMap[Int, ScalarVector] = immutable.HashMap.empty[Int, ScalarVector]) extends KCentersModelAncestor[ScalarVector, D] {
	final val algorithmID = org.clustering4ever.roottraits.KMeans
}
/**
 *
 */
final case class KModesModel[D <: BinaryDistance](final val k: Int, final val metric: D, final val minShift: Double, final val maxIterations: Int, final val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, final val centers: immutable.HashMap[Int, BinaryVector] = immutable.HashMap.empty[Int, BinaryVector]) extends KCentersModelAncestor[BinaryVector, D] {
	final val algorithmID = org.clustering4ever.roottraits.KModes
}
/**
 *
 */
final case class KPrototypesModels[D <: MixedDistance](final val k: Int, final val metric: D, final val minShift: Double, final val maxIterations: Int, final val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, final val centers: immutable.HashMap[Int, MixedVector] = immutable.HashMap.empty[Int, MixedVector]) extends KCentersModelAncestor[MixedVector, D] {
	final val algorithmID = org.clustering4ever.roottraits.KPrototypes
}