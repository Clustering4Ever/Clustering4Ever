package org.clustering4ever.clustering.kcenters.rdd

import scala.language.higherKinds
import scala.math.pow
import scala.collection.immutable
import scala.util.Random
import scala.reflect.ClassTag
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.Dataset
import org.apache.spark.storage.StorageLevel
import org.apache.spark.sql.Encoders
import org.apache.spark.sql.SparkSession
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixedDistance}
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixedVector}
import org.clustering4ever.stats.Stats
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.clustering.kcenters.scala.KCentersModelCommons
import org.clustering4ever.util.{SumVectors, ClusterBasicOperations}
import org.clustering4ever.clustering.rdd.ClusteringModelDistributed
import org.clustering4ever.clustering.models.CenterModelDistributedCz
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
	protected[clustering] final def obtainClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, V]])(implicit ct: ClassTag[Cz[O, V]]): RDD[Cz[O, V]] = centerPredictCz(data)
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
	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KCenters
}
/**
 *
 */
final case class KMeansModel[D <: ContinuousDistance](final val k: Int, final val metric: D, final val minShift: Double, final val maxIterations: Int, final val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, final val centers: immutable.HashMap[Int, ScalarVector] = immutable.HashMap.empty[Int, ScalarVector]) extends KCentersModelAncestor[ScalarVector, D] {
	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KMeans
}
/**
 *
 */
final case class KModesModel[D <: BinaryDistance](final val k: Int, final val metric: D, final val minShift: Double, final val maxIterations: Int, final val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, final val centers: immutable.HashMap[Int, BinaryVector] = immutable.HashMap.empty[Int, BinaryVector]) extends KCentersModelAncestor[BinaryVector, D] {
	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KModes
}
/**
 *
 */
final case class KPrototypesModels[D <: MixedDistance](final val k: Int, final val metric: D, final val minShift: Double, final val maxIterations: Int, final val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, final val centers: immutable.HashMap[Int, MixedVector] = immutable.HashMap.empty[Int, MixedVector]) extends KCentersModelAncestor[MixedVector, D] {
	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KPrototypes
}