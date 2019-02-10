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
	protected[clustering] def obtainClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, V]])(implicit ct: ClassTag[Cz[O, V]]): RDD[Cz[O, V]] = centerPredictCz(data)
	/**
	 *
	 */
	def prototypesDistancePerPoint[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, V]]): RDD[(Cz[O, V], immutable.HashMap[ClusterID, Double])] = {
		data.map( cz => (cz, centers.map{ case (clusterID, center) => (clusterID, metric.d(cz.v, center)) } ) )
	}
}
/**
 *
 */
case class KCentersModel[V <: GVector[V] : ClassTag, D[X <: GVector[X]] <: Distance[X]](val k: Int, val metric: D[V], val epsilon: Double, val maxIterations: Int, val persistanceLVL: StorageLevel, val centers: immutable.HashMap[Int, V]) extends KCentersModelAncestor[V, D[V]] {
	val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KCenters
}
/**
 *
 */
case class KMeansModel[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](val k: Int, val metric: D[V], val epsilon: Double, val maxIterations: Int, val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, val centers: immutable.HashMap[Int, ScalarVector[V]] = immutable.HashMap.empty[Int, ScalarVector[V]]) extends KCentersModelAncestor[ScalarVector[V], D[V]] {
	val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KMeans
}
/**
 *
 */
case class KModesModel[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X]](val k: Int, val metric: D[V], val epsilon: Double, val maxIterations: Int, val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, val centers: immutable.HashMap[Int, BinaryVector[V]] = immutable.HashMap.empty[Int, BinaryVector[V]]) extends KCentersModelAncestor[BinaryVector[V], D[V]] {
	val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KModes
}
/**
 *
 */
case class KPrototypesModels[Vb <: Seq[Int], Vs <: Seq[Double], D[X <: Seq[Int], Y <: Seq[Double]] <: MixedDistance[X, Y]](val k: Int, val metric: D[Vb, Vs], val epsilon: Double, val maxIterations: Int, val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, val centers: immutable.HashMap[Int, MixedVector[Vb, Vs]] = immutable.HashMap.empty[Int, MixedVector[Vb, Vs]]) extends KCentersModelAncestor[MixedVector[Vb, Vs], D[Vb, Vs]] {
	val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KPrototypes
}