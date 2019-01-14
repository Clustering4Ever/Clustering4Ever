package org.clustering4ever.spark.clustering.kcenters

import scala.language.higherKinds
import org.apache.spark.SparkContext
import scala.collection.JavaConverters._
import scala.math.pow
import scala.collection.mutable
import scala.util.Random
import scala.reflect.ClassTag
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixtVector}
import org.clustering4ever.stats.Stats
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.scala.clustering.kcenters.KCommons
import org.clustering4ever.util.{SumVectors, ClusterBasicOperations}
import org.clustering4ever.clustering.DistributedClusteringAlgorithm
import org.clustering4ever.clustering.ClusteringArgsDistributed
/**
 *
 */
trait KCentersArgsAncestor[V <: GVector[V], D <: Distance[V]] extends ClusteringArgsDistributed[V] {
	/**
	 *
	 */
	val k: Int
	/**
	 *
	 */
	val metric: D
	/**
	 *
	 */
	val epsilon: Double
	/**
	 *
	 */
	val maxIterations: Int
	/**
	 *
	 */
	val persistanceLVL: StorageLevel
	/**
	 *
	 */
	val initializedCenters: mutable.HashMap[Int, V]

}
/**
 *
 */
case class KCentersArgs[V <: GVector[V], D[X <: GVector[X]] <: Distance[X]](val k: Int, val metric: D[V], val epsilon: Double, val maxIterations: Int, val persistanceLVL: StorageLevel, val initializedCenters: mutable.HashMap[Int, V]) extends KCentersArgsAncestor[V, D[V]] {
	val algorithm = org.clustering4ever.extensibleAlgorithmNature.KCenters
}
/**
 *
 */
case class KMeansArgs[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](val k: Int, val metric: D[V], val epsilon: Double, val maxIterations: Int, val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, val initializedCenters: mutable.HashMap[Int, ScalarVector[V]] = mutable.HashMap.empty[Int, ScalarVector[V]]) extends KCentersArgsAncestor[ScalarVector[V], D[V]] {
	val algorithm = org.clustering4ever.extensibleAlgorithmNature.KMeans
}
/**
 *
 */
case class KModesArgs[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X]](val k: Int, val metric: D[V], val epsilon: Double, val maxIterations: Int, val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, val initializedCenters: mutable.HashMap[Int, BinaryVector[V]] = mutable.HashMap.empty[Int, BinaryVector[V]]) extends KCentersArgsAncestor[BinaryVector[V], D[V]] {
	val algorithm = org.clustering4ever.extensibleAlgorithmNature.KModes
}
/**
 *
 */
case class KPrototypesArgs[Vb <: Seq[Int], Vs <: Seq[Double], D[X <: Seq[Int], Y <: Seq[Double]] <: MixtDistance[X, Y]](val k: Int, val metric: D[Vb, Vs], val epsilon: Double, val maxIterations: Int, val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, val initializedCenters: mutable.HashMap[Int, MixtVector[Vb, Vs]] = mutable.HashMap.empty[Int, MixtVector[Vb, Vs]]) extends KCentersArgsAncestor[MixtVector[Vb, Vs], D[Vb, Vs]] {
	val algorithm = org.clustering4ever.extensibleAlgorithmNature.KPrototypes
}