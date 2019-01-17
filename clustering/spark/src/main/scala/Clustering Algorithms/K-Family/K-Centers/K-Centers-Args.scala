package org.clustering4ever.spark.clustering.kcenters

import scala.language.higherKinds
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
import org.clustering4ever.clustering.ClusteringAlgorithmDistributed
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
case class KCentersArgs[V <: GVector[V] : ClassTag, D[X <: GVector[X]] <: Distance[X]](val k: Int, val metric: D[V], val epsilon: Double, val maxIterations: Int, val persistanceLVL: StorageLevel, val initializedCenters: mutable.HashMap[Int, V]) extends KCentersArgsAncestor[V, D[V]] {
	
	val algorithm = org.clustering4ever.extensibleAlgorithmNature.KCenters
	/**
	 *
	 */
	def obtainAlgorithm[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: RDD[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): KCenters[ID, O, V, Cz, D] = {
		KCenters[ID, O, V, Cz, D](this)
	}
}
/**
 *
 */
case class KMeansArgs[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](val k: Int, val metric: D[V], val epsilon: Double, val maxIterations: Int, val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, val initializedCenters: mutable.HashMap[Int, ScalarVector[V]] = mutable.HashMap.empty[Int, ScalarVector[V]]) extends KCentersArgsAncestor[ScalarVector[V], D[V]] {
	
	val algorithm = org.clustering4ever.extensibleAlgorithmNature.KMeans
	/**
	 *
	 */
	def obtainAlgorithm[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: RDD[Cz[ID, O, ScalarVector[V]]])(implicit ct: ClassTag[Cz[ID, O, ScalarVector[V]]]): KMeans[ID, O, V, Cz, D] = {
		KMeans[ID, O, V, Cz, D](this)
	}
}
/**
 *
 */
case class KModesArgs[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X]](val k: Int, val metric: D[V], val epsilon: Double, val maxIterations: Int, val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, val initializedCenters: mutable.HashMap[Int, BinaryVector[V]] = mutable.HashMap.empty[Int, BinaryVector[V]]) extends KCentersArgsAncestor[BinaryVector[V], D[V]] {
	
	val algorithm = org.clustering4ever.extensibleAlgorithmNature.KModes
	/**
	 *
	 */
	def obtainAlgorithm[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: RDD[Cz[ID, O, BinaryVector[V]]])(implicit ct: ClassTag[Cz[ID, O, BinaryVector[V]]]): KModes[ID, O, V, Cz, D] = {
		KModes[ID, O, V, Cz, D](this)
	}
}
/**
 *
 */
case class KPrototypesArgs[Vb <: Seq[Int], Vs <: Seq[Double], D[X <: Seq[Int], Y <: Seq[Double]] <: MixtDistance[X, Y]](val k: Int, val metric: D[Vb, Vs], val epsilon: Double, val maxIterations: Int, val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, val initializedCenters: mutable.HashMap[Int, MixtVector[Vb, Vs]] = mutable.HashMap.empty[Int, MixtVector[Vb, Vs]]) extends KCentersArgsAncestor[MixtVector[Vb, Vs], D[Vb, Vs]] {
	
	val algorithm = org.clustering4ever.extensibleAlgorithmNature.KPrototypes
	/**
	 *
	 */
	def obtainAlgorithm[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: RDD[Cz[ID, O, MixtVector[Vb, Vs]]])(implicit ct: ClassTag[Cz[ID, O, MixtVector[Vb, Vs]]]): KPrototypes[ID, O, Vb, Vs, Cz, D] = {
		KPrototypes[ID, O, Vb, Vs, Cz, D](this)
	}

}