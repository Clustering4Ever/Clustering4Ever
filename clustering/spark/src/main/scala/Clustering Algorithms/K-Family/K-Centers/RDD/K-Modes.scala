package org.clustering4ever.clustering.kcenters.rdd
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.immutable
import scala.util.Random
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.math.distances.{Distance, BinaryDistance}
import org.clustering4ever.math.distances.binary.Hamming
import org.clustering4ever.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.util.SparkImplicits._
import org.clustering4ever.vectors.{GVector, BinaryVector}
/**
 *
 */
case class KModes[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X]](val k: Int, val metric: D[V], val epsilon: Double, val maxIterations: Int, val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, val customCenters: immutable.HashMap[Int, BinaryVector[V]] = immutable.HashMap.empty[Int, BinaryVector[V]])(protected implicit val ctV: ClassTag[BinaryVector[V]]) extends KCentersAncestor[BinaryVector[V], D[V], KModesModel[V, D]] {
	/**
	 *
	 */
	def run[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, BinaryVector[V]]])(implicit ct: ClassTag[Cz[O, BinaryVector[V]]]): KModesModel[V, D] = KModesModel[V, D](k, metric, epsilon, maxIterations, persistanceLVL, obtainCenters(data))
}
/**
 *
 */
object KModes {
	/**
	 * Run the K-Modes with any binary distance
	 */
	def run[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X]](
		data: RDD[V],
		k: Int,
		metric: D[V],
		epsilon: Double,
		maxIterations: Int,
		persistanceLVL: StorageLevel
	): KModesModel[V, D] = {
		KModes(k, metric, epsilon, maxIterations, persistanceLVL).run(binaryDataWithIDToClusterizable(data.zipWithIndex))
	}
}