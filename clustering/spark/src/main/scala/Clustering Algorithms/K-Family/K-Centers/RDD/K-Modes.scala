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
final case class KModes[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X]](final val k: Int, final val metric: D[V], final val minShift: Double, final val maxIterations: Int, final val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, final val customCenters: immutable.HashMap[Int, BinaryVector[V]] = immutable.HashMap.empty[Int, BinaryVector[V]])(protected implicit final val ctV: ClassTag[BinaryVector[V]]) extends KCentersAncestor[BinaryVector[V], D[V], KModesModel[V, D]] {

	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KModes

	final def fit[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, BinaryVector[V]]])(implicit ct: ClassTag[Cz[O, BinaryVector[V]]]): KModesModel[V, D] = KModesModel[V, D](k, metric, minShift, maxIterations, persistanceLVL, obtainMedians(data))
}
/**
 *
 */
object KModes {
	/**
	 * Run the K-Modes with any binary distance
	 */
	final def fit[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X]](
		data: RDD[V],
		k: Int,
		metric: D[V],
		minShift: Double,
		maxIterations: Int,
		persistanceLVL: StorageLevel
	): KModesModel[V, D] = {
		KModes(k, metric, minShift, maxIterations, persistanceLVL).fit(binaryDataWithIDToClusterizable(data.zipWithIndex))
	}
}