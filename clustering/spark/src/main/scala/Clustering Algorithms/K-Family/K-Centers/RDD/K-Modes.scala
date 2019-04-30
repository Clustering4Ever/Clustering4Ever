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
final case class KModes[D <: BinaryDistance](final val k: Int, final val metric: D, final val minShift: Double, final val maxIterations: Int, final val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, final val customCenters: immutable.HashMap[Int, BinaryVector] = immutable.HashMap.empty[Int, BinaryVector])(protected implicit final val ctV: ClassTag[BinaryVector]) extends KCentersAncestor[BinaryVector, D, KModesModel[D]] {

	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KModes

	final def fit[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, BinaryVector]])(implicit ct: ClassTag[Cz[O, BinaryVector]]): KModesModel[D] = KModesModel[D](k, metric, minShift, maxIterations, persistanceLVL, obtainCenters(data))
}
/**
 *
 */
object KModes {
	/**
	 * Run the K-Modes with any binary distance
	 */
	final def fit[D <: BinaryDistance](
		data: RDD[Array[Int]],
		k: Int,
		metric: D,
		minShift: Double,
		maxIterations: Int,
		persistanceLVL: StorageLevel
	): KModesModel[D] = {
		KModes(k, metric, minShift, maxIterations, persistanceLVL).fit(binaryDataWithIDToClusterizable(data.zipWithIndex))
	}
}