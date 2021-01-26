package org.clustering4ever.clustering.kfamily.kcenters.rdd

/**
 * @author Beck Gaël
 */
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.distances.BinaryDistance
import org.clustering4ever.roottraits
import org.clustering4ever.roottraits.{BinaryVector, Clusterizable, GVector}
import org.clustering4ever.sparkcoreextension.SparkImplicits._
import org.clustering4ever.util.{ClusterBasicOperations, VectorsAddOperationsImplicits}

import scala.collection.immutable
import scala.language.higherKinds
import scala.reflect.ClassTag
/**
 *
 */
final case class KModes[D <: BinaryDistance](final val k: Int, final val metric: D, final val minShift: Double, final val maxIterations: Int, final val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, final val customCenters: immutable.HashMap[Int, BinaryVector] = immutable.HashMap.empty[Int, BinaryVector])(protected implicit final val ctV: ClassTag[BinaryVector]) extends KCentersAncestor[BinaryVector, D, KModesModel[D]] {

	implicit val sumVector: (BinaryVector, BinaryVector) => BinaryVector = VectorsAddOperationsImplicits.addBinaryVectors

	implicit val getCenter: (BinaryVector, Long) => BinaryVector = (v, d) => ClusterBasicOperations.transformPreModeAndCastIt(v, d)

	final val algorithmID = roottraits.KModes

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