package org.clustering4ever.spark.clustering.kcenters
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{mutable, GenSeq}
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
object KModes {
	/**
	 * Run the K-Modes with any binary distance
	 */
	def run[
		ID,
		O,
		V <: Seq[Int],
		Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
		D <: BinaryDistance[V]
	](
		data: RDD[Cz[ID, O, BinaryVector[V]]],
		k: Int,
		metric: D,
		epsilon: Double,
		maxIterations: Int,
		persistanceLVL: StorageLevel,
		initializedCenters: mutable.HashMap[Int, BinaryVector[V]] = mutable.HashMap.empty[Int, BinaryVector[V]]
	)(implicit ct: ClassTag[Cz[ID, O, BinaryVector[V]]]): KCentersModel[BinaryVector[V], D] = {
		val kmodes = new KCenters[BinaryVector[V], D](new KCentersArgs[BinaryVector[V], D](k, metric, epsilon, maxIterations, persistanceLVL, initializedCenters))
		val kModesModel = kmodes.run(data)
		kModesModel
	}
	/**
	 * Run the K-Modes with any binary distance
	 */
	def run[V <: Seq[Int], D <: BinaryDistance[V]](
		data: RDD[V],
		k: Int,
		metric: D,
		epsilon: Double,
		maxIterations: Int,
		persistanceLVL: StorageLevel
	): KCentersModel[BinaryVector[V], D] = {
		val kModesModel = run(binaryDataWithIDToClusterizable(data.zipWithIndex), k, metric, epsilon, maxIterations, persistanceLVL)
		kModesModel
	}
}