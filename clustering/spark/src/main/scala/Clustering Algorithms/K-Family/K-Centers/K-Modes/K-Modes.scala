package org.clustering4ever.spark.clustering.kmodes
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
import org.clustering4ever.scala.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.util.SparkImplicits._
import org.clustering4ever.spark.clustering.kcenters.{KCentersModel, KCenters}
import org.clustering4ever.scala.vectors.{GVector, BinaryVector}
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
		Cz[X, Y, Z <: GVector] <: Clusterizable[X, Y, Z, Cz],
		D <: BinaryDistance[V]
	](
		data: RDD[Cz[ID, O, BinaryVector[V]]],
		k: Int,
		metric: D,
		epsilon: Double,
		maxIterations: Int,
		persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY,
		initializedCenters: mutable.HashMap[Int, BinaryVector[V]] = mutable.HashMap.empty[Int, BinaryVector[V]]
	)(implicit ct: ClassTag[Cz[ID, O, BinaryVector[V]]]): KCentersModel[ID, O, BinaryVector[V], Cz, D] = {
		val kmodes = new KCenters(k, metric, epsilon, maxIterations, persistanceLVL, initializedCenters)
		val kModesModel = kmodes.run(data)
		kModesModel
	}
	/**
	 * Run the K-Modes with any binary distance
	 */
	def runRawData[V <: Seq[Int], D <: BinaryDistance[V]](
		data: RDD[V],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D,
		persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY
	): KCentersModel[Long, BinaryVector[V], BinaryVector[V], EasyClusterizable, D] = {
		val kModesModel = run(binaryDataWithIDToClusterizable(data.zipWithIndex), k, metric, epsilon, maxIterations, persistanceLVL)
		kModesModel
	}
}