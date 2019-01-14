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
case class KModes[ID, O, V <: Seq[Int], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Int]] <: BinaryDistance[X]](val args: KModesArgs[V, D])(implicit val ct: ClassTag[Cz[ID, O, BinaryVector[V]]], val ctV: ClassTag[BinaryVector[V]]) extends KCentersAncestor[ID, O, BinaryVector[V], Cz, D[V], KModesArgs[V, D], KModesModel[ID, O, V, Cz, D]] {
	/**
	 *
	 */
	def run(data: RDD[Cz[ID, O, BinaryVector[V]]]): KModesModel[ID, O, V, Cz, D] = KModesModel[ID, O, V, Cz, D](obtainCenters(data), args.metric)
}
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
		D[X <: Seq[Int]] <: BinaryDistance[X]
	](
		data: RDD[Cz[ID, O, BinaryVector[V]]],
		k: Int,
		metric: D[V],
		epsilon: Double,
		maxIterations: Int,
		persistanceLVL: StorageLevel,
		initializedCenters: mutable.HashMap[Int, BinaryVector[V]] = mutable.HashMap.empty[Int, BinaryVector[V]]
	)(implicit ct: ClassTag[Cz[ID, O, BinaryVector[V]]]): KModesModel[ID, O, V, Cz, D] = {
		val kmodes = KModes[ID, O, V, Cz, D](KModesArgs[V, D](k, metric, epsilon, maxIterations, persistanceLVL, initializedCenters))
		val kModesModel = kmodes.run(data)
		kModesModel
	}
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
	): KModesModel[Long, BinaryVector[V], V, EasyClusterizable, D] = {
		val kModesModel = run(binaryDataWithIDToClusterizable(data.zipWithIndex), k, metric, epsilon, maxIterations, persistanceLVL)
		kModesModel
	}
}