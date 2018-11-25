package org.clustering4ever.spark.clustering.kmodes
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{mutable, GenSeq}
import scala.util.Random
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.math.distances.binary.Hamming
import org.clustering4ever.math.distances.mixt.HammingAndEuclidean
import org.clustering4ever.scala.measurableclass.BinaryScalarVector
import org.clustering4ever.scala.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.util.SparkImplicits._
import org.apache.spark.rdd.RDD
import org.clustering4ever.spark.clustering.kcenters.{KCentersModel, KCenters}
/**
 *
 */
object KModes {
	/**
	 * Run the K-Modes with any binary distance
	 */
	def run[
		ID: Numeric,
		O,
		V[Int] <: Seq[Int],
		Cz[ID, O, V] <: Clusterizable[ID, O, V, Cz[ID, O, V]],
		D <: BinaryDistance[V[Int]]
	](
		data: RDD[Cz[ID, O, V[Int]]],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D,
		initializedCenters: mutable.HashMap[Int, V[Int]] = mutable.HashMap.empty[Int, V[Int]]
	)(implicit ct1: ClassTag[V[Int]], ct2: ClassTag[Cz[ID, O, V[Int]]], workingVector: Int = 0): KCentersModel[ID, O, V[Int], Cz[ID, O, V[Int]], D] = {
		val kmodes = new KCenters[ID, O, V[Int], Cz[ID, O, V[Int]], D](k, epsilon, maxIterations, metric, initializedCenters)
		val kModesModel = kmodes.run(data)(workingVector)
		kModesModel
	}
}
/**
 *
 */
object EasyKModes {
	/**
	 * Run the K-Modes with any binary distance
	 */
	def run[V[Int] <: Seq[Int], D <: BinaryDistance[V[Int]]](
		data: RDD[V[Int]],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D,
		initializedCenters: mutable.HashMap[Int, V[Int]] = mutable.HashMap.empty[Int, V[Int]]
	)(implicit ct: ClassTag[V[Int]], workingVector: Int = 0): KCentersModel[Long, V[Int], V[Int], EasyClusterizable[Long, V[Int], V[Int]], D] = {
		val kmodes = new KCenters[Long, V[Int], V[Int], EasyClusterizable[Long, V[Int], V[Int]], D](k, epsilon, maxIterations, metric, initializedCenters)
		val kModesModel = kmodes.run(data)(workingVector)
		kModesModel
	}
}