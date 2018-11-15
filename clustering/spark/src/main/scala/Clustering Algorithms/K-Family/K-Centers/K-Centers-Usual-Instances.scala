package org.clustering4ever.spark.clustering.kcenters
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
import org.clustering4ever.util.ClusterBasicOperations
import org.clustering4ever.stats.Stats
import org.clustering4ever.scala.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.util.SparkImplicits._
import org.apache.spark.rdd.RDD
/**
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : preferably and ArrayBuffer or ParArray of Clusterizable descendant, the SimpleRealClusterizable is the basic reference with mutable.ArrayBuffer as vector type, they are recommendend for speed efficiency
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param maxIterations : maximal number of iteration
 * @param metric : a defined dissimilarity measure
 */
object KMeans {
	/**
	 * Run the K-Means with any continuous distance
	 */
	def run[
		ID: Numeric,
		O,
		V[Double] <: Seq[Double],
		Cz[ID, O, V <: Seq[Double]] <: Clusterizable[ID, O, V, Cz[ID, O, V]],
		D <: ContinuousDistance[V[Double]]
	](
		data: RDD[Cz[ID, O, V[Double]]],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D,
		initializedCenters: mutable.HashMap[Int, V[Double]] = mutable.HashMap.empty[Int, V[Double]]
		)(implicit ct: ClassTag[V[Double]], ct2: ClassTag[Cz[ID, O, V[Double]]], workingVector: Int = 0): KCentersModel[ID, O, V[Double], Cz[ID, O, V[Double]], D] = {
		val kMeans = new KCenters[ID, O, V[Double], Cz[ID, O, V[Double]], D](k, epsilon, maxIterations, metric, initializedCenters)
		val kCentersModel = kMeans.run(data)(workingVector)
		kCentersModel
	}
}
/**
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : preferably and ArrayBuffer or ParArray of any sequence type mutable.ArrayBuffer are recommendend for speed efficiency
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param maxIterations : maximal number of iteration
 * @param metric : a defined dissimilarity measure
 */
object EasyKMeans {
	/**
	 * Run the K-Means with any continuous distance
	 */
	def run[V <: Seq[Double], D <: ContinuousDistance[V]](
		data: RDD[V],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D,
		initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
		)(implicit ct: ClassTag[V], workingVector: Int = 0) : KCentersModel[Long, V, V, EasyClusterizable[Long, V, V], D] = {
		val kMeans = new KCenters[Long, V, V, EasyClusterizable[Long, V, V], D](k, epsilon, maxIterations, metric, initializedCenters)
		val kCentersModel = kMeans.run(data)(workingVector)
		kCentersModel
	}
}
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
		Cz[ID, O, V <: Seq[Int]] <: Clusterizable[ID, O, V, Cz[ID, O, V]],
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
	def run[V <: Seq[Int], D <: BinaryDistance[V]](
		data: RDD[V],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D,
		initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
	)(implicit ct: ClassTag[V], workingVector: Int = 0): KCentersModel[Long, V, V, EasyClusterizable[Long, V, V], D] = {
		val kmodes = new KCenters[Long, V, V, EasyClusterizable[Long, V, V], D](k, epsilon, maxIterations, metric, initializedCenters)
		val kModesModel = kmodes.run(data)(workingVector)
		kModesModel
	}
}
/**
 * The famous K-Prototypes using a user-defined dissmilarity measure.
 * @param data :
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param maxIterations : maximal number of iteration
 * @param metric : a defined dissimilarity measure
 */
object KPrototypes {
	/**
	 * Run the K-Prototypes with any mixt distance
	 */
	def run[
		ID: Numeric,
		O,
		Vb[Int] <: Seq[Int],
		Vs[Double] <: Seq[Double],
		Cz[ID, O] <: Clusterizable[ID, O, BinaryScalarVector[Vb[Int], Vs[Double]], Cz[ID, O]],
		D <: MixtDistance[Vb[Int], Vs[Double]]
	](
		data: RDD[Cz[ID, O]],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D,
		initializedCenters: mutable.HashMap[Int, BinaryScalarVector[Vb[Int], Vs[Double]]] = mutable.HashMap.empty[Int, BinaryScalarVector[Vb[Int], Vs[Double]]]
	)(implicit ct: ClassTag[Cz[ID, O]], workingVector: Int = 0): KCentersModel[ID, O, BinaryScalarVector[Vb[Int], Vs[Double]], Cz[ID, O], D] = {
		val kPrototypes = new KCenters[ID, O, BinaryScalarVector[Vb[Int], Vs[Double]], Cz[ID, O], D](k, epsilon, maxIterations, metric)
		val kPrototypesModel = kPrototypes.run(data)(workingVector)
		kPrototypesModel
	}
}