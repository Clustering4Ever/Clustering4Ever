package org.clustering4ever.clustering.kcenters.rdd
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.immutable
import scala.util.Random
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.Dataset
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.util.SparkImplicits._
import org.clustering4ever.vectors.{GVector, ScalarVector}
/**
 *
 */
case class KMeans[ID, O, V <: Seq[Double], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Double]] <: ContinuousDistance[X]](val args: KMeansArgs[V, D])(protected implicit val ct: ClassTag[Cz[ID, O, ScalarVector[V]]], protected val ctV: ClassTag[ScalarVector[V]]) extends KCentersAncestor[ID, O, ScalarVector[V], Cz, D[V], KMeansArgs[V, D], KMeansModel[ID, O, V, Cz, D]] {
	/**
	 *
	 */
	def run(data: RDD[Cz[ID, O, ScalarVector[V]]]): KMeansModel[ID, O, V, Cz, D] = KMeansModel[ID, O, V, Cz, D](obtainCenters(data), args.metric, args)
}
/**
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : preferably and ArrayBuffer or ParArray of Clusterizable descendant, the EasyClusterizable is the basic reference
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
		ID,
		O,
		V <: Seq[Double],
		Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
		D[X <: Seq[Double]] <: ContinuousDistance[X]
	](
		data: RDD[Cz[ID, O, ScalarVector[V]]],
		k: Int,
		metric: D[V],
		epsilon: Double,
		maxIterations: Int,
		persistanceLVL: StorageLevel,
		initializedCenters: immutable.HashMap[Int, ScalarVector[V]] = immutable.HashMap.empty[Int, ScalarVector[V]]
		)(implicit ct: ClassTag[Cz[ID, O, ScalarVector[V]]]): KMeansModel[ID, O, V, Cz, D] = {
		val kMeans = KMeans[ID, O, V, Cz, D](KMeansArgs(k, metric, epsilon, maxIterations, persistanceLVL, initializedCenters))
		val kCentersModel = kMeans.run(data)
		kCentersModel
	}
	/**
	 * Run the K-Means with any continuous distance
	 */
	def run[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](
		data: RDD[V],
		k: Int,
		metric: D[V],
		epsilon: Double,
		maxIterations: Int,
		persistanceLVL: StorageLevel
		): KMeansModel[Long, ScalarVector[V], V, EasyClusterizable, D] = {
		val kCentersModel = run(scalarDataWithIDToClusterizable(data.zipWithIndex), k, metric, epsilon, maxIterations, persistanceLVL)
		kCentersModel
	}
}