package org.clustering4ever.spark.clustering.kmeans
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{mutable, GenSeq}
import scala.util.Random
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.util.SparkImplicits._
import org.clustering4ever.spark.clustering.kcenters.{KCentersArgs, KCentersModel, KCenters}
import org.clustering4ever.scala.vectors.{GVector, ScalarVector}
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
		ID,
		O,
		V <: Seq[Double],
		Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
		D <: ContinuousDistance[V]
	](
		data: RDD[Cz[ID, O, ScalarVector[V]]],
		k: Int,
		metric: D,
		epsilon: Double,
		maxIterations: Int,
		persistanceLVL: StorageLevel,
		initializedCenters: mutable.HashMap[Int, ScalarVector[V]] = mutable.HashMap.empty[Int, ScalarVector[V]]
		)(implicit ct: ClassTag[Cz[ID, O, ScalarVector[V]]]): KCentersModel[ScalarVector[V], D] = {
		val kMeans = new KCenters[ScalarVector[V], D](new KCentersArgs[ScalarVector[V], D](k, metric, epsilon, maxIterations, persistanceLVL, initializedCenters))
		val kCentersModel = kMeans.run(data)
		kCentersModel
	}
	/**
	 * Run the K-Means with any continuous distance
	 */
	def run[V <: Seq[Double], D <: ContinuousDistance[V]](
		data: RDD[V],
		k: Int,
		metric: D,
		epsilon: Double,
		maxIterations: Int,
		persistanceLVL: StorageLevel
		): KCentersModel[ScalarVector[V], D] = {
		val kCentersModel = run(scalarDataWithIDToClusterizable(data.zipWithIndex), k, metric, epsilon, maxIterations, persistanceLVL)
		kCentersModel
	}
}