package org.clustering4ever.spark.clustering.kmeans
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
		V <: Seq[Double],
		Cz[ID, O, V] <: Clusterizable[ID, O, V, Cz[ID, O, V]],
		D <: ContinuousDistance[V]
	](
		data: RDD[Cz[ID, O, V]],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D,
		initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
		)(implicit ct: ClassTag[V], ct2: ClassTag[Cz[ID, O, V]], workingVector: Int = 0): KCentersModel[ID, O, V, Cz[ID, O, V], D] = {
		val kMeans = new KCenters(k, epsilon, maxIterations, metric, initializedCenters)
		val kCentersModel = kMeans.run(data)(workingVector)
		kCentersModel
	}
	/**
	 * Run the K-Means with any continuous distance
	 */
	def runRawData[V[X] <: Seq[X], D <: ContinuousDistance[V[Double]]](
		data: RDD[V[Double]],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D,
		initializedCenters: mutable.HashMap[Int, V[Double]] = mutable.HashMap.empty[Int, V[Double]]
		)(implicit ct: ClassTag[V[Double]], workingVector: Int = 0) : KCentersModel[Long, V[Double], V[Double], EasyClusterizable[Long, V[Double], V[Double]], D] = {
		val kMeans = new KCenters(k, epsilon, maxIterations, metric, initializedCenters)
		val kCentersModel = kMeans.run(realVectorRDDToRealClusterizable(data))(workingVector)
		kCentersModel
	}
}