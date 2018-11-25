package org.clustering4ever.scala.clustering.kmeans
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{mutable, GenSeq}
import scala.util.Random
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.scala.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.scala.clustering.kcenters.{KCentersModel, KCenters}
import org.clustering4ever.util.ScalaImplicits._
/**
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : preferably and ArrayBuffer or ParArray of RealClusterizable descendant, the SimpleRealClusterizable is the basic reference with mutable.ArrayBuffer as vector type, they are recommendend for speed efficiency
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
		data: GenSeq[Cz[ID, O, V]],
		k: Int,
		metric: D,
		maxIterations: Int,
		epsilon: Double,
		initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V],
		workingVector: Int = 0
		)(implicit ct: ClassTag[V]): KCentersModel[ID, O, V, Cz[ID, O, V], D] = {
		val kMeans = new KCenters[ID, O, V, Cz[ID, O, V], D](k, epsilon, maxIterations, metric, initializedCenters)
		val kMeansModel = kMeans.run(data)(workingVector)
		kMeansModel
	}
	/**
	 * Run the K-Means with any continuous distance
	 */
	def easyRun[V[Double] <: Seq[Double], D <: ContinuousDistance[V[Double]]](
		data: GenSeq[V[Double]],
		k: Int,
		metric: D,
		maxIterations: Int,
		epsilon: Double = 0.001,
		initializedCenters: mutable.HashMap[Int, V[Double]] = mutable.HashMap.empty[Int, V[Double]]
		)(implicit ct: ClassTag[V[Double]]) : KCentersModel[Long, V[Double], V[Double], EasyClusterizable[Long, V[Double], V[Double]], D] = {
		val kMeans = new KCenters[Long, V[Double], V[Double], EasyClusterizable[Long, V[Double], V[Double]], D](k, epsilon, maxIterations, metric, initializedCenters)
		val kMeansModel = kMeans.run(data)(0)
		kMeansModel
	}
}