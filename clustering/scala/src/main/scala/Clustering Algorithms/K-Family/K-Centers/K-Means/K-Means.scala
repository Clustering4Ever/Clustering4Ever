package org.clustering4ever.scala.clustering.kmeans
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{mutable, GenSeq}
import org.clustering4ever.math.distances.{Distance, ContinuousDistance}
import org.clustering4ever.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.scala.clustering.kcenters.{KCentersModel, KCenters}
import org.clustering4ever.util.ScalaImplicits._
import org.clustering4ever.scala.vectors.{GVector, ScalarVector}
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
		ID,
		O,
		V <: Seq[Double],
		Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
		D <: ContinuousDistance[V],
		GS[Y] <: GenSeq[Y]
	](
		data: GS[Cz[ID, O, ScalarVector[V]]],
		k: Int,
		metric: D,
		maxIterations: Int,
		epsilon: Double,
		initializedCenters: mutable.HashMap[Int, ScalarVector[V]] = mutable.HashMap.empty[Int, ScalarVector[V]]
		)(implicit ct: ClassTag[Cz[ID, O, ScalarVector[V]]]): KCentersModel[ScalarVector[V], D, GS] = {
		val kMeansModel = KCenters.run(data, k, metric, epsilon, maxIterations, initializedCenters)
		kMeansModel
	}
	/**
	 * Run the K-Means with any continuous distance
	 */
	def run[V <: Seq[Double], D <: ContinuousDistance[V], GS[Y] <: GenSeq[Y]](
		data: GS[V],
		k: Int,
		metric: D,
		maxIterations: Int,
		epsilon: Double
	): KCentersModel[ScalarVector[V], D, GS] = {
		val kMeansModel = run(scalarToClusterizable(data), k, metric, maxIterations, epsilon, mutable.HashMap.empty[Int, ScalarVector[V]])
		kMeansModel
	}
}


		