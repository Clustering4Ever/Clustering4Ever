package org.clustering4ever.scala.clustering.kcenters
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{mutable, GenSeq}
import org.clustering4ever.math.distances.{Distance, ContinuousDistance}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.util.ScalaCollectionImplicits._
import org.clustering4ever.vectors.{GVector, ScalarVector}
/**
 *
 */
case class KMeans[ID, O, V <: Seq[Double], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Double]] <: ContinuousDistance[X], GS[X] <: GenSeq[X]](val args: KMeansArgs[V, D])(protected implicit val ct: ClassTag[Cz[ID, O, ScalarVector[V]]]) extends KCentersAncestor[ID, O, ScalarVector[V], Cz, D[V], GS, KMeansArgs[V, D], KMeansModel[ID, O, V, Cz, D, GS]] {
	def run(data: GS[Cz[ID, O, ScalarVector[V]]]): KMeansModel[ID, O, V, Cz, D, GS] = KMeansModel(obtainCenters(data), args.metric, args)
}
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
	 *
	 */
	def generateAnyArgumentsCombination[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](kValues: Seq[Int] = Seq(4, 6, 8), metricValues: Seq[D[V]] = Seq(Euclidean[V](false)), epsilonValues: Seq[Double] = Seq(0.0001), maxIterationsValues: Seq[Int] = Seq(40, 100), initializedCentersValues: Seq[mutable.HashMap[Int, ScalarVector[V]]] = Seq(mutable.HashMap.empty[Int, ScalarVector[V]])): Seq[KMeansArgs[V, D]] = {
		for(
			k <- kValues;
			metric <- metricValues;
			epsilon <- epsilonValues;
			maxIterations <- maxIterationsValues;
			initializedCenters <- initializedCentersValues
		) yield	KMeansArgs(k, metric, epsilon, maxIterations, initializedCenters)
	}
	/**
	 * Generate a KMeans version with specific arguments
	 */
	def generateAlgorithm[ID, O, V <: Seq[Double], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Double]] <: ContinuousDistance[X], GS[Y] <: GenSeq[Y]](
		data: GS[Cz[ID, O, ScalarVector[V]]],
		k: Int,
		metric: D[V],
		epsilon: Double,
		maxIterations: Int,
		initializedCenters: mutable.HashMap[Int, ScalarVector[V]] = mutable.HashMap.empty[Int, ScalarVector[V]]
	)(implicit ct: ClassTag[Cz[ID, O, ScalarVector[V]]]): KMeans[ID, O, V, Cz, D, GS] = {
		KMeans[ID, O, V, Cz, D, GS](KMeansArgs(k, metric, epsilon, maxIterations, initializedCenters))
	}
	/**
	 * Generate a KMeans version with specific arguments
	 */
	def generateAlgorithm[ID, O, V <: Seq[Double], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Double]] <: ContinuousDistance[X], GS[Y] <: GenSeq[Y]](
		data: GS[Cz[ID, O, ScalarVector[V]]],
		args: KMeansArgs[V, D]
	)(implicit ct: ClassTag[Cz[ID, O, ScalarVector[V]]]): KMeans[ID, O, V, Cz, D, GS] = {
		KMeans[ID, O, V, Cz, D, GS](args)
	}
	/**
	 * Run the K-Means with any continuous distance
	 */
	def run[ID, O, V <: Seq[Double], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Double]] <: ContinuousDistance[X], GS[Y] <: GenSeq[Y]](
		data: GS[Cz[ID, O, ScalarVector[V]]],
		k: Int,
		metric: D[V],
		maxIterations: Int,
		epsilon: Double,
		initializedCenters: mutable.HashMap[Int, ScalarVector[V]] = mutable.HashMap.empty[Int, ScalarVector[V]]
		)(implicit ct: ClassTag[Cz[ID, O, ScalarVector[V]]]): KMeansModel[ID, O, V, Cz, D, GS] = {
		val kmeansAlgorithm = generateAlgorithm(data, k, metric, epsilon, maxIterations, initializedCenters)
		kmeansAlgorithm.run(data)
	}
	/**
	 * Run the K-Means with any continuous distance
	 */
	def run[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X], GS[Y] <: GenSeq[Y]](
		data: GS[V],
		k: Int,
		metric: D[V],
		maxIterations: Int,
		epsilon: Double
	): KMeansModel[Int, ScalarVector[V], V, EasyClusterizable, D, GS] = {
		val kMeansModel = run(scalarToClusterizable(data), k, metric, maxIterations, epsilon, mutable.HashMap.empty[Int, ScalarVector[V]])
		kMeansModel
	}
}