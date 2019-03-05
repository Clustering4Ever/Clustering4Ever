package org.clustering4ever.clustering.kcenters.scala
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{mutable, immutable, GenSeq}
import org.clustering4ever.math.distances.{Distance, ContinuousDistance}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.util.ScalaCollectionImplicits._
import org.clustering4ever.vectors.{GVector, ScalarVector}
import org.clustering4ever.util.FromArrayToSeq
import org.clustering4ever.clustering.ClusteringAlgorithmLocalScalar
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.types.ClusteringNumberType._
/**
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data GenSeq of Clusterizable descendant, the EasyClusterizable is the basic reference
 * @param k number of clusters
 * @param minShift The stopping criteria, ie the distance under which centers are mooving from their previous position
 * @param maxIterations maximal number of iteration
 * @param metric a defined continuous dissimilarity measure on a GVector descendant
 */
final case class KMeans[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](final val k: Int, final val metric: D[V], final val minShift: Double, final val maxIterations: Int, final val customCenters: immutable.HashMap[Int, ScalarVector[V]] = immutable.HashMap.empty[Int, ScalarVector[V]]) extends KCentersAncestor[ScalarVector[V], D[V], KMeansModel[V, D]] with ClusteringAlgorithmLocalScalar[V, KMeansModel[V, D]] {

	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KMeans

	final def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, ScalarVector[V]]]): KMeansModel[V, D] = KMeansModel(k, metric, minShift, maxIterations, obtainMedians(data))

}
/**
 *
 */
object KMeans {
	/**
	 * @param kValues
	 * @param metricValues
	 * @param minShiftValues
	 * @param maxIterationsValues
	 * @param customCentersValues
	 */
	final def generateAnyAlgorithmArgumentsCombination[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](kValues: Seq[Int] = Seq(4, 6, 8), metricValues: Seq[D[V]] = Seq(Euclidean[V](false)), minShiftValues: Seq[Double] = Seq(0.0001), maxIterationsValues: Seq[Int] = Seq(40, 100), customCentersValues: Seq[immutable.HashMap[Int, ScalarVector[V]]] = Seq(immutable.HashMap.empty[Int, ScalarVector[V]])): Seq[KMeans[V, D]] = {
		for(
			k <- kValues;
			metric <- metricValues;
			minShift <- minShiftValues;
			maxIterations <- maxIterationsValues;
			customCenters <- customCentersValues
		) yield	KMeans(k, metric, minShift, maxIterations, customCenters)
	}
	/**
	 * Run the K-Means with any ContinuousDistance[V <: Seq[Double]]
	 */
	final def fit[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X], GS[Y] <: GenSeq[Y]](
		data: GS[V],
		k: Int,
		metric: D[V],
		minShift: Double,
		maxIterations: Int
	): KMeansModel[V, D] = {
		KMeans(k, metric, minShift, maxIterations, immutable.HashMap.empty[Int, ScalarVector[V]]).fit(scalarToClusterizable(data))
	}
	/**
	 * Run the K-Means with any ContinuousDistance[V <: Seq[Double]] with Array[Double] as input vectors
	 */
	final def fit[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X], GS[Y] <: GenSeq[Y]](
		data: GS[Array[Double]],
		k: Int,
		metric: D[V],
		minShift: Double,
		maxIterations: Int
	)(implicit d: DummyImplicit): KMeansModel[V, D] = {
		KMeans(k, metric, minShift, maxIterations, immutable.HashMap.empty[Int, ScalarVector[V]]).fit(scalarToClusterizable(data.map{ a => FromArrayToSeq.arrayToScalarSeq(a) }.asInstanceOf[GS[V]]))
	}
}