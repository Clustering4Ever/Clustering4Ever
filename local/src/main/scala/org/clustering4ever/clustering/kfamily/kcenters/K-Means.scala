package org.clustering4ever.clustering.kfamily.kcenters

/**
 * @author Beck Gaël
 */
import org.clustering4ever.clusteringtraits.ClusteringAlgorithmLocalScalar
import org.clustering4ever.distances.ContinuousDistance
import org.clustering4ever.distances.continuous.Euclidean
import org.clustering4ever.roottraits
import org.clustering4ever.roottraits.{Clusterizable, GVector, ScalarVector}
import org.clustering4ever.util.ScalaCollectionImplicits._

import scala.collection.{GenSeq, immutable}
import scala.language.higherKinds
/**
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data GenSeq of Clusterizable descendant, the EasyClusterizable is the basic reference
 * @param k number of clusters
 * @param minShift The stopping criteria, ie the distance under which centers are mooving from their previous position
 * @param maxIterations maximal number of iteration
 * @param metric a defined continuous dissimilarity measure on a GVector descendant
 */
final case class KMeans[D <: ContinuousDistance](final val k: Int, final val metric: D, final val minShift: Double, final val maxIterations: Int, final val customCenters: immutable.HashMap[Int, ScalarVector] = immutable.HashMap.empty[Int, ScalarVector]) extends KCentersAncestor[ScalarVector, D, KMeansModel[D]] with ClusteringAlgorithmLocalScalar[KMeansModel[D]] {

	final val algorithmID = roottraits.KMeans

	final def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, ScalarVector]]): KMeansModel[D] = KMeansModel(k, metric, minShift, maxIterations, obtainCenters(data))

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
	final def generateAnyAlgorithmArgumentsCombination[D <: ContinuousDistance](kValues: Seq[Int] = Seq(4, 6, 8), metricValues: Seq[D] = Seq(Euclidean(false)), minShiftValues: Seq[Double] = Seq(0.0001), maxIterationsValues: Seq[Int] = Seq(40, 100), customCentersValues: Seq[immutable.HashMap[Int, ScalarVector]] = Seq(immutable.HashMap.empty[Int, ScalarVector])): Seq[KMeans[D]] = {
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
	final def fit[D <: ContinuousDistance, GS[Y] <: GenSeq[Y]](
		data: GS[Array[Double]],
		k: Int,
		metric: D,
		minShift: Double,
		maxIterations: Int
	): KMeansModel[D] = {
		KMeans(k, metric, minShift, maxIterations, immutable.HashMap.empty[Int, ScalarVector]).fit(scalarToClusterizable(data))
	}
}