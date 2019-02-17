package org.clustering4ever.clustering.kcenters.scala
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{immutable, GenSeq}
import org.clustering4ever.math.distances.{Distance, BinaryDistance}
import org.clustering4ever.math.distances.binary.Hamming
import org.clustering4ever.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.util.ScalaCollectionImplicits._
import org.clustering4ever.vectors.{GVector, BinaryVector}
import org.clustering4ever.clustering.ClusteringAlgorithmLocalBinary
/**
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data GenSeq of Clusterizable descendant, the EasyClusterizable is the basic reference
 * @param k number of clusters seeked
 * @param minShift The stopping criteria, ie the distance under which centers are mooving from their previous position
 * @param maxIterations maximal number of iteration
 * @param metric a defined binary dissimilarity measure on a GVector descendant
 */
final case class KModes[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X]](final val k: Int, final val metric: D[V], final val minShift: Double, final val maxIterations: Int, final val customCenters: immutable.HashMap[Int, BinaryVector[V]] = immutable.HashMap.empty[Int, BinaryVector[V]]) extends KCentersAncestor[BinaryVector[V], D[V], KModesModel[V, D]] with ClusteringAlgorithmLocalBinary[V, KModesModel[V, D]] {

	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KModes

	final def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, BinaryVector[V]]]): KModesModel[V, D] = KModesModel(k, metric, minShift, maxIterations, obtainCenters(data))
}
/**
 *
 */
object KModes {
	/**
	 *
	 */
	final def generateAnyAlgorithmArgumentsCombination[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X]](
		kValues: Seq[Int] = Seq(4, 6, 8),
		metricValues: Seq[D[V]] = Seq(Hamming[V] _),
		minShiftValues: Seq[Double] = Seq(0.0001),
		maxIterationsValues: Seq[Int] = Seq(100),
		initializedCentersValues: Seq[immutable.HashMap[Int, BinaryVector[V]]] = Seq(immutable.HashMap.empty[Int, BinaryVector[V]])): Seq[KModes[V, D]] = {
		for(
			k <- kValues;
			metric <- metricValues;
			minShift <- minShiftValues;
			maxIterations <- maxIterationsValues;
			initializedCenters <- initializedCentersValues
		) yield	KModes(k, metric, minShift, maxIterations, initializedCenters)
	}
	/**
	 * Run the K-Modes with any binary distance
	 */
	final def fit[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X], GS[Y] <: GenSeq[Y]](
		data: GS[V],
		k: Int,
		metric: D[V],
		maxIterations: Int,
		minShift: Double
	): KModesModel[V, D] = {
		KModes(k, metric, minShift, maxIterations).fit(binaryToClusterizable(data))
	}
}