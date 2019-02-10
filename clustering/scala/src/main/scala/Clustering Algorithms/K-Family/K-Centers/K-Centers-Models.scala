package org.clustering4ever.clustering.kcenters.scala
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{immutable, GenSeq}
import scala.util.Random
import scala.reflect.ClassTag
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixedDistance}
import org.clustering4ever.clustering.models.{CenterModel, CenterModelLocalReal, CenterModelLocalBinary, CenterModelMixedLocal, CenterModelMixed, CenterModelLocalCz, KnnModelModelLocalCz, KnnModelModel, KnnModelModelReal, KnnModelModelBinary, KnnModelModelMixed}
import org.clustering4ever.clustering.{ClusteringModelLocal, ClusteringModelLocalScalar, ClusteringModelLocalBinary}
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixedVector}
import org.clustering4ever.clusterizables.Clusterizable
/**
 * Trait regrouping commons elements between KCenters models descendant as well for scala than spark
 */
trait KCentersModelCommons[V <: GVector[V], D <: Distance[V]] extends CenterModel[V, D] {
	/**
	 * The number of initial seeked clusters, the real number of cluster can be smaller, cf centers.size
	 */
	val k: Int
	/**
	 * The stopping threshold for iterations
	 */
	val epsilon: Double
	/**
	 * The maximum number of authorized iterations
	 */
	val maxIterations: Int
}
/**
 *
 */
trait KCentersModelAncestor[V <: GVector[V], D <: Distance[V]] extends KCentersModelCommons[V, D] with ClusteringModelLocal[V] with CenterModelLocalCz[V, D] {

	protected[clustering] def obtainClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[Cz[O, V]] = centerPredict(data)
	/**
	 * Compute the distance between every points and all centers
	 */
	def prototypesDistancePerPoint[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[(Cz[O, V], immutable.HashMap[ClusterID, Double])] = {
		data.map( cz => (cz, centers.map{ case (clusterID, center) => (clusterID, metric.d(cz.v, center)) } ) ).asInstanceOf[GS[(Cz[O, V], immutable.HashMap[ClusterID, Double])]]
	}
}
/**
 * Generic KCenters model
 */
case class KCentersModel[V <: GVector[V], D[X <: GVector[X]] <: Distance[X]](val k: Int, val metric: D[V], val epsilon: Double, val maxIterations: Int, val centers: immutable.HashMap[Int, V] = immutable.HashMap.empty[Int, V]) extends KCentersModelAncestor[V, D[V]] with KnnModelModel[V, D[V]] {
	val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KCenters
}
/**
 * KMeans model
 */
case class KMeansModel[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](val k: Int, val metric: D[V], val epsilon: Double, val maxIterations: Int, val centers: immutable.HashMap[Int, ScalarVector[V]] = immutable.HashMap.empty[Int, ScalarVector[V]]) extends ClusteringModelLocalScalar[V] with KCentersModelAncestor[ScalarVector[V], D[V]] with CenterModelLocalReal[V, D[V]] with KnnModelModelReal[V, D[V]] {
	val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KMeans
}
/**
 * KModes model
 */
case class KModesModel[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X]](val k: Int, val metric: D[V], val epsilon: Double, val maxIterations: Int, val centers: immutable.HashMap[Int, BinaryVector[V]] = immutable.HashMap.empty[Int, BinaryVector[V]]) extends ClusteringModelLocalBinary[V] with KCentersModelAncestor[BinaryVector[V], D[V]] with CenterModelLocalBinary[V, D[V]] with KnnModelModelBinary[V, D[V]] {
	val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KModes
}
/**
 * KPrototypes model
 */
case class KPrototypesModels[Vb <: Seq[Int], Vs <: Seq[Double], D[X <: Seq[Int], Y <: Seq[Double]] <: MixedDistance[X, Y]](val k: Int, val metric: D[Vb, Vs], val epsilon: Double, val maxIterations: Int, val centers: immutable.HashMap[Int, MixedVector[Vb, Vs]] = immutable.HashMap.empty[Int, MixedVector[Vb, Vs]]) extends KCentersModelAncestor[MixedVector[Vb, Vs], D[Vb, Vs]] with CenterModelMixedLocal[Vb, Vs, D[Vb, Vs]] with KnnModelModelMixed[Vb, Vs, D[Vb, Vs]] {
	val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KPrototypes
}
