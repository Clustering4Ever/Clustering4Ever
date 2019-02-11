package org.clustering4ever.clustering.rla
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{immutable, GenSeq}
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixedDistance}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.clustering.models.{CenterModelLocal, KnnModelModelLocal, CenterModelLocalCz, KnnModelModelLocalCz}
import org.clustering4ever.clustering.models.{CenterModelLocalReal, CenterModelLocalBinary, CenterModelMixedLocal, CenterModelLocalCz, KnnModelModelLocalCz, KnnModelModel, KnnModelModelReal, KnnModelModelBinary, KnnModelModelMixed}
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixedVector}
import org.clustering4ever.clustering.{ClusteringModelLocal, ClusteringModelLocalScalar, ClusteringModelLocalBinary}
/**
 *
 */
trait RLAModelAncestor[V <: GVector[V], D <: Distance[V]] extends ClusteringModelLocal[V] with KnnModelModelLocalCz[V, D] with CenterModelLocalCz[V, D] {
	/**
	 *
	 */
	val epsilon: Double
	/**
	 *
	 */
	// val algorithm = org.clustering4ever.extensibleAlgorithmNature.RLA
	/**
	 *
	 */
	protected[clustering] final def obtainClustering[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[Cz[O, V]] = centerPredict(data)
}
/**
 *
 */
case class RLAModel[V <: GVector[V], D[X <: GVector[X]] <: Distance[X]](val metric: D[V], val epsilon: Double, centers: immutable.HashMap[Int, V]) extends RLAModelAncestor[V, D[V]] with KnnModelModel[V, D[V]] {
  final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.RLA

}
/**
 *
 */
case class RLAModelScalar[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](val metric: D[V], val epsilon: Double, centers: immutable.HashMap[Int, ScalarVector[V]]) extends ClusteringModelLocalScalar[V] with RLAModelAncestor[ScalarVector[V], D[V]] with CenterModelLocalReal[V, D[V]] with KnnModelModelReal[V, D[V]] {
  final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.RLAScalar

}
/**
 *
 */
case class RLAModelBinary[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X]](val metric: D[V], val epsilon: Double, centers: immutable.HashMap[Int, BinaryVector[V]]) extends ClusteringModelLocalBinary[V] with RLAModelAncestor[BinaryVector[V], D[V]] with CenterModelLocalBinary[V, D[V]] with KnnModelModelBinary[V, D[V]] {
  final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.RLABinary

}
/**
 *
 */
case class RLAModelMixed[Vb <: Seq[Int], Vs <: Seq[Double], D[X <: Seq[Int], Y <: Seq[Double]] <: MixedDistance[X, Y]](val metric: D[Vb, Vs], val epsilon: Double, centers: immutable.HashMap[Int, MixedVector[Vb, Vs]]) extends RLAModelAncestor[MixedVector[Vb, Vs], D[Vb, Vs]] with CenterModelMixedLocal[Vb, Vs, D[Vb, Vs]] with KnnModelModelMixed[Vb, Vs, D[Vb, Vs]] {
  final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.RLAMixt

}
