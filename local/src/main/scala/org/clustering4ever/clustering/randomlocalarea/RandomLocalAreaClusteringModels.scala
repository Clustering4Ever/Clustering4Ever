package org.clustering4ever.clustering.randomlocalarea

/**
 * @author Beck Gaël
 */
import org.clustering4ever.clusteringtraits.{ClusteringModelLocal, ClusteringModelLocalBinary, ClusteringModelLocalScalar, MetricArgs}
import org.clustering4ever.distances.{BinaryDistance, ContinuousDistance, Distance, MixedDistance}
import org.clustering4ever.models._
import org.clustering4ever.roottraits._

import scala.collection.{GenSeq, immutable}
import scala.language.higherKinds
/**
 *
 */
trait RLAModelAncestor[V <: GVector[V], D <: Distance[V]] extends ClusteringModelLocal[V] with KnnModelModelLocalCz[V, D] with CenterModelLocalCz[V, D] with MetricArgs[V, D] {
	/**
	 *
	 */
	val epsilon: Double
	/**
	 *
	 */
	protected[clustering4ever] final def obtainClustering[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[Cz[O, V]] = centerPredict(data)
}
/**
 *
 */
final case class RLAModel[V <: GVector[V], D[X <: GVector[X]] <: Distance[X]](final val metric: D[V], final val epsilon: Double, final val centers: immutable.HashMap[Int, V]) extends RLAModelAncestor[V, D[V]] with KnnModelModel[V, D[V]] {
  final val algorithmID = org.clustering4ever.roottraits.RLA

}
/**
 *
 */
final case class RLAModelScalar[D <: ContinuousDistance](final val metric: D, final val epsilon: Double, final val centers: immutable.HashMap[Int, ScalarVector]) extends ClusteringModelLocalScalar with RLAModelAncestor[ScalarVector, D] with CenterModelLocalReal[D] with KnnModelModelScalar[D] {
  final val algorithmID = org.clustering4ever.roottraits.RLAScalar

}
/**
 *
 */
final case class RLAModelBinary[D <: BinaryDistance](final val metric: D, final val epsilon: Double, final val centers: immutable.HashMap[Int, BinaryVector]) extends ClusteringModelLocalBinary with RLAModelAncestor[BinaryVector, D] with CenterModelLocalBinary[D] with KnnModelModelBinary[D] {
  final val algorithmID = org.clustering4ever.roottraits.RLABinary

}
/**
 *
 */
final case class RLAModelMixed[D <: MixedDistance](final val metric: D, final val epsilon: Double, final val centers: immutable.HashMap[Int, MixedVector]) extends RLAModelAncestor[MixedVector, D] with CenterModelMixedLocal[D] with KnnModelModelMixed[D] {
  final val algorithmID = org.clustering4ever.roottraits.RLAMixed

}
