package org.clustering4ever.clustering.rla
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{immutable, GenSeq}
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.clustering.models.{CenterModelLocal, KnnModelModelLocal, CenterModelLocalCz, KnnModelModelLocalCz}
import org.clustering4ever.clustering.models.{CenterModelLocalReal, CenterModelLocalBinary, CenterModelMixtLocal, CenterModelLocalCz, KnnModelModelLocalCz, KnnModelModel, KnnModelModelReal, KnnModelModelBinary, KnnModelModelMixt}
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixtVector}
import org.clustering4ever.clustering.ClusteringModelLocal
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
	protected[clustering] def obtainClustering[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[Cz[O, V]] = centerPredict(data)
}
/**
 *
 */
case class RLAModel[V <: GVector[V], D[X <: GVector[X]] <: Distance[X]](val metric: D[V], val epsilon: Double, centers: immutable.HashMap[Int, V]) extends RLAModelAncestor[V, D[V]] with KnnModelModel[V, D[V]]
/**
 *
 */
case class RLAModelScalar[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](val metric: D[V], val epsilon: Double, centers: immutable.HashMap[Int, ScalarVector[V]]) extends RLAModelAncestor[ScalarVector[V], D[V]] with CenterModelLocalReal[V, D[V]] with KnnModelModelReal[V, D[V]]
/**
 *
 */
case class RLAModelBinary[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X]](val metric: D[V], val epsilon: Double, centers: immutable.HashMap[Int, BinaryVector[V]]) extends RLAModelAncestor[BinaryVector[V], D[V]] with CenterModelLocalBinary[V, D[V]] with KnnModelModelBinary[V, D[V]]
/**
 *
 */
case class RLAModelMixt[Vb <: Seq[Int], Vs <: Seq[Double], D[X <: Seq[Int], Y <: Seq[Double]] <: MixtDistance[X, Y]](val metric: D[Vb, Vs], val epsilon: Double, centers: immutable.HashMap[Int, MixtVector[Vb, Vs]]) extends RLAModelAncestor[MixtVector[Vb, Vs], D[Vb, Vs]] with CenterModelMixtLocal[Vb, Vs, D[Vb, Vs]] with KnnModelModelMixt[Vb, Vs, D[Vb, Vs]]
