package org.clustering4ever.scala.clustering.rla
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import org.clustering4ever.clustering.GenericClusteringModel
import scala.collection.{mutable, GenSeq}
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.clustering.models.{CenterOrientedModelReal, CenterOrientedModelBinary, CenterOrientedModelMixt, CenterOrientedModelLocal, KnnOrientedModelLocal, CenterOrientedModelLocalClusterizable, KnnOrientedModelLocalClusterizable}
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixtVector}
import org.clustering4ever.clustering.ClusteringModelLocal
/**
 *
 */
trait RLAModelAncestor[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D <: Distance[V], GS[X] <: GenSeq[X], +Args <: RLAArgsAncestor[V, D]] extends CenterOrientedModelLocalClusterizable[V, D] with KnnOrientedModelLocalClusterizable[V, D] with ClusteringModelLocal[ID, O, V, Cz, GS, Args] {
	/**
	 *
	 */
	def obtainClustering(data: GS[Cz[ID, O, V]]): GS[Cz[ID, O, V]] = centerPredict(data)
}
/**
 *
 */
case class RLAModel[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: GVector[X]] <: Distance[X], GS[X] <: GenSeq[X]](val centers: mutable.HashMap[Int, V], val metric: D[V], val args: RLAArgs[V, D])(protected implicit val ct: ClassTag[Cz[ID, O, V]]) extends RLAModelAncestor[ID, O, V, Cz, D[V], GS, RLAArgs[V, D]] {
}
/**
 *
 */
case class RLAModelScalar[ID, O, V <: Seq[Double], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Double]] <: ContinuousDistance[X], GS[X] <: GenSeq[X]](val centers: mutable.HashMap[Int, ScalarVector[V]], val metric: D[V], val args: RLAArgsScalar[V, D])(protected implicit val ct: ClassTag[Cz[ID, O, ScalarVector[V]]]) extends RLAModelAncestor[ID, O, ScalarVector[V], Cz, D[V], GS, RLAArgsScalar[V, D]]  with CenterOrientedModelReal[V, D]
/**
 *
 */
case class RLAModelBinary[ID, O, V <: Seq[Int], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Int]] <: BinaryDistance[X], GS[X] <: GenSeq[X]](val centers: mutable.HashMap[Int, BinaryVector[V]], val metric: D[V], val args: RLAArgsBinary[V, D])(protected implicit val ct: ClassTag[Cz[ID, O, BinaryVector[V]]]) extends RLAModelAncestor[ID, O, BinaryVector[V], Cz, D[V], GS, RLAArgsBinary[V, D]] with CenterOrientedModelBinary[V, D]
/**
 *
 */
case class RLAModelMixt[ID, O, Vb <: Seq[Int], Vs <: Seq[Double], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Int], Y <: Seq[Double]] <: MixtDistance[X, Y], GS[X] <: GenSeq[X]](val centers: mutable.HashMap[Int, MixtVector[Vb, Vs]], val metric: D[Vb, Vs], val args: RLAArgsMixt[Vb, Vs, D])(protected implicit val ct: ClassTag[Cz[ID, O, MixtVector[Vb, Vs]]]) extends RLAModelAncestor[ID, O, MixtVector[Vb, Vs], Cz, D[Vb, Vs], GS, RLAArgsMixt[Vb, Vs, D]] with CenterOrientedModelMixt[Vb, Vs, D]