package org.clustering4ever.scala.clustering.kcenters
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{mutable, GenSeq}
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.clustering.models.{CenterOrientedModelReal, CenterOrientedModelBinary, CenterOrientedModelMixt, CenterOrientedModelLocalClusterizable, KnnOrientedModelLocalClusterizable}
import org.clustering4ever.clustering.ClusteringArgs
import org.clustering4ever.clustering.ClusteringModelLocal
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixtVector}
/**
 *
 */
trait KCentersModelAncestor[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D <: Distance[V], GS[X] <: GenSeq[X], +Args <: KCentersArgsAncestor[V, D]] extends CenterOrientedModelLocalClusterizable[V, D] with KnnOrientedModelLocalClusterizable[V, D] with ClusteringModelLocal[ID, O, V, Cz, GS, Args] {
	/**
	 *
	 */
	val args: Args
	/**
	 *
	 */
	def obtainClustering(data: GS[Cz[ID, O, V]]): GS[Cz[ID, O, V]] = centerPredict(data)
}
/**
 *
 */
case class KCentersModel[ID, O, V  <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: GVector[X]] <: Distance[X], GS[X] <: GenSeq[X]](val centers: mutable.HashMap[Int, V], val metric: D[V], val args: KCentersArgs[V, D])(implicit val ct: ClassTag[Cz[ID, O, V]]) extends KCentersModelAncestor[ID, O, V, Cz, D[V], GS, KCentersArgs[V, D]]
/**
 *
 */
case class KMeansModel[ID, O, V  <: Seq[Double], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Double]] <: ContinuousDistance[X], GS[X] <: GenSeq[X]](val centers: mutable.HashMap[Int, ScalarVector[V]], val metric: D[V], val args: KMeansArgs[V, D])(implicit val ct: ClassTag[Cz[ID, O, ScalarVector[V]]]) extends KCentersModelAncestor[ID, O, ScalarVector[V], Cz, D[V], GS, KMeansArgs[V, D]] with CenterOrientedModelReal[V, D]
/**
 *
 */
case class KModesModel[ID, O, V  <: Seq[Int], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Int]] <: BinaryDistance[X], GS[X] <: GenSeq[X]](val centers: mutable.HashMap[Int, BinaryVector[V]], val metric: D[V], val args: KModesArgs[V, D])(implicit val ct: ClassTag[Cz[ID, O, BinaryVector[V]]]) extends KCentersModelAncestor[ID, O, BinaryVector[V], Cz, D[V], GS, KModesArgs[V, D]] with CenterOrientedModelBinary[V, D]
/**
 *
 */
case class KPrototypesModel[ID, O, Vb  <: Seq[Int], Vs <: Seq[Double], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Int], Y <: Seq[Double]] <: MixtDistance[X, Y], GS[X] <: GenSeq[X]](val centers: mutable.HashMap[Int, MixtVector[Vb, Vs]], val metric: D[Vb, Vs], val args: KPrototypesArgs[Vb, Vs, D])(implicit val ct: ClassTag[Cz[ID, O, MixtVector[Vb, Vs]]]) extends KCentersModelAncestor[ID, O, MixtVector[Vb, Vs], Cz, D[Vb, Vs], GS, KPrototypesArgs[Vb, Vs, D]] with CenterOrientedModelMixt[Vb, Vs, D]