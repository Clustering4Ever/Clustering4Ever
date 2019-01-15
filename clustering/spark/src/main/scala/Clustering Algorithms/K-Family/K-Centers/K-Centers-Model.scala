package org.clustering4ever.spark.clustering.kcenters
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.mutable
import org.apache.spark.rdd.RDD
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.clustering.models.CenterOrientedModelDistributedCz
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixtVector}
import org.clustering4ever.clustering.ClusteringModelDistributed
/**
 *
 */
trait KCentersModelAncestor[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D <: Distance[V], Args <: KCentersArgsAncestor[V, D]] extends CenterOrientedModelDistributedCz[V, D] with ClusteringModelDistributed[ID, O, V, Cz, Args] {
	/**
	 *
	 */
	implicit val ct: ClassTag[Cz[ID, O, V]]
	/**
	 *
	 */
	def obtainClustering(data: RDD[Cz[ID, O, V]]): RDD[Cz[ID, O, V]] = centerPredictCz(data)
}
/**
 *
 */
case class KCentersModel[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: GVector[X]] <: Distance[X]](val centers: mutable.HashMap[Int, V], val metric: D[V], val args: KCentersArgs[V, D])(implicit val ct: ClassTag[Cz[ID, O, V]]) extends KCentersModelAncestor[ID, O, V, Cz, D[V], KCentersArgs[V, D]]
/**
 *
 */
case class KMeansModel[ID, O, V <: Seq[Double], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Double]] <: ContinuousDistance[X]](val centers: mutable.HashMap[Int, ScalarVector[V]], val metric: D[V], val args: KMeansArgs[V, D])(implicit val ct: ClassTag[Cz[ID, O, ScalarVector[V]]]) extends KCentersModelAncestor[ID, O, ScalarVector[V], Cz, D[V], KMeansArgs[V, D]]
/**
 *
 */
case class KModesModel[ID, O, V <: Seq[Int], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Int]] <: BinaryDistance[X]](val centers: mutable.HashMap[Int, BinaryVector[V]], val metric: D[V], val args: KModesArgs[V, D])(implicit val ct: ClassTag[Cz[ID, O, BinaryVector[V]]]) extends KCentersModelAncestor[ID, O, BinaryVector[V], Cz, D[V], KModesArgs[V, D]]
/**
 *
 */
case class KPrototypesModel[ID, O, Vb <: Seq[Int], Vs <: Seq[Double], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Int], Y <: Seq[Double]] <: MixtDistance[X, Y]](val centers: mutable.HashMap[Int, MixtVector[Vb, Vs]], val metric: D[Vb, Vs], val args: KPrototypesArgs[Vb, Vs, D])(implicit val ct: ClassTag[Cz[ID, O, MixtVector[Vb, Vs]]]) extends KCentersModelAncestor[ID, O, MixtVector[Vb, Vs], Cz, D[Vb, Vs], KPrototypesArgs[Vb, Vs, D]]