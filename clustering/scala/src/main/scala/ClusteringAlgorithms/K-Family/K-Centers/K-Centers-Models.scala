package org.clustering4ever.clustering.kcenters.scala
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{immutable, GenSeq}
import scala.util.Random
import scala.reflect.ClassTag
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixedDistance}
import org.clustering4ever.clustering.models.{CenterModel, CenterModelLocalReal, CenterModelLocalBinary, CenterModelMixedLocal, CenterModelMixed, CenterModelLocalCz, KnnModelModelLocalCz, KnnModelModel, KnnModelModelScalar, KnnModelModelBinary, KnnModelModelMixed}
import org.clustering4ever.clustering.{ClusteringModelLocal, ClusteringModelLocalScalar, ClusteringModelLocalBinary}
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixedVector}
import org.clustering4ever.clusterizables.Clusterizable
/**
 * @tparam V
 * @tparam D
 * Trait regrouping commons elements between KCenters models descendant as well for scala than spark
 */
trait KCentersModelCommons[V <: GVector[V], D <: Distance[V]] extends CenterModel[V, D] with KCommonsArgs[V, D] {
	/**
	 * The number of initial seeked clusters, the real number of cluster can be smaller, cf centers.size
	 */
	val k: Int
	/**
	 * The stopping threshold for iterations
	 */
	val minShift: Double
	/**
	 * The maximum number of authorized iterations
	 */
	val maxIterations: Int
}
/**
 *
 */
trait KCentersModelAncestor[V <: GVector[V], D <: Distance[V]] extends KCentersModelCommons[V, D] with ClusteringModelLocal[V] with CenterModelLocalCz[V, D] {

	protected[clustering] final def obtainClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[Cz[O, V]] = centerPredict(data)
	/**
	 * Compute the distance between every points and all centers
	 */
	final def prototypesDistancePerPoint[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[(Cz[O, V], immutable.HashMap[ClusterID, Double])] = {
		data.map( cz => (cz, centers.map{ case (clusterID, center) => (clusterID, metric.d(cz.v, center)) } ) ).asInstanceOf[GS[(Cz[O, V], immutable.HashMap[ClusterID, Double])]]
	}
}
/**
 * Generic KCenters model
 */
final case class KCentersModel[V <: GVector[V], D[X <: GVector[X]] <: Distance[X]](final val k: Int, final val metric: D[V], final val minShift: Double, final val maxIterations: Int, final val centers: immutable.HashMap[Int, V] = immutable.HashMap.empty[Int, V]) extends KCentersModelAncestor[V, D[V]] with KnnModelModel[V, D[V]] {
	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KCenters
}
/**
 * KMeans model
 */
final case class KMeansModel[D <: ContinuousDistance](final val k: Int, final val metric: D, final val minShift: Double, final val maxIterations: Int, final val centers: immutable.HashMap[Int, ScalarVector] = immutable.HashMap.empty[Int, ScalarVector]) extends KCentersModelAncestor[ScalarVector, D] with ClusteringModelLocalScalar with CenterModelLocalReal[D] with KnnModelModelScalar[D] {
	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KMeans
}
/**
 * KModes model
 */
final case class KModesModel[D <: BinaryDistance](final val k: Int, final val metric: D, final val minShift: Double, final val maxIterations: Int, final val centers: immutable.HashMap[Int, BinaryVector] = immutable.HashMap.empty[Int, BinaryVector]) extends KCentersModelAncestor[BinaryVector, D] with ClusteringModelLocalBinary with CenterModelLocalBinary[D] with KnnModelModelBinary[D] {
	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KModes
}
/**
 * KPrototypes model
 */
final case class KPrototypesModels[D <: MixedDistance](final val k: Int, final val metric: D, final val minShift: Double, final val maxIterations: Int, final val centers: immutable.HashMap[Int, MixedVector] = immutable.HashMap.empty[Int, MixedVector]) extends KCentersModelAncestor[MixedVector, D] with CenterModelMixedLocal[D] with KnnModelModelMixed[D] {
	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KPrototypes
}
