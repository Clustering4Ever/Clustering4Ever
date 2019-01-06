package org.clustering4ever.scala.clustering.rla
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import org.clustering4ever.clustering.ClusteringModel
import scala.collection.{mutable, GenSeq}
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.clustering.models.{CenterOrientedModelLocal, KnnOrientedModelLocal, CenterOrientedModelLocalClusterizable, KnnOrientedModelLocalClusterizable}
import org.clustering4ever.vectors.GVector
import org.clustering4ever.clustering.ClusteringModelCz
/**
 *
 */
class RLAModel[O, D <: Distance[O]](val centers: mutable.HashMap[Int, O], val metric: D) extends CenterOrientedModelLocal[O, D] with KnnOrientedModelLocal[O, D]
/**
 *
 */
class RLAModelCz[V <: GVector[V], D <: Distance[V], GS[X] <: GenSeq[X]](val centers: mutable.HashMap[Int, V], val metric: D) extends CenterOrientedModelLocalClusterizable[V, D] with KnnOrientedModelLocalClusterizable[V, D] with ClusteringModelCz[V, GS] {
	/**
	 *
	 */
	def obtainClustering[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: GS[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): GS[Cz[ID, O, V]] = centerPredictCz(data)
}