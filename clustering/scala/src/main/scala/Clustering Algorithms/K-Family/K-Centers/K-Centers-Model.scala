package org.clustering4ever.scala.clustering.kcenters
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{mutable, GenSeq}
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.clustering.models.{CenterOrientedModelLocalClusterizable, KnnOrientedModelLocalClusterizable}
import org.clustering4ever.clustering.ClusteringArgs
import org.clustering4ever.clustering.ClusteringModelLocal
import org.clustering4ever.vectors.GVector
/**
 *
 */
class KCentersModel[V <: GVector[V], D <: Distance[V], GS[X] <: GenSeq[X]](val centers: mutable.HashMap[Int, V], val metric: D, val kCentersArgs: ClusteringArgs) extends CenterOrientedModelLocalClusterizable[V, D] with KnnOrientedModelLocalClusterizable[V, D] with ClusteringModelLocal[V, GS] {
	/**
	 *
	 */
	def obtainClustering[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: GS[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): GS[Cz[ID, O, V]] = centerPredictCz(data)
}