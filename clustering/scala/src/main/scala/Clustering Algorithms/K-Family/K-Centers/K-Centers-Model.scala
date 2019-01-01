package org.clustering4ever.scala.clustering.kcenters
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{mutable, GenSeq}
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.scala.clusterizables.Clusterizable
import org.clustering4ever.clustering.{CenterOrientedModelLocalClusterizable, KnnOrientedModelLocalClusterizable}
import org.clustering4ever.clustering.ClusteringArgs
import org.clustering4ever.scala.vectors.GVector
/**
 *
 */
class KCentersModel[
	V <: GVector,
	D <: Distance[V]
	](
	val centers: mutable.HashMap[Int, V],
	val metric: D,
	val kCentersArgs: ClusteringArgs
) extends CenterOrientedModelLocalClusterizable[V, D] with KnnOrientedModelLocalClusterizable[V, D]