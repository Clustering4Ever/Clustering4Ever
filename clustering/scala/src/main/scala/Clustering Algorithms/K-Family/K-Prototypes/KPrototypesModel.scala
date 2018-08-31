package clustering4ever.scala.clustering.kprotoypes

import scala.collection.{mutable, GenSeq}
import clustering4ever.clustering.ClusteringModel
import clustering4ever.math.distances.MixtDistance
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.scala.clusterizables.MixtClusterizable
import clustering4ever.scala.clustering.KCommonsModel

/**
 * @author Beck Gaël
 **/
final class KPrototypesModel[
	ID: Numeric,
	Vb <: GenSeq[Int],
	Vs <: GenSeq[Double],
	Obj,
	V <: BinaryScalarVector[Vb, Vs],
	Cz <: MixtClusterizable[ID, Obj, Vb, Vs, V],
	D <: MixtDistance[Vb, Vs, V]
](centers: mutable.HashMap[Int, V], metric: D) extends KCommonsModel[ID, V, D, Cz](centers, metric)