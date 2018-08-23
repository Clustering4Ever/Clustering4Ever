package clustering4ever.scala.clustering.kprotoypes

import scala.collection.{mutable, immutable}
import clustering4ever.clustering.ClusteringModel
import clustering4ever.math.distances.MixtDistance
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.scala.clusterizables.MixtClusterizable
import clustering4ever.scala.clustering.KCommonsModel

/**
 * @author Beck Gaël
 **/
final class KPrototypesModel[ID: Numeric, Vb <: Seq[Int], Vs <: Seq[Double], Obj, V <: BinaryScalarVector[Vb, Vs]](centers: mutable.HashMap[Int, V], metric: MixtDistance[Vb, Vs, V]) extends KCommonsModel[ID, V, MixtDistance[Vb, Vs, V], MixtClusterizable[ID, Obj, Vb, Vs, V]](centers, metric)