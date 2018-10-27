package clustering4ever.scala.clustering.kprotoypes
/**
 * @author Beck Gaël
 */
import scala.collection.mutable
import clustering4ever.clustering.ClusteringModel
import clustering4ever.math.distances.MixtDistance
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.scala.clusterizables.MixtClusterizable
import clustering4ever.scala.clustering.KCommonsModel

final class KPrototypesModel[
	ID: Numeric,
	O,
	Vb <: Seq[Int],
	Vs <: Seq[Double],
	Cz <: MixtClusterizable[ID, O, Vb, Vs, Cz],
	D <: MixtDistance[Vb, Vs]
](centers: mutable.HashMap[Int, BinaryScalarVector[Vb, Vs]], metric: D) extends KCommonsModel[ID, BinaryScalarVector[Vb, Vs], D, Cz](centers, metric)