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
	Vb <: Seq[Int],
	Vs <: Seq[Double],
	O,
	V <: BinaryScalarVector[Vb, Vs],
	Cz <: MixtClusterizable[ID, O, Vb, Vs, V, Cz],
	D <: MixtDistance[Vb, Vs, V]
](centers: mutable.HashMap[Int, V], metric: D) extends KCommonsModel[ID, V, D, Cz](centers, metric)