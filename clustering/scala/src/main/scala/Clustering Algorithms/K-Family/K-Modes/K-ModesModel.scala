package clustering4ever.scala.clustering.kmodes
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import clustering4ever.clustering.CentersBasedModel
import scala.collection.mutable
import clustering4ever.math.distances.{BinaryDistance, Distance}
import clustering4ever.scala.clusterizables.BinaryClusterizable
import clustering4ever.scala.clustering.KCommonsModel
/**
 *
 */
final class KModesModel[
	ID: Numeric,
	O,
	V[Int] <: Seq[Int],
	Cz <: BinaryClusterizable[ID, O, V[Int], Cz],
	D <: BinaryDistance[V]
](centers: mutable.HashMap[Int, V[Int]], metric: D) extends KCommonsModel[ID, V[Int], D, Cz](centers, metric)