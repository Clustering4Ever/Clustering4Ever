package clustering4ever.scala.clustering.kmodes
/**
 * @author Beck Gaël
 */
import clustering4ever.clustering.CommonPredictClusteringModel
import scala.collection.mutable
import clustering4ever.math.distances.{BinaryDistance, Distance}
import clustering4ever.scala.clusterizables.BinaryClusterizable
import scala.reflect.ClassTag
import clustering4ever.scala.clustering.KCommonsModel

final class KModesModel[
	ID: Numeric,
	O,
	V <: Seq[Int] : ClassTag,
	Cz <: BinaryClusterizable[ID, O, V, Cz],
	D <: BinaryDistance[V]
](centers: mutable.HashMap[Int, V], metric: D) extends KCommonsModel[ID, V, D, Cz](centers, metric)