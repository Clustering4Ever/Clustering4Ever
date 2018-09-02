package clustering4ever.scala.clustering.kmodes

import clustering4ever.clustering.CommonPredictClusteringModel
import scala.collection.mutable
import clustering4ever.math.distances.{BinaryDistance, Distance}
import clustering4ever.scala.clusterizables.BinaryClusterizable
import scala.reflect.ClassTag
import clustering4ever.scala.clustering.KCommonsModel
/**
 * @author Beck Gaël
 **/
final class KModesModel[
	ID: Numeric,
	Obj,
	V <: Seq[Int] : ClassTag,
	Cz <: BinaryClusterizable[ID, Obj, V],
	D <: BinaryDistance[V]
](centers: mutable.HashMap[Int, V], metric: D) extends KCommonsModel[ID, V, D, Cz](centers, metric)