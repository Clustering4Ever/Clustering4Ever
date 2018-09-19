package clustering4ever.spark.clustering.kmodes

import scala.collection.mutable
import clustering4ever.spark.clustering.KCommonsModelSpark
import scala.reflect.ClassTag
import clustering4ever.scala.clusterizables.BinaryClusterizable
import clustering4ever.math.distances.BinaryDistance

/**
 * @author Beck Gaël
 **/
class KModesModel[
	ID: Numeric,
	O,
	V <: Seq[Int],
	Cz <: BinaryClusterizable[ID, O, V] : ClassTag,
	D <: BinaryDistance[V]
](centers: mutable.HashMap[Int, V], metric: D) extends KCommonsModelSpark[ID, V, D, Cz](centers, metric)