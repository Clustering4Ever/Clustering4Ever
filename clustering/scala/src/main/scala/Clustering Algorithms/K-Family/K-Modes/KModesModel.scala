package clustering4ever.scala.clustering.kmodes

import clustering4ever.clustering.CommonPredictClusteringModel
import scala.collection.{mutable, immutable, GenSeq}
import clustering4ever.math.distances.{BinaryDistance, Distance}
import clustering4ever.scala.clusterizables.BinaryClusterizable
import scala.reflect.ClassTag
import clustering4ever.scala.clustering.KCommonsModel
/**
 * @author Beck Gaël
 **/
final class KModesModel[ID: Numeric, V <: Seq[Int] : ClassTag, Obj](centers: mutable.HashMap[Int, V], metric: BinaryDistance[V]) extends KCommonsModel[ID, V, BinaryDistance[V], BinaryClusterizable[ID, Obj, V]](centers, metric)