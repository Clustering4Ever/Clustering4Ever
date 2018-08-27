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
sealed abstract class KModesModel[
	ID: Numeric,
	V <: immutable.Seq[Int] : ClassTag,
	Obj
](
	centers: mutable.HashMap[Int, V],
	metric: BinaryDistance[V]
) extends KCommonsModel[
	ID,
	Int,
	V,
	BinaryDistance[V],
	BinaryClusterizable[ID, Obj, V]
](centers, metric)

final class KModesModelSeq[ID: Numeric, Obj](centers: mutable.HashMap[Int, immutable.Seq[Int]], metric: BinaryDistance[immutable.Seq[Int]]) extends KModesModel[ID, immutable.Seq[Int], Obj](centers, metric)

final class KModesModelCustom[ID: Numeric, V <: immutable.Seq[Int] : ClassTag, Obj](centers: mutable.HashMap[Int, V], metric: BinaryDistance[V]) extends KModesModel[ID, V, Obj](centers, metric)