package clustering4ever.spark.clustering.kprototypes

import scala.collection.mutable
import scala.reflect.ClassTag
import org.apache.spark.rdd.RDD
import clustering4ever.clustering.CommonRDDPredictClusteringModel
import clustering4ever.math.distances.MixtDistance
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.scala.clusterizables.MixtClusterizable
import clustering4ever.spark.clustering.KCommonsModelSpark

/**
 * @author Beck Gaël
 **/
class KPrototypesModel[
	ID: Numeric,
	O,
	Vb <: Seq[Int],
	Vs <: Seq[Double],
	V <: BinaryScalarVector[Vb, Vs],
	Cz <: MixtClusterizable[ID, O, Vb, Vs, V] : ClassTag,
	D <: MixtDistance[Vb, Vs, V]
](centers: mutable.HashMap[Int, V], metric: D) extends KCommonsModelSpark[ID, V, D, Cz](centers, metric)