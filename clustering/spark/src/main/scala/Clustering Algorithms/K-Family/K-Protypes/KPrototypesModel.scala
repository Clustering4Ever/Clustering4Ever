package clustering4ever.spark.clustering.kprototypes

import scala.collection.{mutable, immutable}
import org.apache.spark.rdd.RDD
import clustering4ever.clustering.CommonRDDPredictClusteringModel
import clustering4ever.math.distances.MixtDistance
import clustering4ever.scala.measurableclass.BinaryScalarVector


/**
 * @author Beck Gaël
 **/
class KPrototypesModel[Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs]](val centers: mutable.HashMap[Int, V], val metric: MixtDistance[Vb, Vs, V]) extends CommonRDDPredictClusteringModel[V, MixtDistance[Vb, Vs, V]]