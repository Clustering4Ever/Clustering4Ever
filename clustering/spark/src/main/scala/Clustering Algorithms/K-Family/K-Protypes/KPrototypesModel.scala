package clustering4ever.spark.clustering.kprototypes

import scala.collection.{mutable, immutable}
import org.apache.spark.rdd.RDD
import clustering4ever.clustering.CommonRDDPredictClusteringModel
import clustering4ever.math.distances.MixtDistance
import clustering4ever.scala.measurableclass.BinaryScalarVector


/**
 * @author Beck Gaël
 **/
class KPrototypesModel[Vb <: immutable.Seq[Int], Vs <: immutable.Seq[Double]](val centers: mutable.HashMap[Int, BinaryScalarVector[Vb, Vs]], val metric: MixtDistance[Vb, Vs]) extends CommonRDDPredictClusteringModel[BinaryScalarVector[Vb, Vs], MixtDistance[Vb, Vs]]