package clustering4ever.spark.clustering.kprototypes

import _root_.scala.collection.mutable
import _root_.org.apache.spark.rdd.RDD
import _root_.clustering4ever.clustering.CommonRDDPredictClusteringModel
import _root_.clustering4ever.math.distances.MixtDistance
import _root_.clustering4ever.scala.measurableclass.BinaryScalarVector


/**
 * @author Beck Gaël
 **/
class KPrototypesModel(centers: mutable.HashMap[Int, BinaryScalarVector], metric: MixtDistance) extends CommonRDDPredictClusteringModel(centers, metric)