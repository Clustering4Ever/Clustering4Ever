package clustering4ever.spark.clustering.kmodes

import _root_.scala.collection.mutable
import _root_.clustering4ever.math.distances.BinaryDistance
import _root_.clustering4ever.clustering.CommonRDDPredictClusteringModel
import _root_.org.apache.spark.rdd.RDD

/**
 * @author Beck Gaël
 **/
class KModesModel(centers: mutable.HashMap[Int, Array[Int]], metric: BinaryDistance) extends CommonRDDPredictClusteringModel(centers, metric)