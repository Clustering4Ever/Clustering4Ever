package clustering4ever.spark.clustering.kmodes

import _root_.scala.collection.{mutable, immutable}
import _root_.clustering4ever.math.distances.BinaryDistance
import _root_.clustering4ever.clustering.CommonRDDPredictClusteringModel
import _root_.org.apache.spark.rdd.RDD

/**
 * @author Beck Gaël
 **/
class KModesModel(centers: mutable.HashMap[Int, immutable.Seq[Int]], metric: BinaryDistance[immutable.Seq[Int]]) extends CommonRDDPredictClusteringModel(centers, metric)