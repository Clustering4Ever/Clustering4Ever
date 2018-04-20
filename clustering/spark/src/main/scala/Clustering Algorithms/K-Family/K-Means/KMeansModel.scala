package clustering4ever.spark.clustering.kmeans

import _root_.scala.collection.mutable
import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.clustering4ever.clustering.CommonRDDPredictClusteringModel
import _root_.org.apache.spark.rdd.RDD

/**
 * @author Beck Gaël
 **/
class KMeansModel(centers: mutable.HashMap[Int, Array[Double]], metric: ContinuousDistances) extends CommonRDDPredictClusteringModel(centers, metric)