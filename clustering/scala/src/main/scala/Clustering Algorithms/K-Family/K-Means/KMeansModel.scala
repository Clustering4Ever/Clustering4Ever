package clustering4ever.scala.clustering.kmeans

import _root_.clustering4ever.clustering.CommonPredictClusteringModel
import _root_.scala.collection.mutable
import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes

/**
 * @author Beck Gaël
 **/
class KMeansModel(centers: mutable.HashMap[Int, Array[Double]], metric: ContinuousDistances) extends CommonPredictClusteringModel[Array[Double]](centers, metric)