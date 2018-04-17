package clustering4ever.scala.clustering.kmodes

import _root_.clustering4ever.clustering.CommonPredictClusteringModel
import _root_.scala.collection.mutable
import _root_.clustering4ever.math.distances.BinaryDistance
import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes

/**
 * @author Beck Gaël
 **/
class KModesModel(centers: mutable.HashMap[Int, Array[Int]], val cardinalities: mutable.HashMap[Int, Int], metric: BinaryDistance) extends CommonPredictClusteringModel[Array[Int]](centers, metric)