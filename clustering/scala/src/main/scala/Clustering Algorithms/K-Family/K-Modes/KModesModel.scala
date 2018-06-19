package clustering4ever.scala.clustering.kmodes

import _root_.clustering4ever.clustering.CommonPredictClusteringModel
import _root_.scala.collection.mutable
import _root_.clustering4ever.math.distances.BinaryDistance
import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes

/**
 * @author Beck Gaël
 **/
class KModesModel(centers: mutable.HashMap[Int, Vector[Int]], metric: BinaryDistance) extends CommonPredictClusteringModel[Vector[Int]](centers, metric)