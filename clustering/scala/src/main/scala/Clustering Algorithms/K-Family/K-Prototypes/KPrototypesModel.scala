package clustering4ever.scala.clustering.kprotoypes

import _root_.clustering4ever.clustering.ClusteringModel
import _root_.scala.collection.mutable
import _root_.clustering4ever.math.distances.MixtDistance
import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes
import _root_.clustering4ever.scala.measurableclass.BinaryScalarVector
import _root_.clustering4ever.clustering.CommonPredictClusteringModel

/**
 * @author Beck Gaël
 **/
class KPrototypesModel(centers: mutable.HashMap[Int, BinaryScalarVector], metric: MixtDistance) extends CommonPredictClusteringModel[BinaryScalarVector](centers, metric)