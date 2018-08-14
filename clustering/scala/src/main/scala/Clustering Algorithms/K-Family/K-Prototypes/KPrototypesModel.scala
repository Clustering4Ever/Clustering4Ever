package clustering4ever.scala.clustering.kprotoypes

import scala.collection.mutable
import clustering4ever.clustering.ClusteringModel
import clustering4ever.math.distances.MixtDistance
import clustering4ever.clustering.datasetstype.DataSetsTypes
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.clustering.CommonPredictClusteringModel

/**
 * @author Beck Gaël
 **/
class KPrototypesModel(centers: mutable.HashMap[Int, BinaryScalarVector], metric: MixtDistance) extends CommonPredictClusteringModel[BinaryScalarVector](centers, metric)