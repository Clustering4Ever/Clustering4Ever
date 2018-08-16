package clustering4ever.scala.clustering.kmodes

import scala.collection.{mutable, immutable}
import clustering4ever.clustering.CommonPredictClusteringModel
import clustering4ever.math.distances.BinaryDistance
import clustering4ever.clustering.datasetstype.DataSetsTypes

/**
 * @author Beck Gaël
 **/
class KModesModel(centers: mutable.HashMap[Int, Seq[Int]], metric: BinaryDistance[Seq[Int]]) extends CommonPredictClusteringModel[Seq[Int]](centers, metric)