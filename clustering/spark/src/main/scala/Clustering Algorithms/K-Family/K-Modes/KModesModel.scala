package clustering4ever.spark.clustering.kmodes

import scala.collection.{mutable, immutable}
import clustering4ever.math.distances.BinaryDistance
import clustering4ever.clustering.CommonRDDPredictClusteringModel
import org.apache.spark.rdd.RDD

/**
 * @author Beck Gaël
 **/
class KModesModel(val centers: mutable.HashMap[Int, Seq[Int]], val metric: BinaryDistance[Seq[Int]]) extends CommonRDDPredictClusteringModel[Seq[Int]]