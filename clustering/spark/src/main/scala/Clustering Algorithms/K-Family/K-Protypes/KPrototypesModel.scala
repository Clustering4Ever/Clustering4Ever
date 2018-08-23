package clustering4ever.spark.clustering.kprototypes

import scala.collection.mutable
import org.apache.spark.rdd.RDD
import clustering4ever.clustering.CommonRDDPredictClusteringModel
import clustering4ever.math.distances.MixtDistance
import clustering4ever.scala.measurableclass.BinaryScalarVector


/**
 * @author Beck Gaël
 **/
class KPrototypesModel(val centers: mutable.HashMap[Int, BinaryScalarVector], val metric: MixtDistance) extends CommonRDDPredictClusteringModel[BinaryScalarVector]