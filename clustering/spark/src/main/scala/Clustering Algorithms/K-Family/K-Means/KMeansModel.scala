package clustering4ever.spark.clustering.kmeans

import scala.collection.{mutable, immutable}
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.clustering.CommonRDDPredictClusteringModel
import org.apache.spark.rdd.RDD

/**
 * @author Beck Gaël
 **/
class KMeansModel(val centers: mutable.HashMap[Int, immutable.Seq[Double]], val metric: ContinuousDistance[immutable.Seq[Double]]) extends CommonRDDPredictClusteringModel[immutable.Seq[Double], ContinuousDistance[immutable.Seq[Double]]]