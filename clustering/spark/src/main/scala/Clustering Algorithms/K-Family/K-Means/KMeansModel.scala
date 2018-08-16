package clustering4ever.spark.clustering.kmeans

import scala.collection.{mutable, immutable}
import clustering4ever.math.distances.ContinuousDistances
import clustering4ever.clustering.CommonRDDPredictClusteringModel
import org.apache.spark.rdd.RDD

/**
 * @author Beck Gaël
 **/
class KMeansModel(centers: mutable.HashMap[Int, Seq[Double]], metric: ContinuousDistances) extends CommonRDDPredictClusteringModel(centers, metric)