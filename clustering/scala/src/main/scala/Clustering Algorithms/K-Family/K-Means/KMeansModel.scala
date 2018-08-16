package clustering4ever.scala.clustering.kmeans

import scala.collection.{mutable, immutable}
import clustering4ever.clustering.CommonPredictClusteringModel
import clustering4ever.math.distances.ContinuousDistances
import clustering4ever.clustering.datasetstype.DataSetsTypes

/**
 * @author Beck Gaël
 **/
class KMeansModel(centers: mutable.HashMap[Int, Seq[Double]], metric: ContinuousDistances) extends CommonPredictClusteringModel[Seq[Double]](centers, metric)