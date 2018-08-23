package clustering4ever.scala.clustering.kmeans

import scala.collection.{mutable, immutable, GenSeq}
import clustering4ever.clustering.CommonPredictClusteringModel
import clustering4ever.math.distances.{ContinuousDistance, Distance}
import clustering4ever.scala.clusterizables.RealClusterizable
import scala.reflect.ClassTag
import clustering4ever.scala.clustering.KCommonsModel

/**
 * @author Beck Gaël
 */
final class KMeansModel[ID: Numeric, Obj, V <: Seq[Double] : ClassTag, Rc <: RealClusterizable[ID, Obj, V], D <: ContinuousDistance[V]](centers: mutable.HashMap[Int, V], metric: D) extends KCommonsModel[ID, V, D, Rc](centers, metric)
