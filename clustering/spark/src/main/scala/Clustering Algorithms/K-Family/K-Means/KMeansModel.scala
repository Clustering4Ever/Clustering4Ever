package clustering4ever.spark.clustering.kmeans

import scala.collection.{mutable, immutable}
import org.apache.spark.rdd.RDD
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.spark.clustering.KCommonsModelSpark
import clustering4ever.math.distances.{ContinuousDistance, Distance}
import clustering4ever.scala.clusterizables.RealClusterizable
import scala.reflect.ClassTag

/**
 * @author Beck Gaël
 **/
final class KMeansModel[ID: Numeric, Obj, V <: Seq[Double], Cz <: RealClusterizable[ID, Obj, V] : ClassTag](centers: mutable.HashMap[Int, V], metric: ContinuousDistance[V]) extends KCommonsModelSpark[ID, V, ContinuousDistance[V], Cz](centers, metric)