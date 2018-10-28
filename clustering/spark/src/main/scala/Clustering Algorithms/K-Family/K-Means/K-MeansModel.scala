package clustering4ever.spark.clustering.kmeans
/**
 * @author Beck Gaël
 **/
import scala.language.higherKinds
import scala.collection.{mutable, immutable}
import org.apache.spark.rdd.RDD
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.spark.clustering.KCommonsModelSpark
import clustering4ever.math.distances.{ContinuousDistance, Distance}
import clustering4ever.scala.clusterizables.RealClusterizable
import scala.reflect.ClassTag
/**
 *
 */
final class KMeansModel[
	ID: Numeric,
	O,
	V[Double] <: Seq[Double],
	Cz <: RealClusterizable[ID, O, V[Double], Cz],
	D <: ContinuousDistance[V]
](centers: mutable.HashMap[Int, V[Double]], metric: D)(implicit ev: ClassTag[Cz]) extends KCommonsModelSpark[ID, V[Double], D, Cz](centers, metric)