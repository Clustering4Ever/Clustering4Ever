package clustering4ever.scala.clustering.kmeans
/**
 * @author Beck Gaël
 */
import scala.collection.{mutable, immutable}
import clustering4ever.clustering.CommonPredictClusteringModel
import clustering4ever.math.distances.{ContinuousDistance, Distance}
import clustering4ever.scala.clusterizables.RealClusterizable
import scala.reflect.ClassTag
import clustering4ever.scala.clustering.KCommonsModel

final class KMeansModel[
	ID: Numeric,
	O,
	V <: Seq[Double] : ClassTag,
	Cz <: RealClusterizable[ID, O, V],
	D <: ContinuousDistance[V]
](centers: mutable.HashMap[Int, V], metric: D) extends KCommonsModel[ID, V, D, Cz](centers, metric)
