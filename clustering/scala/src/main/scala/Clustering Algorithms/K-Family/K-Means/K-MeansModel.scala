package clustering4ever.scala.clustering.kmeans
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{mutable, immutable}
import clustering4ever.clustering.CentersBasedModel
import clustering4ever.math.distances.{ContinuousDistance, Distance}
import clustering4ever.scala.clusterizables.RealClusterizable
import clustering4ever.scala.clustering.KCommonsModel
/**
 *
 */
final class KMeansModel[
	ID: Numeric,
	O,
	V[Double] <: Seq[Double],
	Cz <: RealClusterizable[ID, O, V[Double], Cz],
	D <: ContinuousDistance[V]
](centers: mutable.HashMap[Int, V[Double]], metric: D) extends KCommonsModel[ID, V[Double], D, Cz](centers, metric)
