package clustering4ever.spark.clustering.kmodes
/**
 * @author Beck Gaël
 **/
import scala.language.higherKinds
import scala.collection.mutable
import clustering4ever.spark.clustering.KCommonsModelSpark
import scala.reflect.ClassTag
import clustering4ever.scala.clusterizables.BinaryClusterizable
import clustering4ever.math.distances.BinaryDistance
/**
 *
 */
class KModesModel[
	ID: Numeric,
	O,
	V[Int] <: Seq[Int],
	Cz[ID, O, V[Int] <: Seq[Int]] <: BinaryClusterizable[ID, O, V[Int], Cz[ID, O, V]],
	D[V[Int] <: Seq[Int]] <: BinaryDistance[V]
](centers: mutable.HashMap[Int, V[Int]], metric: D[V])(implicit ev: ClassTag[Cz[ID, O, V]]) extends KCommonsModelSpark[ID, V[Int], D[V], Cz[ID, O, V]](centers, metric)