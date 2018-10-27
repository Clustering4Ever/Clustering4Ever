package clustering4ever.spark.clustering.kprototypes
/**
 * @author Beck Gaël
 */
import scala.collection.mutable
import scala.reflect.ClassTag
import org.apache.spark.rdd.RDD
import clustering4ever.clustering.CommonRDDPredictClusteringModel
import clustering4ever.math.distances.MixtDistance
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.scala.clusterizables.MixtClusterizable
import clustering4ever.spark.clustering.KCommonsModelSpark
/**
 *
 */
class KPrototypesModel[
	ID: Numeric,
	O,
	Vb <: Seq[Int],
	Vs <: Seq[Double],
	Cz <: MixtClusterizable[ID, O, Vb, Vs, Cz] : ClassTag,
	D <: MixtDistance[Vb, Vs]
](centers: mutable.HashMap[Int, BinaryScalarVector[Vb, Vs]], metric: D) extends KCommonsModelSpark[ID, BinaryScalarVector[Vb, Vs], D, Cz](centers, metric)