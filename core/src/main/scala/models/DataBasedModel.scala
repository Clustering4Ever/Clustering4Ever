package clustering4ever.scala.clustering
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{GenSeq, mutable}
import spire.math.{Numeric => SNumeric}
import clustering4ever.clustering.ClusteringModel
import clustering4ever.math.distances.{Distance, DistanceSeq, ContinuousDistance, BinaryDistance}
/**
 *
 */
trait DataBasedModel[@specialized(Int, Long) ID, O, D <: Distance[O]] extends ClusteringModel {

	val data: scala.collection.Map[Int, scala.collection.Traversable[(ID, O)]]
	val metric: D
	/**
	 *
	 */
	lazy val dataAsSeq: Seq[(Int, (ID, O))] = data.toSeq.flatMap{ case (clusterID, values) => values.map{ case (id, vector) => (clusterID, (id, vector)) } }
	/**
	 * @return clusterID associate to obj
	 */
	def knnPredict(obj: O, k: Int): ClusterID = dataAsSeq.sortBy{ case (_, (_, obj2)) => metric.d(obj, obj2) }.take(k).map(_._1).groupBy(identity).maxBy{ case (clusterID, aggregate) => aggregate.size }._1
	/**
	 * @return clusterID associate to obj and its knn containing (ClusterID, (ID, obj))
	 */
	def knn(obj: O, k: Int): (ClusterID, Seq[(ClusterID, (ID, O))]) = dataAsSeq.sortBy{ case (_, (_, obj2)) => metric.d(obj, obj2) }.take(k).groupBy(_._1).maxBy{ case (clusterID, aggregate) => aggregate.size }
	/**
	 * @return clusterID associate to a GenSeq of object
	 */
	def knnPredict(genSeq: GenSeq[O], k: Int): GenSeq[(ClusterID, O)] = genSeq.map( obj => (knnPredict(obj, k), obj) )
}
/**
 *
 */
abstract class DataBasedModelSeq[ID: Numeric, @specialized(Int, Double) N: SNumeric, V <: Seq[N], D[V <: Seq[N]] <: DistanceSeq[N, V]] extends DataBasedModel[ID, V, D[V]]
/**
 *
 */
abstract class DataBasedModelSeqReal[ID: Numeric, V <: Seq[Double], D[V <: Seq[Double]] <: ContinuousDistance[V]] extends DataBasedModel[ID, V, D[V]]
/**
 *
 */
abstract class DataBasedModelSeqBinary[ID: Numeric, V <: Seq[Int], D[V <: Seq[Int]] <: BinaryDistance[V]] extends DataBasedModel[ID, V, D[V]]