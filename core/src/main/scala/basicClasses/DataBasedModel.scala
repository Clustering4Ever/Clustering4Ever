package clustering4ever.scala.clustering
/**
 * @author Beck Gaël
 */
import clustering4ever.clustering.ClusteringModel
import clustering4ever.math.distances.Distance
import scala.collection.{GenSeq, mutable}
import clustering4ever.clustering.CommonPredictClusteringModel

class DataBasedModel[O, ID](val data: mutable.HashMap[Int, mutable.HashSet[(ID, O)]], metric: Distance[O]) extends ClusteringModel {
	/**
	 *
	 */
	lazy val dataAsSeq = data.toSeq.flatMap{ case (clusterID, values) => values.map{ case (id, vector) => (clusterID, (id, vector)) } }
	/**
	 *
	 */
	def knnPredict(obj: O, k: Int): Int = dataAsSeq.sortBy{ case (_, (_, obj2)) => metric.d(obj, obj2) }.take(k).map(_._1).groupBy(identity).maxBy{ case (clusterID, aggregate) => aggregate.size }._1
	/**
	 *
	 */
	def knnPredict(seq: GenSeq[O], k: Int): GenSeq[(Int, O)] = seq.map( obj => (knnPredict(obj, k), obj) )
}