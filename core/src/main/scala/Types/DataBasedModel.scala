package clustering4ever.scala.clustering

import clustering4ever.clustering.ClusteringModel
import clustering4ever.math.distances.Distance
import scala.collection.{GenSeq, mutable}

/**
 * @author Beck Gaël
 **/
class DataBasedModel[Obj, ID](val data: mutable.HashMap[Int, mutable.HashSet[(ID, Obj)]], metric: Distance[Obj]) extends ClusteringModel
{

	lazy val dataAsSeq = data.toSeq.flatMap{ case (clusterID, values) => values.map{ case (id, vector) => (clusterID, (id, vector)) } }

	def knnPredict(obj: Obj, k: Int): Int =
		dataAsSeq.sortBy{ case (_, (_, obj2)) => metric.d(obj, obj2) }.take(k).map(_._1).groupBy(identity).maxBy{ case (clusterID, aggregate) => aggregate.size }._1

	def knnPredict(seq: GenSeq[Obj], k: Int): GenSeq[(Int, Obj)] = seq.map( obj => (knnPredict(obj, k), obj) )
}