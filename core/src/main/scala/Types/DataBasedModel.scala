package clustering4ever.scala.clustering

import clustering4ever.clustering.ClusteringModel
import clustering4ever.math.distances.Distance
import scala.collection.GenSeq

/**
 * @author Beck Gaël
 **/
class DataBasedModel[Obj, ID](val data: Seq[(Int, (ID, Obj))], metric: Distance[Obj]) extends ClusteringModel
{
	def knnPredict(obj: Obj, k: Int): Int =
		data.sortBy{ case (_, (_, obj2)) => metric.d(obj, obj2) }.take(k).map(_._1).groupBy(identity).map{ case (clusterID, aggregate) => (clusterID, aggregate.size) }.maxBy(_._2)._1

	def knnPredict(seq: GenSeq[Obj], k: Int): GenSeq[(Int, Obj)] = seq.map( obj => (knnPredict(obj, k), obj) )
}