package clustering4ever.scala.clustering

import clustering4ever.clustering.ClusteringModel
import clustering4ever.math.distances.Distance

/**
 * @author Beck Gaël
 **/
class DataBasedModel[Obj, ID](val data: Seq[(Int, (ID, Obj))], val metric: Distance[Obj]) extends ClusteringModel
{
	def knnPredict(obj: Obj, k: Int): Int =
	{
		data.map{ case (clusterID, (id, obj2)) => (clusterID, metric.d(obj, obj2)) }.sortBy(_._2).take(k).map(_._1).groupBy(identity).map{ case (clusterID, aggregate) => (clusterID, aggregate.size) }.maxBy(_._2)._1
	}

	def knnPredict(seq: Seq[Obj], k: Int): Seq[(Int, Obj)] =
	{
		seq.map( obj => (knnPredict(obj, k), obj) )
	}
}