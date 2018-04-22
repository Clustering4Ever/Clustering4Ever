package clustering4ever.clustering

import _root_.clustering4ever.math.distances.Distance
import _root_.scala.collection.mutable
import _root_.org.apache.spark.rdd.RDD

/**
 * @author Beck Gaël
 **/
abstract class CommonRDDPredictClusteringModel[T](centers: mutable.HashMap[Int, T], metric: Distance[T]) extends CommonPredictClusteringModel(centers, metric)
{
	/**
	 * Return the nearest mode for a dataset
	 **/
	def predict(data: RDD[T]): RDD[(ClusterID, T)] =
	{
		data.map( v => (predict(v), v) )
	}
}