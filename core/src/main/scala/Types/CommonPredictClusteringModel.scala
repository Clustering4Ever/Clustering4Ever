package clustering4ever.clustering

import _root_.clustering4ever.math.distances.Distance
import _root_.scala.collection.mutable

/**
 * @author Beck Gaël
 **/
abstract class CommonPredictClusteringModel[T](val centers: mutable.HashMap[Int, T], val metric: Distance[T]) extends ClusteringModel
{
	type ClusterID = Int
	/**
	 * Return the nearest mode for a specific point
	 **/
	def predict(v: T): ClusterID =
	{
		centers.map{ case(clusterID, centroid) => (clusterID, metric.d(centroid, v)) }.minBy(_._2)._1
	}
	/**
	 * Return the nearest mode for a dataset
	 **/
	def predict(data: Seq[T]): Seq[(ClusterID, T)] =
	{
		data.map( v => (predict(v), v) )
	}
}