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
	 * Time complexity O(c) with c the number of clusters
	 * @return the clusterID of nearest cluster center for a specific point
	 **/
	def centerPredict(v: T): ClusterID =
	{
		centers.map{ case(clusterID, centroid) => (clusterID, metric.d(centroid, v)) }.minBy(_._2)._1
	}
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 **/
	def centerPredict(data: Seq[T]): Seq[(ClusterID, T)] =
	{
		data.map( v => (centerPredict(v), v) )
	}
	/**
	 * Time complexity O(n<sub>trainDS</sub>)
	 * @return the clusterID of cluster which has the most number of vectors closest from a specific point among its k nearest neighbors
	 **/
	def knnPredict(v: T, k: Int, trainDS: Array[(ClusterID, T)]): ClusterID =
	{
		trainDS.map{ case (clusterID, vTrain) => (clusterID, metric.d(vTrain, v)) }.sortBy(_._2).take(k).map(_._1).groupBy(identity).maxBy(_._2.size)._1
	}
	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input Seq with labels obtain via knnPredict method
	 **/
	def knnPredict(data: Seq[T], k: Int, trainDS: Array[(ClusterID, T)]): Seq[(ClusterID, T)] =
	{
		data.map( v => (knnPredict(v, k, trainDS), v) )
	}
}