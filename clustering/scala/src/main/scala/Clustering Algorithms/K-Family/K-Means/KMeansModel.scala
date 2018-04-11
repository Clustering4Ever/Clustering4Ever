package clustering4ever.scala.clustering.kmeans

import _root_.clustering4ever.clustering.ClusteringModel
import _root_.scala.collection.mutable
import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes

/**
 * @author Beck Gaël
 **/
class KMeansModel(val centers: mutable.HashMap[Int, Array[Double]], val cardinalities: mutable.HashMap[Int, Int], val metric: ContinuousDistances) extends ClusteringModel with DataSetsTypes[Int, Array[Double]]
{
	val centersAsArray = centers.toArray
	/**
	 * Return the nearest mode for a specific point
	 **/
	def predict(v: Array[Double]): ClusterID =
	{
		centersAsArray.map{ case(clusterID, centroid) => (clusterID, metric.d(centroid, v)) }.minBy(_._2)._1
	}

	/**
	 * Return the nearest mode for a dataset
	 **/
	def predict(data: Seq[Array[Double]]): Seq[(ClusterID, Vector)] =
	{
		data.map( v => (predict(v), v) )
	}
}