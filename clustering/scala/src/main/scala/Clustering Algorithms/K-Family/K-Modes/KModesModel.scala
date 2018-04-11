package clustering4ever.scala.clustering.kmodes

import _root_.clustering4ever.clustering.ClusteringModel
import _root_.scala.collection.mutable
import _root_.clustering4ever.math.distances.BinaryDistance
import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes

/**
 * @author Beck Gaël
 **/
class KModesModel(val centers: mutable.HashMap[Int, Array[Int]], val cardinalities: mutable.HashMap[Int, Int], val metric: BinaryDistance) extends ClusteringModel with DataSetsTypes[Int, Array[Int]]
{
	val centersAsArray = centers.toArray
	/**
	 * Return the nearest mode for a specific point
	 **/
	def predict(v: Array[Int]): ClusterID =
	{
		centersAsArray.map{ case(clusterID, centroid) => (clusterID, metric.d(centroid, v)) }.minBy(_._2)._1
	}
	/**
	 * Return the nearest mode for a dataset
	 **/
	def predict(data: Seq[Array[Int]]): Seq[(ClusterID, Vector)] =
	{
		data.map( v => (predict(v), v) )
	}
}