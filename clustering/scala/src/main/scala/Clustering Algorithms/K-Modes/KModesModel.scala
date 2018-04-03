package clustering4ever.scala.clustering.kmodes

import _root_.clustering4ever.clustering.ClusteringModel
import _root_.scala.collection.mutable
import _root_.clustering4ever.math.distances.BinaryDistance
import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes

/**
 * @author Beck Gaël
 **/
class KModesModel(val centroids: mutable.HashMap[Int, Array[Int]], val cardinalities: mutable.HashMap[Int, Int], val metric: BinaryDistance) extends ClusteringModel with DataSetsTypes[Int, Int]
{
	/**
	 * Return the nearest mode for a specific point
	 **/
	def predict(v: Array[Int]): ClusterID =
	{
		centroids.toArray.map{ case(clusterID, centroid) => (clusterID, metric.d(centroid, v)) }.sortBy(_._2).head._1
	}

	/**
	 * Return the nearest mode for a dataset
	 **/
	def predict(data: Array[Array[Int]]): Array[(ClusterID, Vector)] =
	{
		val centroidsAsArray = centroids.toArray

		def predictCluster(v: Array[Int]) =
		{
			centroidsAsArray.map{ case(clusterID, centroid) => (clusterID, metric.d(centroid, v)) }.sortBy(_._2).head._1
		}

		data.map( v => (predictCluster(v), v) )
	}
}