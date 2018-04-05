package clustering4ever.scala.clustering.kmeans

import _root_.clustering4ever.clustering.ClusteringModel
import _root_.scala.collection.mutable
import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes

/**
 * @author Beck Gaël
 **/
class GaussianMixturesModel(val centroids: mutable.HashMap[Int, Array[Double]], val cardinalities: mutable.HashMap[Int, Int], val metric: ContinuousDistances) extends ClusteringModel with DataSetsTypes[Int, Double]
{
	/**
	 * Return the nearest mode for a specific point
	 **/
	def predict(v: Array[Double]): ClusterID = {
		centroids.toArray.map{ case(clusterID, centroid) => (clusterID, metric.d(centroid, v)) }.sortBy(_._2).head._1
	}

	/**
	 * Return the nearest mode for a dataset
	 **/
	def predict(data: Seq[Array[Double]]): Seq[(ClusterID, Vector)] = {
		val centroidsAsArray = centroids.toArray

		def predictCluster(v: Array[Double]) = {
			centroidsAsArray.map{ case(clusterID, centroid) => (clusterID, metric.d(centroid, v)) }.sortBy(_._2).head._1
		}

		data.map( v => (predictCluster(v), v) )
	}
}