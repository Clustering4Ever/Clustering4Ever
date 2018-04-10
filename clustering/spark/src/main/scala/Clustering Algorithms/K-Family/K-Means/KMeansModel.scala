package clustering4ever.spark.clustering.kmeans

import _root_.clustering4ever.clustering.ClusteringModel
import _root_.scala.collection.mutable
import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes
import _root_.org.apache.spark.rdd.RDD

/**
 * @author Beck Gaël
 **/
class KMeansModel(val centroids: mutable.HashMap[Int, Array[Double]], val cardinalities: mutable.HashMap[Int, Long], val metric: ContinuousDistances) extends ClusteringModel with DataSetsTypes[Long, Array[Double]]
{
	val centroidsAsArray = centroids.toArray
	/**
	 * Return the nearest mode for a specific point
	 **/
	def predict(v: Array[Double]): ClusterID = {
		centroidsAsArray.map{ case(clusterID, centroid) => (clusterID, metric.d(centroid, v)) }.minBy(_._2)._1
	}
	/**
	 * Return the nearest mode for a dataset
	 **/
	def predict(data: RDD[Array[Double]]): RDD[(ClusterID, Vector)] = {
		data.map( v => (predict(v), v) )
	}
}