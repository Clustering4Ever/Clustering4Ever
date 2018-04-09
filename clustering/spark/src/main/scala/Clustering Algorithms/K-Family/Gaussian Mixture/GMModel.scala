package clustering4ever.spark.clustering.gaussianmixtures

import _root_.clustering4ever.clustering.ClusteringModel
import _root_.scala.collection.mutable
import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes
import _root_.org.apache.spark.rdd.RDD

/**
 * @author Beck Gaël
 **/
class GaussianMixturesModel(val centroids: mutable.HashMap[Int, Array[Double]], val cardinalities: mutable.HashMap[Int, Int], val metric: ContinuousDistances, val finalAffectation: RDD[(Int, Array[Double])]) extends ClusteringModel with DataSetsTypes[Int, Array[Double]]
{
	val centroidsAsArray = centroids.toArray
	/**
	 * Return the nearest mode for a specific point
	 **/
	def predict(v: Array[Double]): ClusterID = {
		centroidsAsArray.map{ case(clusterID, centroid) => (clusterID, metric.d(centroid, v)) }.sortBy(_._2).head._1
	}
	/**
	 * Return the nearest mode for a dataset
	 **/
	def predict(data: RDD[Array[Double]]): RDD[(ClusterID, Vector)] = {
		data.map( v => (predict(v), v) )
	}
}