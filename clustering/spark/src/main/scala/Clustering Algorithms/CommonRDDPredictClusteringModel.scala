package clustering4ever.clustering

import clustering4ever.math.distances.Distance
import scala.collection.{mutable, immutable}
import org.apache.spark.rdd.RDD

/**
 * @author Beck Gaël
 **/
trait CommonRDDPredictClusteringModel[V, D <: Distance[V]] extends CommonPredictClusteringModel[V, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input RDD with labels obtain via centerPredict method
	 **/
	def centerPredict(data: RDD[V]): RDD[(ClusterID, V)] = data.map( v => (centerPredict(v), v) )

	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input RDD with labels obtain via knnPredict method
	 **/
	def knnPredict(data: RDD[V], k: Int, trainDS: Seq[(ClusterID, V)]): RDD[(ClusterID, V)] = data.map( v => (knnPredict(v, k, trainDS), v) )
}