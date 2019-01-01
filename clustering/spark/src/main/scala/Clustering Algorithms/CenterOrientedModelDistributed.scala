package org.clustering4ever.clustering
/**
 * @author Beck Gaël
 **/
import scala.language.higherKinds
import org.clustering4ever.math.distances.Distance
import scala.collection.{mutable, immutable}
import org.apache.spark.rdd.RDD
/**
 *
 */
trait FusionedModelsDistributed[O, D <: Distance[O]] extends CenterOrientedModel[O, D] with KnnOrientedModel[O, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	def centerPredict(data: RDD[O]): RDD[(ClusterID, O)] = data.map( v => (centerPredict(v), v) )
	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input Seq with labels obtain via knnPredict method
	 */
	def knnPredict(data: RDD[O], k: Int, trainDS: Seq[(ClusterID, O)]): RDD[(ClusterID, O)] = data.map( v => (knnPredict(v, k, trainDS), v) )

}
