package clustering4ever.clustering
/**
 * @author Beck Gaël
 */
import clustering4ever.math.distances.{Distance, ClusterizableDistance}
import scala.collection.{mutable, GenSeq}
/**
 *
 */
trait CenterOrientedModel[V, D <: Distance[V]] extends ClusteringModel {
	/**
	 *
	 */
	val centers: mutable.HashMap[ClusterID, V]
	/**
	 *
	 */
	val metric: D
	/**
	 * Time complexity O(c) with c the number of clusters
	 * @return the clusterID of nearest cluster center for a specific point
	 */
	def centerPredict(v: V): ClusterID = centers.minBy{ case(_, centroid) => metric.d(centroid, v) }._1
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	def centerPredict(data: GenSeq[V]): GenSeq[(ClusterID, V)] = data.map( v => (centerPredict(v), v) )
	/**
	 * Time complexity O(n<sub>trainDS</sub>)
	 * @return the clusterID of cluster which has the most number of vectors closest from a specific point among its k nearest neighbors
	 */
	def knnPredict(v: V, k: Int, trainDS: Seq[(ClusterID, V)]): ClusterID = trainDS.sortBy{ case (_, vTrain) => metric.d(vTrain, v) }.take(k).map(_._1).groupBy(identity).maxBy(_._2.size)._1
	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input Seq with labels obtain via knnPredict method
	 */
	def knnPredict(data: GenSeq[V], k: Int, trainDS: Seq[(ClusterID, V)]): GenSeq[(ClusterID, V)] = data.map( v => (knnPredict(v, k, trainDS), v) )

}