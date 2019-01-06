package org.clustering4ever.clustering.models
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import org.apache.spark.rdd.RDD
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.vectors.GVector
import org.clustering4ever.clusterizables.Clusterizable
/**
 *
 */
trait CenterOrientedModelDistributed[O, D <: Distance[O]] extends CenterOrientedModel[O, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	def centerPredict(data: RDD[O]): RDD[(ClusterID, O)] = data.map( v => (centerPredict(v), v) )
}
/**
 *
 */
trait CenterOrientedModelDistributedCz[
	V <: GVector[V],
	D <: Distance[V]
] extends CenterOrientedModelDistributed[V, D] with CenterOrientedModelCz[V, D] {
	/**
	 *
	 */
	type CollectionType[X] = RDD[X]
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	def centerPredictCz[ID, O, Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz]](data: RDD[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): RDD[Cz[ID, O, V]] = data.map( cz => cz.addClusterIDs(centerPredictCz(cz)) )
}