package org.clustering4ever.clustering
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import org.apache.spark.rdd.RDD
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.scala.vectors.GVector
import org.clustering4ever.scala.clusterizables.Clusterizable
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
trait CenterOrientedModelLocalClusterizable[
	V <: GVector,
	D <: Distance[V]
] extends CenterOrientedModelDistributed[V, D] with CenterOrientedModelClusterizable[V, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	def centerPredictCz[ID, O, Cz[A, B, C <: GVector] <: Clusterizable[A, B, C, Cz]](data: RDD[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): RDD[Cz[ID, O, V]] = data.map( cz => cz.addClusterID(centerPredictCz(cz)) )
}