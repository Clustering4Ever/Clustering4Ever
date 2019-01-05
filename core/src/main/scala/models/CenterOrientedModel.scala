package org.clustering4ever.clustering.models
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import org.clustering4ever.math.distances.{Distance, ClusterizableDistance}
import scala.collection.{mutable, GenSeq}
import org.clustering4ever.scala.vectors.GVector
import org.clustering4ever.clusterizables.Clusterizable
/**
 *
 */
trait CenterOrientedModel[O, D <: Distance[O]] extends MetricModel[O, D] {
	/**
	 *
	 */
	val centers: mutable.HashMap[ClusterID, O]
	/**
	 * Time complexity O(c) with c the number of clusters
	 * @return the clusterID of nearest cluster center for a specific point
	 */
	def centerPredict(v: O): ClusterID = centers.minBy{ case(_, centroid) => metric.d(centroid, v) }._1
}
/**
 *
 */
trait CenterOrientedModelCz[V <: GVector[V], D <: Distance[V]] extends CenterOrientedModel[V, D] {
	/**
	 * Time complexity O(c) with c the number of clusters
	 * @return the clusterID of nearest cluster center for a specific point
	 */
	def centerPredictCz[ID, O, Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz]](cz: Cz[ID, O, V]): ClusterID = centerPredict(cz.v)

} 
/**
 *
 */
trait CenterOrientedModelLocal[O, D <: Distance[O]] extends CenterOrientedModel[O, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	def centerPredict[GS[X] <: GenSeq[X]](data: GS[O]): GS[(ClusterID, O)] = data.map( v => (centerPredict(v), v) ).asInstanceOf[GS[(ClusterID, O)]]
}
/**
 *
 */
trait CenterOrientedModelLocalClusterizable[V <: GVector[V], D <: Distance[V]] extends CenterOrientedModelLocal[V, D] with CenterOrientedModelCz[V, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	def centerPredictCz[ID, O, Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, V]]): GS[Cz[ID, O, V]] = data.map( cz => cz.addClusterID(centerPredictCz(cz)) ).asInstanceOf[GS[Cz[ID, O, V]]]
}