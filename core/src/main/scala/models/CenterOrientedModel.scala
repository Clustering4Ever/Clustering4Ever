package org.clustering4ever.clustering.models
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import org.clustering4ever.math.distances.{GenericDistance, Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import scala.collection.{mutable, GenSeq}
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixtVector}
import org.clustering4ever.clusterizables.Clusterizable
/**
 *
 */
trait GenericCenterOrientedModel[O, D <: GenericDistance[O]] extends GenericMetricModel[O, D] {
	/**
	 *
	 */
	val centers: mutable.HashMap[ClusterID, O]
	/**
	 * Time complexity O(c) with c the number of clusters
	 * @return the clusterID of nearest cluster center for a specific point
	 */
	def centerPredict(o: O): ClusterID = centers.minBy{ case(_, centroid) => metric.d(centroid, o) }._1
}
/**
 *
 */
trait CenterOrientedModel[V <: GVector[V], D <: Distance[V]] extends GenericCenterOrientedModel[V, D]
/**
 *
 */
trait CenterOrientedModelReal[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]] extends CenterOrientedModel[ScalarVector[V], D[V]] {
	/**
	 * Time complexity O(c) with c the number of clusters
	 * @return the clusterID of nearest cluster center for a specific point
	 */
	def centerPredict(v: V): ClusterID = centerPredict(v)
}
/**
 *
 */
trait CenterOrientedModelBinary[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X]] extends CenterOrientedModel[BinaryVector[V], D[V]] {
	/**
	 * Time complexity O(c) with c the number of clusters
	 * @return the clusterID of nearest cluster center for a specific point
	 */
	def centerPredict(v: V): ClusterID = centerPredict(v)
}
/**
 *
 */
trait CenterOrientedModelMixt[Vb <: Seq[Int], Vs <: Seq[Double], D[X <: Seq[Int], Y <: Seq[Double]] <: MixtDistance[X, Y]] extends CenterOrientedModel[MixtVector[Vb, Vs], D[Vb, Vs]] {
	/**
	 * Time complexity O(c) with c the number of clusters
	 * @return the clusterID of nearest cluster center for a specific point
	 */
	def centerPredict(v: (Vb, Vs)): ClusterID = centerPredict(v)
}
/**
 *
 */
trait CenterOrientedModelCz[V <: GVector[V], D <: Distance[V]] extends CenterOrientedModel[V, D] {
	/**
	 * Time complexity O(c) with c the number of clusters
	 * @return the clusterID of nearest cluster center for a specific point
	 */
	def centerPredict[ID, O, Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz]](cz: Cz[ID, O, V]): ClusterID = centerPredict(cz.v)

}
/**
 *
 */
trait GenericCenterOrientedModelLocal[O, D <: GenericDistance[O]] extends GenericCenterOrientedModel[O, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	def centerPredict[GS[X] <: GenSeq[X]](data: GS[O]): GS[(ClusterID, O)] = data.map( v => (centerPredict(v), v) ).asInstanceOf[GS[(ClusterID, O)]]
}
/**
 *
 */
trait CenterOrientedModelLocal[V <: GVector[V], D <: Distance[V]] extends GenericCenterOrientedModelLocal[V, D]
/**
 *
 */
trait CenterOrientedModelLocalClusterizable[V <: GVector[V], D <: Distance[V]] extends CenterOrientedModelLocal[V, D] with CenterOrientedModelCz[V, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	def centerPredict[ID, O, Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, V]])(implicit d: DummyImplicit): GS[Cz[ID, O, V]] = {
		data.map( cz => cz.addClusterIDs(centerPredict(cz)) ).asInstanceOf[GS[Cz[ID, O, V]]]
	}
}