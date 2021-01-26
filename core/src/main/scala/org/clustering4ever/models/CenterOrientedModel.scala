package org.clustering4ever.models

/**
 * @author Beck Gaël
 */
import org.clustering4ever.distances._
import org.clustering4ever.roottraits._

import scala.collection.{GenSeq, immutable}
import scala.language.higherKinds
/**
 * @tparam V
 * @tparam D
 */
trait CenterModel[V <: GVector[V], D <: Distance[V]] extends MetricModel[V, D] {
	/**
	 * Prototypes of clusters, ie elements which minimize distances to each members of their respective clusters
	 */
	val centers: immutable.HashMap[ClusterID, V]
	/**
	 * Time complexity O(c) with c the number of clusters
	 * @return the clusterID of nearest cluster center for a specific point
	 */
	final def centerPredict(v: V): ClusterID = centers.minBy{ case(_, centroid) => metric.d(centroid, v) }._1
}
/**
 * @tparam T
 * @tparam V
 * @tparam SV
 * @tparam D
 */
trait CenterModelSimpleV[N, SV <: GSimpleVector[N, SV], D <: GSimpleVectorDistance[N, SV]] extends CenterModel[SV, D] {
	/**
	 * Time complexity O(c) with c the number of clusters
	 * @return the clusterID of nearest cluster center for a specific point
	 */
	final def centerPredict(v: Array[N]): ClusterID = centers.minBy{ case(_, centroid) => metric.dRaw(centroid.vector, v) }._1	
}
/**
 * @tparam V
 * @tparam D
 */
trait CenterModelReal[D <: ContinuousDistance] extends CenterModelSimpleV[Double, ScalarVector, D]
/**
 * @tparam V
 * @tparam D
 */
trait CenterModelBinary[D <: BinaryDistance] extends CenterModelSimpleV[Int, BinaryVector, D]
/**
 * @tparam Vs
 * @tparam Vb
 * @tparam D
 */
trait CenterModelMixed[D <: MixedDistance] extends CenterModel[MixedVector, D] {
	/**
	 * Time complexity O(c) with c the number of clusters
	 * @return the clusterID of nearest cluster center for a specific point
	 */
	final def centerPredict(v: (Array[Int], Array[Double])): ClusterID = centers.minBy{ case(_, centroid) => metric.dRaw((centroid.binary, centroid.scalar), v) }._1
}
/**
 * @tparam V
 * @tparam D
 */
trait CenterModelCz[V <: GVector[V], D <: Distance[V]] extends CenterModel[V, D] {
	/**
	 * Time complexity O(c) with c the number of clusters
	 * @return the clusterID of nearest cluster center for a specific point
	 */
	final def centerPredict[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz]](cz: Cz[O, V]): ClusterID = centerPredict(cz.v)

}
/**
 * @tparam V
 * @tparam D
 */
trait CenterModelLocal[V <: GVector[V], D <: Distance[V]] extends CenterModel[V, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	final def centerPredict[GS[X] <: GenSeq[X]](data: GS[V]): GS[(ClusterID, V)] = data.map( v => (centerPredict(v), v) ).asInstanceOf[GS[(ClusterID, V)]]
}
/**
 * @tparam T
 * @tparam V
 * @tparam SV
 * @tparam D
 */
trait CenterModelSimpleVLocal[N, SV <: GSimpleVector[N, SV], D <: GSimpleVectorDistance[N, SV]] extends CenterModelSimpleV[N, SV, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	final def centerPredict[GS[X] <: GenSeq[X]](data: GS[Array[N]])(implicit d1: DummyImplicit, d2: DummyImplicit): GS[(ClusterID, Array[N])] = data.map( v => (centerPredict(v), v) ).asInstanceOf[GS[(ClusterID, Array[N])]]
}
/**
 * @tparam Vb
 * @tparam Vs
 * @tparam D
 */
trait CenterModelMixedLocal[D <: MixedDistance] extends CenterModelMixed[D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	final def centerPredict[GS[X] <: GenSeq[X]](data: GS[(Array[Int], Array[Double])])(implicit d1: DummyImplicit, d2: DummyImplicit): GS[(ClusterID, (Array[Int], Array[Double]))] = data.map( v => (centerPredict(v), v) ).asInstanceOf[GS[(ClusterID, (Array[Int], Array[Double]))]]
}
/**
 * @tparam V
 * @tparam D
 */
trait CenterModelLocalReal[D <: ContinuousDistance] extends CenterModelSimpleVLocal[Double, ScalarVector, D]
/**
 * @tparam V
 * @tparam D
 */
trait CenterModelLocalBinary[D <: BinaryDistance] extends CenterModelSimpleVLocal[Int, BinaryVector, D]
/**
 * @tparam V
 * @tparam D
 */
trait CenterModelLocalCz[V <: GVector[V], D <: Distance[V]] extends CenterModelLocal[V, D] with CenterModelCz[V, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	final def centerPredict[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]])(implicit d: DummyImplicit): GS[Cz[O, V]] = {
		data.map( cz => cz.addClusterIDs(centerPredict(cz)) ).asInstanceOf[GS[Cz[O, V]]]
	}
}