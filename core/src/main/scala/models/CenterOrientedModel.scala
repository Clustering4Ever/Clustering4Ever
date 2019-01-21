package org.clustering4ever.clustering.models
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import org.clustering4ever.math.distances.{GSimpleVectorDistance, Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import scala.collection.{mutable, GenSeq}
import org.clustering4ever.vectors.{GVector, GSimpleVector,ScalarVector, BinaryVector, MixtVector}
import org.clustering4ever.clusterizables.Clusterizable
/**
 *
 */
trait CenterModel[V <: GVector[V], D <: Distance[V]] extends MetricModel[V, D] {
	/**
	 *
	 */
	val centers: mutable.HashMap[ClusterID, V]
	/**
	 * Time complexity O(c) with c the number of clusters
	 * @return the clusterID of nearest cluster center for a specific point
	 */
	def centerPredict(o: V): ClusterID = centers.minBy{ case(_, centroid) => metric.d(centroid, o) }._1
}
/**
 *
 */
trait CenterModelSimpleV[T, V <: Seq[T], SV <: GSimpleVector[T, V, SV], D <: GSimpleVectorDistance[T, V, SV]] extends CenterModel[SV, D] {
	/**
	 * Time complexity O(c) with c the number of clusters
	 * @return the clusterID of nearest cluster center for a specific point
	 */
	def centerPredict(v: V): ClusterID = centers.minBy{ case(_, centroid) => metric.d(centroid.vector, v) }._1	
}
/**
 *
 */
trait CenterModelReal[V <: Seq[Double], D <: ContinuousDistance[V]] extends CenterModelSimpleV[Double, V, ScalarVector[V], D]
/**
 *
 */
trait CenterModelBinary[V <: Seq[Int], D <: BinaryDistance[V]] extends CenterModelSimpleV[Int, V, BinaryVector[V], D]
/**
 *
 */
trait CenterModelMixt[Vb <: Seq[Int], Vs <: Seq[Double], D <: MixtDistance[Vb, Vs]] extends CenterModel[MixtVector[Vb, Vs], D] {
	/**
	 * Time complexity O(c) with c the number of clusters
	 * @return the clusterID of nearest cluster center for a specific point
	 */
	def centerPredict(v: (Vb, Vs)): ClusterID = centers.minBy{ case(_, centroid) => metric.d((centroid.binary, centroid.scalar), v) }._1
}
/**
 *
 */
trait CenterModelCz[V <: GVector[V], D <: Distance[V]] extends CenterModel[V, D] {
	/**
	 * Time complexity O(c) with c the number of clusters
	 * @return the clusterID of nearest cluster center for a specific point
	 */
	def centerPredict[ID, O, Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz]](cz: Cz[ID, O, V]): ClusterID = centerPredict(cz.v)

}
/**
 *
 */
trait CenterModelLocal[V <: GVector[V], D <: Distance[V]] extends CenterModel[V, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	def centerPredict[GS[X] <: GenSeq[X]](data: GS[V]): GS[(ClusterID, V)] = data.map( v => (centerPredict(v), v) ).asInstanceOf[GS[(ClusterID, V)]]
}
/**
 *
 */
trait CenterModelSimpleVLocal[T, V <: Seq[T], SV <: GSimpleVector[T, V, SV], D <: GSimpleVectorDistance[T, V, SV]] extends CenterModelSimpleV[T, V, SV, D] {
	/**0
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	def centerPredict[GS[X] <: GenSeq[X]](data: GS[V])(implicit d1: DummyImplicit, d2 : DummyImplicit): GS[(ClusterID, V)] = data.map( v => (centerPredict(v), v) ).asInstanceOf[GS[(ClusterID, V)]]
}
/**
 *
 */
trait CenterModelMixtLocal[Vb <: Seq[Int], Vs <: Seq[Double], D <: MixtDistance[Vb, Vs]] extends CenterModelMixt[Vb, Vs, D] {
	/**0
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	def centerPredict[GS[X] <: GenSeq[X]](data: GS[(Vb, Vs)])(implicit d1: DummyImplicit, d2 : DummyImplicit): GS[(ClusterID, (Vb, Vs))] = data.map( v => (centerPredict(v), v) ).asInstanceOf[GS[(ClusterID, (Vb, Vs))]]
}
/**
 *
 */
trait CenterModelLocalReal[V <: Seq[Double], D <: ContinuousDistance[V]] extends CenterModelSimpleVLocal[Double, V, ScalarVector[V], D]
/**
 *
 */
trait CenterModelLocalBinary[V <: Seq[Int], D <: BinaryDistance[V]] extends CenterModelSimpleVLocal[Int, V, BinaryVector[V], D]
/**
 *
 */
trait CenterModelLocalCz[V <: GVector[V], D <: Distance[V]] extends CenterModelLocal[V, D] with CenterModelCz[V, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	def centerPredict[ID, O, Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, V]])(implicit d: DummyImplicit): GS[Cz[ID, O, V]] = {
		data.map( cz => cz.addClusterIDs(centerPredict(cz)) ).asInstanceOf[GS[Cz[ID, O, V]]]
	}
}