package org.clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.collection.{GenSeq, mutable, immutable}
import org.clustering4ever.clusterizables.EasyClusterizable
import org.clustering4ever.supervizables.EasySupervizable
import org.clustering4ever.vectors.SupervizedVector
import org.clustering4ever.vectors.{SupervizedVector, BinaryVector, ScalarVector, MixtVector}
/**
 *
 */
object ArrayAndSeqTowardGVectorImplicit {
	/**
	 *
	 */
	implicit def realSeqToScalarVector[V <: Seq[Double]](v: V): ScalarVector[V] = ScalarVector(v) 
	/**
	 *
	 */
	implicit def realArrayToScalarVector(v: Array[Double]): ScalarVector[mutable.ArrayBuffer[Double]] = ScalarVector(mutable.ArrayBuffer(v:_*)) 
	/**
	 *
	 */
	implicit def binarySeqToScalarVector[V <: Seq[Int]](v: V): BinaryVector[V] = BinaryVector(v) 
	/**
	 *
	 */
	implicit def binaryArrayToScalarVector(v: Array[Int]): BinaryVector[mutable.ArrayBuffer[Int]] = BinaryVector(mutable.ArrayBuffer(v:_*)) 
	/**
	 *
	 */
	implicit def mixtSeqToScalarVector[Vb <: Seq[Int], Vs <: Seq[Double]](v: (Vb, Vs)): MixtVector[Vb, Vs] = MixtVector(v._1, v._2) 
	/**
	 *
	 */
	implicit def mixtArrayToScalarVector(v: (Array[Int], Array[Double])): MixtVector[mutable.ArrayBuffer[Int], mutable.ArrayBuffer[Double]] = MixtVector(mutable.ArrayBuffer(v._1:_*), mutable.ArrayBuffer(v._2:_*))
}
/**
 *
 */
object ScalaCollectionImplicits {
	/**
	 *
	 */
	implicit def scalarDataWithIDToClusterizable[ID, V <: Seq[Double], GS[Y] <: GenSeq[Y]](genSeq: GS[(V, ID)]): GS[EasyClusterizable[ID, ScalarVector[V], ScalarVector[V]]] = {
		genSeq.map{ case (vector, id) => EasyClusterizable(id, ScalarVector(vector)) }.asInstanceOf[GS[EasyClusterizable[ID, ScalarVector[V], ScalarVector[V]]]]
	}
	/**
	 *
	 */
	implicit def binaryDataWithIDToClusterizable[ID, V <: Seq[Int], GS[Y] <: GenSeq[Y]](genSeq: GS[(V, ID)]): GS[EasyClusterizable[ID, BinaryVector[V], BinaryVector[V]]] = {
		genSeq.map{ case (vector, id) => EasyClusterizable(id, BinaryVector(vector)) }.asInstanceOf[GS[EasyClusterizable[ID, BinaryVector[V], BinaryVector[V]]]]
	}
	/**
	 *
	 */
	implicit def mixtDataWithIDToClusterizable[ID, Vb <: Seq[Int], Vs <: Seq[Double], GS[X] <: GenSeq[X]](genSeq: GS[((Vb, Vs), ID)]): GS[EasyClusterizable[ID, MixtVector[Vb, Vs], MixtVector[Vb, Vs]]] = {
		genSeq.map{ case ((binary, scalar), id) => EasyClusterizable(id, MixtVector(binary, scalar)) }.asInstanceOf[GS[EasyClusterizable[ID, MixtVector[Vb, Vs], MixtVector[Vb, Vs]]]]
	}
	/**
	 *
	 */
	implicit def scalarToClusterizable[V <: Seq[Double], GS[Y] <: GenSeq[Y]](genSeq: GS[V]): GS[EasyClusterizable[Int, ScalarVector[V], ScalarVector[V]]] = genSeq.zipWithIndex.asInstanceOf[GS[(V, Int)]]
	/**
	 *
	 */
	implicit def binaryToClusterizable[V <: Seq[Int], GS[Y] <: GenSeq[Y]](genSeq: GS[V]): GS[EasyClusterizable[Int, BinaryVector[V], BinaryVector[V]]] = genSeq.zipWithIndex.asInstanceOf[GS[(V, Int)]]
	/**
	 *
	 */
	implicit def mixtToClusterizable[Vb <: Seq[Int], Vs <: Seq[Double], GS[X] <: GenSeq[X]](genSeq: GS[(Vb, Vs)]): GS[EasyClusterizable[Int, MixtVector[Vb, Vs], MixtVector[Vb, Vs]]] = genSeq.zipWithIndex.asInstanceOf[GS[((Vb, Vs), Int)]]
	/**
	 *
	 */
	implicit def rawDataToSupervizable[T, S[X] <: Seq[X], GS[Y] <: GenSeq[Y]](gs: GS[(S[T], Int)]): GS[EasySupervizable[Int, SupervizedVector[T, S], SupervizedVector[T, S]]] = {
		gs.zipWithIndex.map{ case ((v, l), id) => EasySupervizable(id, SupervizedVector(v),  l) }.asInstanceOf[GS[EasySupervizable[Int, SupervizedVector[T, S], SupervizedVector[T, S]]]]
	}
}