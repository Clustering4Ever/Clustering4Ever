package org.clustering4ever.util

/**
 * @author Beck Gaël
 */
import org.clustering4ever.roottraits._

import scala.collection.GenSeq
import scala.language.{higherKinds, implicitConversions}
/**
 *
 */
object ArrayAndSeqTowardGVectorImplicit {
	/**
	 *
	 */
	implicit def realArrayToScalarVector(v: Array[Double]): ScalarVector = ScalarVector(v) 
	/**
	 *
	 */
	implicit def binaryArrayToBinaryVector(v: Array[Int]): BinaryVector = BinaryVector(v) 
	/**
	 *
	 */
	implicit def mixtArrayToMixedVector(v: (Array[Int], Array[Double])): MixedVector = MixedVector(v._1, v._2)
}
/**
 *
 */
object ScalaCollectionImplicits {
	/**
	 *
	 */
	implicit def scalarDataWithIDToClusterizable[GS[Y] <: GenSeq[Y]](genSeq: GS[(Array[Double], Long)]): GS[EasyClusterizable[ScalarVector, ScalarVector]] = {
		genSeq.map{ case (vector, id) => EasyClusterizable(id, ScalarVector(vector)) }.asInstanceOf[GS[EasyClusterizable[ScalarVector, ScalarVector]]]
	}
	/**
	 *
	 */
	implicit def binaryDataWithIDToClusterizable[GS[Y] <: GenSeq[Y]](genSeq: GS[(Array[Int], Long)]): GS[EasyClusterizable[BinaryVector, BinaryVector]] = {
		genSeq.map{ case (vector, id) => EasyClusterizable(id, BinaryVector(vector)) }.asInstanceOf[GS[EasyClusterizable[BinaryVector, BinaryVector]]]
	}
	/**
	 *
	 */
	implicit def mixtDataWithIDToClusterizable[GS[X] <: GenSeq[X]](genSeq: GS[((Array[Int], Array[Double]), Long)]): GS[EasyClusterizable[MixedVector, MixedVector]] = {
		genSeq.map{ case ((binary, scalar), id) => EasyClusterizable(id, MixedVector(binary, scalar)) }.asInstanceOf[GS[EasyClusterizable[MixedVector, MixedVector]]]
	}
	/**
	 *
	 */
	implicit def scalarToClusterizable[GS[Y] <: GenSeq[Y]](genSeq: GS[Array[Double]]): GS[EasyClusterizable[ScalarVector, ScalarVector]] = genSeq.zipWithIndex.map{ case (v, id) => (v, id.toLong) }.asInstanceOf[GS[(Array[Double], Long)]]
	/**
	 *
	 */
	implicit def binaryToClusterizable[GS[Y] <: GenSeq[Y]](genSeq: GS[Array[Int]]): GS[EasyClusterizable[BinaryVector, BinaryVector]] = genSeq.zipWithIndex.map{ case (v, id) => (v, id.toLong) }.asInstanceOf[GS[(Array[Int], Long)]]
	/**
	 *
	 */
	implicit def mixtToClusterizable[GS[X] <: GenSeq[X]](genSeq: GS[(Array[Int], Array[Double])]): GS[EasyClusterizable[MixedVector, MixedVector]] = genSeq.zipWithIndex.map{ case (v, id) => (v, id.toLong) }.asInstanceOf[GS[((Array[Int], Array[Double]), Long)]]
	/**
	 *
	 */
	implicit def rawDataToSupervizable[T, GS[Y] <: GenSeq[Y]](gs: GS[(Array[T], Int)]): GS[EasySupervizable[SupervizedVector[T], SupervizedVector[T]]] = {
		gs.zipWithIndex.map{ case ((v, l), id) => EasySupervizable(id.toLong, SupervizedVector(v),  l) }.asInstanceOf[GS[EasySupervizable[SupervizedVector[T], SupervizedVector[T]]]]
	}
}