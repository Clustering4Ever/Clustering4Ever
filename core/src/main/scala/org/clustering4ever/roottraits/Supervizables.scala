package org.clustering4ever.roottraits

/**
 * @author Beck Gaël
 */
import org.clustering4ever.roottraits.VectorizationIDTypes._
import shapeless.HMap

import scala.collection.mutable
import scala.language.higherKinds
/**
 *
 */
trait Supervizable[O, V <: GVector[V], Self[B, C <: GVector[C]] <: Supervizable[B, C, Self]] extends Preprocessable[O, V, Self] {
	this: Self[O, V] =>
	/**
	 *
	 */
	val label: Int
	/**
	 *
	 */
	val bucketizedFeatures: mutable.ArrayBuffer[V]
	/**
	 *
	 */
	def obtainOneBucket(i: Int): Self[O, V]
	/**
	 * Transform from DFCL to HDFCL by applying bucketing of bucketsOfFeats argument 
	 */
	def definedBucketizedFeatures(bucketsOfFeats: Seq[Seq[Int]]): Self[O, V]

}
/**
 *
 */
object EasySupervizable {
	/**
	 * Simplest way to generate an EasySupervizable
	 */
	final def apply[V <: GVector[V]](id: Long, v: V, label: Int): EasySupervizable[None.type, V] = EasySupervizable(id, Vectorizable(None), label, v, mutable.ArrayBuffer.empty[V], HMap.empty[VMapping])
	/**
	 * Generate a proper EasySupervizable
	 */
	final def apply[O, V <: GVector[V]](id: Long, o: O, v: V, label: Int): EasySupervizable[O, V] = EasySupervizable(id, Vectorizable(o), label, v, mutable.ArrayBuffer.empty[V], HMap.empty[VMapping])

}
/**
 *
 */
@SerialVersionUID(1L)
final case class EasySupervizable[O, V <: GVector[V]](
	final val id: Long,
	final val o: Vectorizable[O],
	final val label: Int,
	final val v: V,
	final val bucketizedFeatures: mutable.ArrayBuffer[V] = mutable.ArrayBuffer.empty[V],
	final val vectorized: HMap[VMapping] = HMap.empty[VMapping]
) extends Supervizable[O, V, EasySupervizable] {
	/**
	 *
	 */
	final override def canEqual(a: Any): Boolean = a.isInstanceOf[EasySupervizable[O, V]]
	/**
	 *
	 */
	final override def equals(that: Any): Boolean = {
		that match {
		  case that: EasySupervizable[O, V] => that.canEqual(this) && that.hashCode == this.hashCode // && that.hashCode2 == this.hashCode2
		  case _ => false
		}
	}

	final def obtainOneBucket(i: Int): EasySupervizable[O, V] = {
		EasySupervizable(id, o, label, bucketizedFeatures(i), bucketizedFeatures)
	}

	final def definedBucketizedFeatures(bucketsOfFeats: Seq[Seq[Int]]): EasySupervizable[O, V] = {
		bucketizedFeatures.clear
		bucketsOfFeats.foreach( oneBucket => bucketizedFeatures += v.pickFeatures(oneBucket:_*) )
		this
	}

	final def addVectorization[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto[A, B]]](vectorization: Vecto[O, GV]): EasySupervizable[O, V] = {
		this.copy(vectorized = vectorized.+(((vectorization.vectorizationID, o.toVector(vectorization.vectorizationFct.get))))(VMapping[VectorizationID, GV]))
	}

	final def addAlternativeVector[GV <: GVector[GV]](vectorizationID: VectorizationID, newAltVector: GV): EasySupervizable[O, V] = {
		this.copy(vectorized = vectorized.+(((vectorizationID, newAltVector)))(VMapping[VectorizationID, GV]))
	}

	final def switchForExistingVectorization[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto[A, B]]](vectorization: Vecto[O, GV]): EasySupervizable[O, GV] = {
		EasySupervizable(id, o, label, vectorized.get(vectorization.vectorizationID)(vectorization.vMapping).get, mutable.ArrayBuffer.empty[GV], vectorized)
	}

	final def switchForExistingVectorization[GV <: GVector[GV]](vectorizationID: VectorizationID, vMapping: VMapping[VectorizationID, GV]): EasySupervizable[O, GV] = {
		EasySupervizable(id, o, label, vectorized.get(vectorizationID)(vMapping).get, mutable.ArrayBuffer.empty[GV], vectorized)
	}

	final def updateVectorization[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto[A, B]]](vectorization: Vecto[O, GV]): EasySupervizable[O, GV] = {
		EasySupervizable(id, o, label, o.toVector(vectorization.vectorizationFct.get), mutable.ArrayBuffer.empty[GV], vectorized)
	}

	final def updateVectorizationOfSameNature[Vecto <: Vectorization[O, V, Vecto]](vectorization: Vecto): EasySupervizable[O, V] = {
		this.copy(v = o.toVector(vectorization.vectorizationFct.get))
	}

	final def updateWorkingVector[GV <: GVector[GV]](newWorkingVector: GV): EasySupervizable[O, GV] = {
		EasySupervizable(id, o, label, newWorkingVector, mutable.ArrayBuffer.empty[GV], vectorized)
	}


}