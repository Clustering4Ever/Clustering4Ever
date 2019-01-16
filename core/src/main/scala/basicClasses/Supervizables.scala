package org.clustering4ever.supervizables
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.mutable
import org.clustering4ever.vectorizables.Vectorizable
import org.clustering4ever.preprocessing.Preprocessable
import org.clustering4ever.shapeless.VMapping
import shapeless.HMap
import org.clustering4ever.vectors.GVector
import org.clustering4ever.vectorizables.NotVectorizable
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.vectorizations.Vectorization
/**
 *
 */
trait Supervizable[ID, O, V <: GVector[V], Self[A, B, C <: GVector[C]] <: Supervizable[A, B, C, Self]] extends Preprocessable[ID, O, V, Self] {
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
	def obtainOneBucket(i: Int): Self[ID, O, V]
	/**
	 * Transform from DFCL to HDFCL by applying bucketing of bucketsOfFeats argument 
	 */
	def definedBucketizedFeatures(bucketsOfFeats: Seq[Seq[Int]]): Self[ID, O, V]

}
/**
 *
 */
object EasySupervizable {
	/**
	 * Simplest way to generate an EasySupervizable
	 */
	def apply[ID, V <: GVector[V]](id: ID, v: V, label: Int): EasySupervizable[ID, V, V] = new EasySupervizable(id, Vectorizable(v), label, v, mutable.ArrayBuffer.empty[V], HMap[VMapping](0 -> v)(VMapping[Int, V]))
	/**
	 * Generate a proper EasySupervizable
	 */
	def apply[ID, O, V <: GVector[V]](id: ID, o: O, v: V, label: Int) = new EasySupervizable(id, Vectorizable(o), label, v, mutable.ArrayBuffer.empty[V], HMap[VMapping](0 -> v)(VMapping[Int, V]))

}
/**
 *
 */
case class EasySupervizable[ID, O, V <: GVector[V]](
	val id: ID,
	val o: Vectorizable[O],
	val label: Int,
	val v: V,
	val bucketizedFeatures: mutable.ArrayBuffer[V] = mutable.ArrayBuffer.empty[V],
	val vectorized: HMap[VMapping] = HMap.empty[VMapping]
) extends Supervizable[ID, O, V, EasySupervizable] {
	/**
	 *
	 */
	final override def canEqual(a: Any): Boolean = a.isInstanceOf[EasySupervizable[ID, O, V]]
	/**
	 *
	 */
	final override def equals(that: Any): Boolean = {
		that match {
		  case that: EasySupervizable[ID, O, V] => that.canEqual(this) && that.hashCode == this.hashCode && that.hashCode2 == this.hashCode2
		  case _ => false
		}
	}
	/**
	 *
	 */
	final def obtainOneBucket(i: Int): EasySupervizable[ID, O, V] = {
		EasySupervizable(id, o, label, bucketizedFeatures(i), bucketizedFeatures)
	}
	/**
	 * Transform from DFCL to HDFCL by applying bucketing of bucketsOfFeats argument 
	 */
	final def definedBucketizedFeatures(bucketsOfFeats: Seq[Seq[Int]]): EasySupervizable[ID, O, V] = {
		bucketizedFeatures.clear
		bucketsOfFeats.foreach( oneBucket => bucketizedFeatures += v.pickFeatures(oneBucket:_*) )
		this
	}
	/**
	 *
	 */
	final def addVectorization[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto]](vectorization: Vecto[O, GV]): EasySupervizable[ID, O, V] = {
		this.copy(vectorized = vectorized.+(((vectorization.vectorizationID, o.toVector(vectorization.vectorizationFct.get))))(VMapping[VectorizationID, GV]))
	}
	/**
	 *
	 */
	final def addAlternativeVector[GV <: GVector[GV]](vectorizationID: VectorizationID, newAltVector: GV): EasySupervizable[ID, O, V] = {
		this.copy(vectorized = vectorized.+(((vectorizationID, newAltVector)))(VMapping[VectorizationID, GV]))
	}
	/**
	 *
	 */
	final def switchForExistingVector[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto]](vectorization: Vecto[O, GV]): EasySupervizable[ID, O, GV] = {
		EasySupervizable(id, o, label, vectorized.get(vectorization.vectorizationID)(vectorization.vMapping).get.asInstanceOf[GV], mutable.ArrayBuffer.empty[GV], vectorized)
	}

	/**
	 *
	 */
	final def updateVectorization[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto]](vectorization: Vecto[O, GV]): EasySupervizable[ID, O, GV] = {
		EasySupervizable(id, o, label, o.toVector(vectorization.vectorizationFct.get), mutable.ArrayBuffer.empty[GV], vectorized)
	}
}