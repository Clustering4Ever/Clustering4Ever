package org.clustering4ever.supervizables
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.mutable
import org.clustering4ever.scala.vectorizables.Vectorizable
import org.clustering4ever.preprocessables.Preprocessable
import org.clustering4ever.shapeless.VMapping
import shapeless.HMap
import org.clustering4ever.scala.vectors.GVector
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
	def obtainOneBucket(i: Int): EasySupervizable[ID, O, V] = {
		new EasySupervizable(id, o, label, bucketizedFeatures(i), bucketizedFeatures)
	}
	/**
	 * Transform from DFCL to HDFCL by applying bucketing of bucketsOfFeats argument 
	 */
	def definedBucketizedFeatures(bucketsOfFeats: Seq[Seq[Int]]): EasySupervizable[ID, O, V] = {

		bucketizedFeatures.clear

		bucketsOfFeats.foreach( oneBucket => bucketizedFeatures += v.pickFeatures(oneBucket:_*) )

		this
	}
	/**
	 *
	 */
	def addVectorized[GV <: GVector[GV]](vectorizationID: Int, towardNewVector: O => GV)(implicit vMapping: VMapping[Int, GV] = new VMapping[Int, GV]): EasySupervizable[ID, O, V] = {
		this.copy(vectorized = vectorized + ((vectorizationID, o.toVector(towardNewVector))))
	}
	/**
	 *
	 */
	def addAltVector[GV <: GVector[GV]](vectorizationID: Int, newAltVector: GV)(implicit vMapping: VMapping[Int, GV] = new VMapping[Int, GV]): EasySupervizable[ID, O, V] = {
		this.copy(vectorized = vectorized + ((vectorizationID, newAltVector)))
	}
	/**
	 *
	 */
	def updtV[GV <: GVector[GV]](vectorizationID: Int)(implicit vMapping: VMapping[Int, GV] = new VMapping[Int, GV]): EasySupervizable[ID, O, GV] = {
		new EasySupervizable(id, o, label, vectorized.get(vectorizationID).get.asInstanceOf[GV], mutable.ArrayBuffer.empty[GV], vectorized)
	}
}