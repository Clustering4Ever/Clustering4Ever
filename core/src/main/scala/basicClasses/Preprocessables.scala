package org.clustering4ever.preprocessing
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.mutable
import shapeless.HMap
import org.clustering4ever.identifiables.IdentifiedWithVectorizations
import org.clustering4ever.shapeless.VMapping
import org.clustering4ever.vectors.GVector
import org.clustering4ever.supervizables.Supervizable
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.vectorizations.Vectorization
/**
 * Common trait for object than can be preprocessed to obtain new vectorization
 */
trait Preprocessable[ID, O, V <: GVector[V], Self[A, B, C <: GVector[C]] <: Preprocessable[A, B, C, Self]] extends IdentifiedWithVectorizations[ID, O, V] {
	/**
	 *
	 */
	this: Self[ID, O, V] =>
	/**
	 * Apply the given vectorization on raw object and add the obtain vector in vectorized field
	 */
	def addVectorization[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto[A, B]]](vectorization: Vecto[O, GV]): Self[ID, O, V]
	/**
	 * Add directly a vector in vectorized field without passing through a vectorization
	 */
	def addAlternativeVector[GV <: GVector[GV]](vectorizationID: Int, newAlternativeVector: GV): Self[ID, O, V]
	/**
	 * Look for an existing vector in vectorized field and put it as working vector
	 */
	def switchForExistingVector[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto[A, B]]](vectorization: Vecto[O, GV]): Self[ID, O, GV]
	/**
	 * Update working vector by applying given vectorization.
	 * This method doesn't save previous working vector and given vectorization in vectorized field
	 */
	def updateVectorization[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto[A, B]]](vectorization: Vecto[O, GV]): Self[ID, O, GV]

}