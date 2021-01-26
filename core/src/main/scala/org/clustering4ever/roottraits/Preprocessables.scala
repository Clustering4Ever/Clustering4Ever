package org.clustering4ever.roottraits

/**
 * @author Beck Gaël
 */
import org.clustering4ever.roottraits.VectorizationIDTypes._

import scala.language.higherKinds
/**
 * Common trait for object than can be preprocessed to obtain new vectorization
 * @tparam ID the ID corresponding to this clusterizable, it is recommend to put a type with an easy Ordering associate to it, ie Int, Long, Double
 * @tparam O the raw object from which vectorization are made
 * @tparam V the nature of the working vector on which algorithm are run
 * @tparam Self the concrete implementation of Preprocessable
 */
trait Preprocessable[O, V <: GVector[V], Self[B, C <: GVector[C]] <: Preprocessable[B, C, Self]] extends IdentifiedWithVectorizations[O, V] {
	/**
	 *
	 */
	this: Self[O, V] =>
	/**
	 * Apply the given vectorization on raw object and add the obtain vector in vectorized field
	 * @tparam GV the type of vector resulting from this vectorization
	 * @tparam Vecto the type of given vectorization
	 * @param : vectorization the employed vectorization
	 */
	def addVectorization[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto[A, B]]](vectorization: Vecto[O, GV]): Self[O, V]
	/**
	 * Add directly a vector in vectorized field without passing through a vectorization
	 * @tparam GV the type of the added alternative vector
	 */
	def addAlternativeVector[GV <: GVector[GV]](vectorizationID: Int, newAlternativeVector: GV): Self[O, V]
	/**
	 * Look for an existing vector in vectorized field and put it as working vector
	 * @tparam GV the type of the updated working vector
	 * @tparam Vecto the type of given vectorization
	 * @param : vectorization the employed vectorization
	 */
	def switchForExistingVectorization[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto[A, B]]](vectorization: Vecto[O, GV]): Self[O, GV]
	/**
	 * Look for an existing vector in vectorized field and put it as working vector
	 * @tparam GV the type of the updated working vector
	 * @tparam Vecto the type of given vectorization
	 * @param : vectorization the employed vectorization
	 */
	def switchForExistingVectorization[GV <: GVector[GV]](vectorizationID: VectorizationID, mapping: VMapping[VectorizationID, GV]): Self[O, GV]
	/**
	 * Update working vector by applying given vectorization.
	 * This method doesn't save previous working vector and given vectorization in vectorized field
	 * @tparam GV the type of the updated working vector
	 * @tparam Vecto the type of given vectorization
	 * @param : vectorization the employed vectorization
	 */
	def updateVectorization[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto[A, B]]](vectorization: Vecto[O, GV]): Self[O, GV]
	/**
	 * Update working vector by applying given vectorization of same nature that current vectorization.
	 * @tparam Vecto the type of given vectorization
	 */
	def updateVectorizationOfSameNature[Vecto <: Vectorization[O, V, Vecto]](vectorization: Vecto): Self[O, V]
	/**
	 *
	 */
	def updateWorkingVector[GV <: GVector[GV]](newWorkingVector: GV): Self[O, GV]

}