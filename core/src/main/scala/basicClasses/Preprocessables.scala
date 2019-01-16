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
	def addVectorization[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto]](vectorization: Vecto[O, GV]): Self[ID, O, V]
	/**
	 *
	 */
	def addAlternativeVector[GV <: GVector[GV]](vectorizationID: Int, newAlternativeVector: GV): Self[ID, O, V]
	/**
	 *
	 */
	def switchForExistingVector[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto]](vectorization: Vecto[O, GV]): Self[ID, O, GV]
	/**
	 *
	 */
	def updateVectorization[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto]](vectorization: Vecto[O, GV]): Self[ID, O, GV]
}