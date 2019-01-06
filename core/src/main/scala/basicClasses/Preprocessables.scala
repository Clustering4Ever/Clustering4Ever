package org.clustering4ever.preprocessables
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.mutable
import shapeless.HMap
import org.clustering4ever.identifiables.IdentifiedVectorized
import org.clustering4ever.shapeless.VMapping
import org.clustering4ever.vectors.GVector
import org.clustering4ever.supervizables.Supervizable
/**
 * Common trait for object than can be preprocessed to obtain new vectorization
 */
trait Preprocessable[ID, O, V <: GVector[V], Self[A, B, C <: GVector[C]] <: Preprocessable[A, B, C, Self]] extends IdentifiedVectorized[ID, O, V] {
	/**
	 *
	 */
	def addVectorized[GV <: GVector[GV]](vectorizationID: Int, towardNewVector: O => GV)(implicit vMapping: VMapping[Int, GV] = new VMapping[Int, GV]): Self[ID, O, V]
	/**
	 *
	 */
	def addAltVector[GV <: GVector[GV]](vectorizationID: Int, newAltVector: GV)(implicit vMapping: VMapping[Int, GV] = new VMapping[Int, GV]): Self[ID, O, V]
	/**
	 *
	 */
	def updtV[GV <: GVector[GV]](vectorizationID: Int)(implicit vMapping: VMapping[Int, GV] = new VMapping[Int, GV]): Self[ID, O, GV]
}