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
final case class VectorizationsKeeper(var vectorizations: HMap[VectorizationMapping] = HMap.empty[VectorizationMapping]) extends Serializable {
    /**
	 *
	 */
	// def getVectorization[O, V <: GVector[V], Vecto[A, B <: GVector[B]] <: VectorizationGenLocal[A, B, Vecto[A, B]]](vectorization: Vecto[O, V]): Option[Vecto[O, V]] = {
	// 	vectorizations.get(vectorization.vectorizationID)(vectorization.vectoMapping)
	// }
    /**
	 *
	 */
	// def updateVectorizations[O, V <: GVector[V], Vecto[A, B <: GVector[B]] <: VectorizationGenLocal[A, B, Vecto[A, B]]](vectorization: Vecto[O, V]): VectorizationsKeeper = {
	// 	val updatedVectorizations = vectorizations.+((vectorization.vectorizationID, vectorization))(vectorization.vectoMapping)
	// 	this.copy(vectorizations = updatedVectorizations)
	// }
	/**
	 *
	 */
	final def addVectorization[Vecto <: VectorizationAncestor[Vecto]](vecto: Vecto): Unit = {
		implicit val mapping = VectorizationMapping[VectorizationID, Vecto]
		vectorizations = vectorizations + ((vecto.vectorizationID, vecto))
	}
	/**
	 *
	 */
	final def getVectorization[Vecto <: VectorizationAncestor[Vecto]](vecto: Vecto): Option[Vecto] = vectorizations.get(vecto.vectorizationID)(vecto.vectoMapping)

}
/**
 *
 */
final case class SameNatureVectorizationsKeeper[O, V <: GVector[V], Vecto <: Vectorization[O, V, Vecto]](val vectorizations: mutable.ArrayBuffer[Vecto])