package org.clustering4ever.clustering.keeper
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.mutable
import shapeless.{HList, HNil, HMap}
import org.clustering4ever.vectors.GVector
import org.clustering4ever.vectorizations.{VectorizationAncestor, Vectorization, VectorizationGenLocal}
import org.clustering4ever.shapeless.VectorizationMapping
import org.clustering4ever.types.VectorizationIDTypes._
/**
 *
 */
case class VectorizationsKeeper(var vectorizations: HMap[VectorizationMapping] = HMap.empty[VectorizationMapping]) extends Serializable {
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
	def addVectorization[Vecto <: VectorizationAncestor[Vecto]](vecto: Vecto): Unit = {
		implicit val mapping = VectorizationMapping[VectorizationID, Vecto]
		vectorizations = vectorizations + ((vecto.vectorizationID, vecto))
	}
	/**
	 *
	 */
	def getVectorization[Vecto <: VectorizationAncestor[Vecto]](vecto: Vecto): Option[Vecto] = vectorizations.get(vecto.vectorizationID)(vecto.vectoMapping)

}
/**
 *
 */
case class SameNatureVectorizationsKeeper[O, V <: GVector[V], Vecto <: Vectorization[O, V, Vecto]](val vectorizations: mutable.ArrayBuffer[Vecto])