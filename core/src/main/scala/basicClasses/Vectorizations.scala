package org.clustering4ever.vectorizations
/**
 * @author Beck Gaël
 */
import org.clustering4ever.vectors.GVector
import org.clustering4ever.types.VectorizationIDTypes._
/**
 *
 */
sealed trait VectorizationNature extends Serializable
/**
 *
 */
object Scalar extends VectorizationNature
/**
 *
 */
object Binary extends VectorizationNature
/**
 *
 */
object Mixt extends VectorizationNature
/**
 *
 */
object Other extends VectorizationNature
/**
 *
 */
object Default extends VectorizationNature
/**
 *
 */
sealed trait EmployedVectorization extends Serializable {
	val vectorizationID: VectorizationID
	val vectorizationNature: VectorizationNature
}
/**
 *
 */
case class IthVectorization[O, V <: GVector[V]](val vectorizationID: VectorizationID, val vectorization: Option[O => V], val vectorizationNature: VectorizationNature) extends EmployedVectorization