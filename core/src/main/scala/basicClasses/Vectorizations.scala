package org.clustering4ever.vectorizations
/**
 * @author Beck Gaël
 */
import org.clustering4ever.vectors.GVector
import org.clustering4ever.types.VectorizationIDTypes._
/**
 *
 */
sealed trait EmployedVectorization extends Serializable {
	val vectorizationID: Int
	val vectorNature: Int
}
/**
 *
 */
case class DefaultWorkingVector(val vectorizationID: VectorizationID = 0, val vectorNature: VectorizationNature = 0) extends EmployedVectorization
/**
 *
 */
case class IthVectorization[O, V <: GVector[V]](val vectorizationID: VectorizationID, val vectorization: O => V, val vectorNature: VectorizationNature = 0) extends EmployedVectorization
