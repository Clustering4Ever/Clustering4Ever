package org.clustering4ever.vectorizations
/**
 * @author Beck Gaël
 */
import org.clustering4ever.vectors.GVector
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
class DefaultWorkingVector(val vectorizationID: Int = 0, val vectorNature: Int = 0) extends EmployedVectorization
/**
 *
 */
class IthVectorization[O, V <: GVector[V]](val vectorizationID: Int, val vectorization: O => V, val vectorNature: Int = 0) extends EmployedVectorization
