package org.clustering4ever.identifiables
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import shapeless.HMap
import org.clustering4ever.shapeless.VMapping
import org.clustering4ever.vectorizables.Vectorizable
import org.clustering4ever.vectors.GVector
/**
 * HashCode of IdentifiedRawObject descendant is fixed to ID hashcode
 */
trait IdentifiedRawObject[ID, O] {
	/**
	 * The ID of the container class
	 */
	val id: ID
	/**
	 * The raw object of the container class
	 */
	val o: O
	/**
	 * HashCode of the container is defined by hashcode of its id field
	 */
	final override def hashCode(): Int = id.hashCode
}
/**
 *
 */
case class EasyIdentifiedRawObject[ID, O](val id: ID, val o: O) extends IdentifiedRawObject[ID, O]
/**
 * Identified Vectorizable Object
 */
trait IdentifiedVectorizableObject[ID, O] extends IdentifiedRawObject[ID, Vectorizable[O]]
/**
 *
 */
trait IdentifiedVector[ID, O, V] extends IdentifiedRawObject[ID, Vectorizable[O]] {
	/**
	 * The working vector of the container
	 */
	val v: V
	/**
	 * Second hashCode just in case if IDs are more than 2^32
	 */
	final val hashCode2: Int = v.hashCode
}
/**
 * Restrict V to be a GVector descendant
 */
trait IdentifiedGVector[ID, O, V <: GVector[V]] extends IdentifiedVector[ID, O, V]
/**
 *
 */
trait IdentifiedWithVectorizations[ID, O, V <: GVector[V]] extends IdentifiedGVector[ID, O, V] {
	/**
	 * HMap containing various vectorization of the raw object as descendant of GVector
	 */
	val vectorized: HMap[VMapping]
}
/**
 *
 */
case class EasyIdentifiedVector[ID, O, V <: GVector[V]](
	val id: ID,
	val v: V,
	val o: Vectorizable[O],
	val vectorized: HMap[VMapping] = HMap.empty[VMapping]
) extends IdentifiedWithVectorizations[ID, O, V]