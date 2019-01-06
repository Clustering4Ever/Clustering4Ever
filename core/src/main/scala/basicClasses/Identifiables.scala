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
 *
 */
trait Identified[ID] extends Serializable {
	/**
	 *
	 */
	val id: ID
	/**
	 *
	 */
	final override def hashCode(): Int = id.hashCode
}
/**
 *
 */
trait RawObject[O] extends Serializable {
	/**
	 *
	 */
	val o: O
}
/**
 *
 */
trait IdentifiedRawObject[ID, O] extends Identified[ID] with RawObject[O]
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
	 *
	 */
	val v: V
}
/**
 *
 */
trait IdentifiedGVector[ID, O, V <: GVector[V]] extends IdentifiedVector[ID, O, V] {
	/**
	 * Second hashCode just in case if IDs are more than 2^32
	 */
	final val hashCode2: Int = v.hashCode
}
/**
 *
 */
trait IdentifiedVectorized[ID, O, V <: GVector[V]] extends IdentifiedGVector[ID, O, V] {
	/**
	 *
	 */
	val vectorized: HMap[VMapping]
}
/**
 *
 */
case class EasyIdentifiedVector[ID, O, V <: GVector[V]](
	val id: ID,
	val o: Vectorizable[O],
	val v: V,
	val vectorized: HMap[VMapping] = HMap.empty[VMapping]
) extends IdentifiedVectorized[ID, O, V]