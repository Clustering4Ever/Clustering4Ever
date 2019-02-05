package org.clustering4ever.scala.wtf
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import org.clustering4ever.vectors.GVector
import org.clustering4ever.supervizables.Supervizable
import org.clustering4ever.clusterizables.Clusterizable
/**
 *
 */
trait GeneralPurposable[
	O,
	V <: GVector[V],
	Self[B, C <: GVector[C]] <: GeneralPurposable[B, C, Self]
] extends Clusterizable[O, V, Self] with Supervizable[O, V, Self] {
	/**
	 *
	 */
	this: Self[O, V] =>
}