package org.clustering4ever.roottraits

/**
 * @author Beck Gaël
 */

import scala.language.higherKinds
/**
 *
 */
trait GeneralPurposable[
	O,
	V <: GVector[V],
	Self[B, C <: GVector[C]] <: GeneralPurposable[B, C, Self]
] extends Clusterizable[O, V, Self] with Supervizable[O, V, Self] {
	this: Self[O, V] =>
}