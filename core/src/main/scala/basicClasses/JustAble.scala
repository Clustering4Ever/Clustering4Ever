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
	ID,
	O,
	V <: GVector[V],
	Self[A, B, C <: GVector[C]] <: GeneralPurposable[A, B, C, Self]
] extends Clusterizable[ID, O, V, Self] with Supervizable[ID, O, V, Self]