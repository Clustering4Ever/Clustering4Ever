package org.clustering4ever.math.distances.binary
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import org.clustering4ever.math.distances.{Distance, BinaryDistance, RawBinaryDistance}
import scala.collection.mutable
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.vectors.{BinaryVector, GVector}
import org.clustering4ever.types.MetricIDType._
/**
 *
 */
trait HammingMeta extends Serializable {

	protected def hamming[V <: Seq[Int]](dot1: V, dot2: V): Double = {

		@annotation.tailrec
		def go(d: Double, i: Int): Double = {
			if( i < dot1.size ) {
			  go(d + (dot1(i) ^ dot2(i)), i + 1)
			}
			else d
		}
		go(0D, 0)

	}
}
/**
 *
 */
class RawHamming[V <: Seq[Int]](final val id: MetricID = 4) extends HammingMeta with RawBinaryDistance[V] {
	/**
	  * The Hamming distance with or without squareRoot
	  * @return The Hamming distance between dot1 and dot2
	  */
	def d(dot1: V, dot2: V): Double = hamming(dot1, dot2)
}
/**
 *
 */
case class Hamming[V <: Seq[Int]](final val id: MetricID = 4) extends HammingMeta with BinaryDistance[V] {
	/**
	  * The Hamming distance with or without squareRoot
	  * @return The Hamming distance between dot1 and dot2
	  */
	def d(dot1: V, dot2: V): Double = hamming(dot1, dot2)
	/**
	  * The Hamming distance with or without squareRoot
	  * @return The Hamming distance between dot1 and dot2
	  */
	def d(dot1: BinaryVector[V], dot2: BinaryVector[V]): Double = hamming(dot1.vector, dot2.vector)
}