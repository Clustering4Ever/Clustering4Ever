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
 * @tparam V
 */
trait HammingMeta[V <: Seq[Int]] extends Serializable {
	/**
	 *
	 */
	protected final def hamming(v1: V, v2: V): Double = {
		@annotation.tailrec
		def go(d: Double, i: Int): Double = {
			if( i < v1.size ) {
			  go(d + (v1(i) ^ v2(i)), i + 1)
			}
			else d
		}
		go(0D, 0)
	}
}
/**
 * @tparam V
 */
final case class RawHamming[V <: Seq[Int]](final val id: MetricID = 4) extends HammingMeta[V] with RawBinaryDistance[V] {
	/**
	  * The Hamming distance with or without squareRoot
	  * @return The Hamming distance between v1 and v2
	  */
	def d(v1: V, v2: V): Double = hamming(v1, v2)
}
/**
 * @tparam V
 */
final case class Hamming[V <: Seq[Int]](final val id: MetricID = 4) extends HammingMeta[V] with BinaryDistance[V] {
	/**
	  * The Hamming distance with or without squareRoot
	  * @return The Hamming distance between v1 and v2
	  */
	def d(v1: V, v2: V): Double = hamming(v1, v2)
	/**
	  * The Hamming distance with or without squareRoot
	  * @return The Hamming distance between v1 and v2
	  */
	def d(v1: BinaryVector[V], v2: BinaryVector[V]): Double = hamming(v1.vector, v2.vector)
}