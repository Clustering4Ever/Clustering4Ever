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
 * @tparam Array[Int]
 */
trait HammingMeta extends Serializable {
	/**
	 *
	 */
	protected final def hamming(v1: Array[Int], v2: Array[Int]): Double = {
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
 * @tparam Array[Int]
 */
final case class RawHamming(final val id: MetricID = 4) extends HammingMeta with RawBinaryDistance {
	/**
	  * The Hamming distance with or without squareRoot
	  * @return The Hamming distance between v1 and v2
	  */
	final def d(v1: Array[Int], v2: Array[Int]): Double = hamming(v1, v2)
}
/**
 * @tparam Array[Int]
 */
final case class Hamming(final val id: MetricID = 4) extends HammingMeta with BinaryDistance {
	/**
	  * The Hamming distance with or without squareRoot
	  * @return The Hamming distance between v1 and v2
	  */
	final def dRaw(v1: Array[Int], v2: Array[Int]): Double = hamming(v1, v2)
	/**
	  * The Hamming distance with or without squareRoot
	  * @return The Hamming distance between v1 and v2
	  */
	final def d(v1: BinaryVector, v2: BinaryVector): Double = hamming(v1.vector, v2.vector)
}