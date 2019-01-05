package org.clustering4ever.math.distances.binary
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import org.clustering4ever.math.distances.{Distance, BinaryDistance, RawBinaryDistance}
import scala.collection.mutable
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.scala.vectors.{BinaryVector, GVector}
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
class RawHamming[V <: Seq[Int]] extends HammingMeta with RawBinaryDistance[V] {
	/**
	  * The Hamming distance with or without squareRoot
	  * @return The Hamming distance between dot1 and dot2
	  */
	def d(dot1: V, dot2: V): Double = hamming(dot1, dot2)
}
/**
 *
 */
class Hamming[V <: Seq[Int]] extends HammingMeta with BinaryDistance[V] {
	/**
	  * The Hamming distance with or without squareRoot
	  * @return The Hamming distance between dot1 and dot2
	  */
	def d(dot1: BinaryVector[V], dot2: BinaryVector[V]): Double = hamming(dot1.vector, dot2.vector)
}
/**
 * The easy to use Hamminh distance for vectors =:= mutable.ArrayBuffer[Int]
 */
class EasyHamming extends Hamming[mutable.ArrayBuffer[Int]]
/**
 *
 */
// class HammingClusterizable[V <: Seq[Int], D[X <: Seq[Int]] <: Hamming[X]](val classicalMetric: D[V]) extends HammingMeta with BinaryClusterizableDistance[V, D[V]] {
// 	/**
// 	  * The Hamming distance with or without squareRoot
// 	  * @return The Hamming distance between dot1 and dot2
// 	  */
// 	def d[@specialized(Int, Long) ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz[X, Y, Z]]](dot1: Cz[ID, O, BinaryVector[V]], dot2: Cz[ID, O, BinaryVector[V]]): Double = hamming(dot1.workingVector.vector, dot2.workingVector.vector)
// }