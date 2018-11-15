package org.clustering4ever.math.distances.binary
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import org.clustering4ever.math.distances.BinaryDistance
import scala.collection.mutable
import org.clustering4ever.scala.clusterizables.Clusterizable
import org.clustering4ever.math.distances.BinaryClusterizableDistance
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
class Hamming[V <: Seq[Int]] extends HammingMeta with BinaryDistance[V] {
	/**
	  * The Hamming distance with or without squareRoot
	  * @return The Hamming distance between dot1 and dot2
	  */
	def d(dot1: V, dot2: V): Double = hamming(dot1, dot2)
}
/**
 * The easy to use Hamminh distance for vectors =:= mutable.ArrayBuffer[Int]
 */
class EasyHamming extends Hamming[mutable.ArrayBuffer[Int]]
/**
 *
 */
class HammingClusterizable[@specialized(Int, Long) ID: Numeric, O, V <: Seq[Int], D <: Hamming[V], Cz <: Clusterizable[ID, O, V, Cz]](val classicalMetric: D) extends HammingMeta with BinaryClusterizableDistance[Cz, V, D] {
	/**
	  * The Hamming distance with or without squareRoot
	  * @return The Hamming distance between dot1 and dot2
	  */
	def d(dot1: Cz, dot2: Cz): Double = hamming(dot1.vector, dot2.vector)
}