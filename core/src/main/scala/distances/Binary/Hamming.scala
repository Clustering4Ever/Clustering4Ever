package clustering4ever.math.distances.binary
/**
 * @author Beck Gaël
 */
import clustering4ever.math.distances.BinaryDistance
import scala.collection.mutable
import clustering4ever.scala.clusterizables.SimpleBinaryClusterizable
import clustering4ever.math.distances.BinaryClusterizableDistance

trait HammingMeta extends Serializable {

	protected def hamming[V <: Seq[Int]](dot1: V, dot2: V): Double = {
		var d = 0D
		var i = 0
		while( i < dot1.size ) {
			d += dot1(i) ^ dot2(i)
			i += 1
		}
		d
	}
}

class Hamming[V <: Seq[Int]] extends HammingMeta with BinaryDistance[V] {
	/**
	  * The Hamming distance with or without squareRoot
	  * @return The Hamming distance between dot1 and dot2
	  */
	def d(dot1: V, dot2: V): Double = hamming[V](dot1, dot2)
}

class HammingClusterizable[ID: Numeric, O, V <: Seq[Int]](val classicalMetric: Hamming[V]) extends HammingMeta with BinaryClusterizableDistance[SimpleBinaryClusterizable[ID, O, V], V] {
	/**
	  * The Hamming distance with or without squareRoot
	  * @return The Hamming distance between dot1 and dot2
	  */
	def d(dot1: SimpleBinaryClusterizable[ID, O, V], dot2: SimpleBinaryClusterizable[ID, O, V]): Double = hamming[V](dot1.vector, dot2.vector)

	def obtainClassicalDistance(): Hamming[V] = new Hamming[V]
}