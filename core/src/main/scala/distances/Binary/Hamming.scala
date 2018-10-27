package clustering4ever.math.distances.binary
/**
 * @author Beck Gaël
 */
import clustering4ever.math.distances.BinaryDistance
import scala.collection.mutable
import clustering4ever.scala.clusterizables.BinaryClusterizable
import clustering4ever.math.distances.BinaryClusterizableDistance
/**
 *
 */
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
/**
 *
 */
class Hamming[V <: Seq[Int]] extends HammingMeta with BinaryDistance[V] {
	/**
	  * The Hamming distance with or without squareRoot
	  * @return The Hamming distance between dot1 and dot2
	  */
	def d(dot1: V, dot2: V): Double = hamming[V](dot1, dot2)
}
/**
 * The easy to use Hamminh distance for vectors =:= mutable.ArrayBuffer[Int]
 */
class EasyHamming extends Hamming[mutable.ArrayBuffer[Int]]
/**
 *
 */
class HammingClusterizable[ID: Numeric, O, V <: Seq[Int], D <: Hamming[V], Cz <: BinaryClusterizable[ID, O, V, Cz]](val classicalMetric: D) extends HammingMeta with BinaryClusterizableDistance[Cz, V, D] {
	/**
	  * The Hamming distance with or without squareRoot
	  * @return The Hamming distance between dot1 and dot2
	  */
	def d(dot1: Cz, dot2: Cz): Double = hamming[V](dot1.vector, dot2.vector)
}