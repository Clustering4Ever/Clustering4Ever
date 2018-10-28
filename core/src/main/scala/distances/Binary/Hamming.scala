package clustering4ever.math.distances.binary
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
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
class Hamming[V[Int] <: Seq[Int]] extends HammingMeta with BinaryDistance[V] {
	/**
	  * The Hamming distance with or without squareRoot
	  * @return The Hamming distance between dot1 and dot2
	  */
	def d(dot1: V[Int], dot2: V[Int]): Double = hamming(dot1, dot2)
}
/**
 * The easy to use Hamminh distance for vectors =:= mutable.ArrayBuffer[Int]
 */
class EasyHamming extends Hamming[mutable.ArrayBuffer]
/**
 *
 */
class HammingClusterizable[ID: Numeric, O, V[Int] <: Seq[Int], D <: Hamming[V], Cz <: BinaryClusterizable[ID, O, V[Int], Cz]](val classicalMetric: D) extends HammingMeta with BinaryClusterizableDistance[Cz, V, D] {
	/**
	  * The Hamming distance with or without squareRoot
	  * @return The Hamming distance between dot1 and dot2
	  */
	def d(dot1: Cz, dot2: Cz): Double = hamming(dot1.vector, dot2.vector)
}