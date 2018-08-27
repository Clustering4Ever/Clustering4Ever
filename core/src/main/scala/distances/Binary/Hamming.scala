package clustering4ever.math.distances.binary

import clustering4ever.math.distances.BinaryDistance
import scala.collection.immutable

/**
 * @author Beck Gaël
 **/
class Hamming extends BinaryDistance[immutable.Seq[Int]]
{
	/**
	  * The famous hamming distance implemented in its fast mono thread scala version
	  */
	def d(vector1: immutable.Seq[Int], vector2: immutable.Seq[Int]): Double = vector1.zip(vector2).map{ case (a, b) => a ^ b }.sum
	
}