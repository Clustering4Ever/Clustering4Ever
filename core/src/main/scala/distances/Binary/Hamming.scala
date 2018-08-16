package clustering4ever.math.distances.binary

import clustering4ever.math.distances.BinaryDistanceSeq
import scala.collection.immutable

/**
 * @author Beck Gaël
 **/
class Hamming extends BinaryDistanceSeq
{
	/**
	  * The famous hamming distance implemented in its fast mono thread scala version
	  */
	override def d(vector1: Seq[Int], vector2: Seq[Int]): Double = vector1.zip(vector2).map{ case (a, b) => a ^ b }.sum
	
}