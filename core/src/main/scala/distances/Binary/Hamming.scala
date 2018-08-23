package clustering4ever.math.distances.binary

import clustering4ever.math.distances.BinaryDistance

/**
 * @author Beck Gaël
 **/
class Hamming extends BinaryDistance[Seq[Int]]
{
	/**
	  * The famous hamming distance implemented in its fast mono thread scala version
	  */
	def d(vector1: Seq[Int], vector2: Seq[Int]): Double = vector1.zip(vector2).map{ case (a, b) => a ^ b }.sum
	
}