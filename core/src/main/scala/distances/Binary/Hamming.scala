package clustering4ever.math.distances.binary

import _root_.clustering4ever.math.distances.BinaryDistance

/**
 * @author Beck Gaël
 **/
class Hamming extends BinaryDistance
{
	/**
	  * The famous hamming distance implemented in its fast mono thread scala version
	  */
	override def d(vector1: Vector[Int], vector2: Vector[Int]): Double = 
	{
		var dh = 0D
		for( idx <- 0 until vector1.size ) dh += vector1(idx) ^ vector2(idx)
		dh
	}
	
}