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
	override def d(vector1: immutable.Seq[Int], vector2: immutable.Seq[Int]): Double = 
	{
		var dh = 0D
		for( idx <- 0 until vector1.size ) dh += vector1(idx) ^ vector2(idx)
		dh
	}
	
}