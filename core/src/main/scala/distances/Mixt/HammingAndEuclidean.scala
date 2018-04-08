package clustering4ever.math.distances.mixt

import _root_.clustering4ever.math.distances.MixtDistance
import _root_.clustering4ever.scala.measurableclass.BinaryScalarVector
import _root_.scala.math.{pow, sqrt}

/**
 * @author Beck Gaël
 **/
class HammingAndEuclidean(α: Double = 0D) extends MixtDistance
{
	/**
	  * The famous hamming distance implemented in its fast mono thread scala version
	  */
	override def d(vector1: BinaryScalarVector, vector2: BinaryScalarVector): Double = 
	{
		var db = 0D
		for( idx <- 0 until vector1.binary.size ) db += vector1.binary(idx) ^ vector2.binary(idx)
		
		var ds = 0D
		for( idx <- 0 until vector1.scalar.size ) ds += sqrt(pow(vector1.scalar(idx) - vector2.scalar(idx), 2))
		
		if( α == 0 )
		{
			val nbDim = vector1.binary.size + vector1.scalar.size
			db * (vector1.binary.size.toDouble / nbDim) + ds * (vector1.scalar.size.toDouble / nbDim) 
		}
		else
		{
			db + α * ds
		}
	}
	
}