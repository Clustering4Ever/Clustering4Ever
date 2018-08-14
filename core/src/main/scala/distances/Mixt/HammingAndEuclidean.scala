package clustering4ever.math.distances.mixt

import scala.math.{pow, sqrt}
import clustering4ever.math.distances.MixtDistance
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.math.distances.MixtDistanceClusterizable
import clustering4ever.scala.clusterizables.ClusterizableM

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
		for( idx <- vector1.binary.indices ) db += vector1.binary(idx) ^ vector2.binary(idx)
		
		var ds = 0D
		for( idx <- vector1.scalar.indices ) ds += sqrt(pow(vector1.scalar(idx) - vector2.scalar(idx), 2))
		
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

class HammingAndEuclideanClusterizable[ID: Numeric, Obj](α: Double = 0D) extends MixtDistanceClusterizable[ID, Obj]
{
	/**
	  * The famous hamming distance implemented in its fast mono thread scala version
	  */
	override def d(vector1: ClusterizableM[ID, Obj], vector2: ClusterizableM[ID, Obj]): Double = 
	{
		var db = 0D
		for( idx <- vector1.vector._1.indices ) db += vector1.vector._1(idx) ^ vector2.vector._1(idx)
		
		var ds = 0D
		for( idx <- vector1.vector._2.indices ) ds += sqrt(pow(vector1.vector._2(idx) - vector2.vector._2(idx), 2))
		
		if( α == 0 )
		{
			val nbDim = vector1.vector._1.size + vector1.vector._2.size
			db * (vector1.vector._1.size.toDouble / nbDim) + ds * (vector1.vector._2.size.toDouble / nbDim) 
		}
		else
		{
			db + α * ds
		}
	}
	
}