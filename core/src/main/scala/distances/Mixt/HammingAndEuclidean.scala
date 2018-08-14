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
		val db = vector1.binary.zip(vector2.binary).map{ case (a, b) => a ^ b }.sum		
		val ds = vector1.scalar.zip(vector2.scalar).map{ case (a, b) => pow(a - b, 2) }.sum
		
		if( α == 0 )
		{
			val nbDim = vector1.binary.size + vector1.scalar.size
			db * (vector1.binary.size.toDouble / nbDim) + ds * (vector1.scalar.size.toDouble / nbDim) 
		}
		else db + α * ds
	}
	
}

class HammingAndEuclideanClusterizable[ID: Numeric, Obj](α: Double = 0D) extends MixtDistanceClusterizable[ID, Obj]
{
	/**
	  * The famous hamming distance implemented in its fast mono thread scala version
	  */
	override def d(vector1: ClusterizableM[ID, Obj], vector2: ClusterizableM[ID, Obj]): Double = 
	{		
		val db = vector1.vector._1.zip(vector2.vector._1).map{ case (a, b) => a ^ b }.sum		
		val ds = vector1.vector._2.zip(vector2.vector._2).map{ case (a, b) => pow(a - b, 2) }.sum

		if( α == 0 )
		{
			val nbDim = vector1.vector._1.size + vector1.vector._2.size
			db * (vector1.vector._1.size.toDouble / nbDim) + ds * (vector1.vector._2.size.toDouble / nbDim) 
		}
		else db + α * ds
	}
	
}