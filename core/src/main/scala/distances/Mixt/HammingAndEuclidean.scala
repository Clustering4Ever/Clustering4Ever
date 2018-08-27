package clustering4ever.math.distances.mixt

import scala.math.{pow, sqrt}
import clustering4ever.math.distances.MixtDistance
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.math.distances.MixtDistanceClusterizable
import clustering4ever.scala.clusterizables.MixtClusterizable
import scala.collection.immutable

/**
 * @author Beck Gaël
 **/
trait HammingAndEuclideanMeta[Vb <: immutable.Seq[Int], Vs <: immutable.Seq[Double]] extends Serializable
{
	protected val α: Double

	protected def hammingAndEuclidean(vector1: BinaryScalarVector[Vb, Vs], vector2: BinaryScalarVector[Vb, Vs]): Double =
	{
		val db = vector1.binary.zip(vector2.binary).map{ case (a, b) => a ^ b }.sum		
		val ds = sqrt(vector1.scalar.zip(vector2.scalar).map{ case (a, b) => pow(a - b, 2) }.sum)
		
		if( α == 0 )
		{
			val nbDim = vector1.binary.size + vector1.scalar.size
			db * (vector1.binary.size.toDouble / nbDim) + ds * (vector1.scalar.size.toDouble / nbDim) 
		}
		else db + α * ds
	}
}

class HammingAndEuclidean[Vb <: immutable.Seq[Int], Vs <: immutable.Seq[Double]](val α: Double = 0D) extends HammingAndEuclideanMeta[Vb, Vs] with MixtDistance[Vb, Vs]
{
	/**
	 *	
	 */
	override def d(vector1: BinaryScalarVector[Vb, Vs], vector2: BinaryScalarVector[Vb, Vs]): Double = hammingAndEuclidean(vector1, vector2)
	
}

class HammingAndEuclideanClusterizable[ID: Numeric, Obj, Vb <: immutable.Seq[Int], Vs <: immutable.Seq[Double]](val α: Double = 0D) extends HammingAndEuclideanMeta[Vb, Vs] with MixtDistanceClusterizable[ID, Obj, Vb, Vs]
{
	/**
	 *	
	 */
	override def d(vector1: MixtClusterizable[ID, Obj, Vb, Vs], vector2: MixtClusterizable[ID, Obj, Vb, Vs]): Double = hammingAndEuclidean(vector1.vector, vector2.vector)
}