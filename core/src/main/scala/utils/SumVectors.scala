package clustering4ever.util

import scala.reflect.ClassTag
import scala.math.pow
import scala.collection.GenSeq
/**
 * @author Beck Gaël
 * Object which gather common operation on arrays and datasets matrix
 */
object SumVectors
{
	/**
	 * Sum two vector of Numeric into one
	 */
	def sumVectors[T: ClassTag, S <: Seq[T]](a: S, b: S)(implicit num: Numeric[T]): S = a.zip(b).map{ case (c, d) => num.plus(c, d) }.asInstanceOf[S]
	/**
	 * Reduce an Array[Array[T]] into an Array[T]
	 */
	def sumColumnMatrix[T: Numeric : ClassTag, S <: Seq[T]](cluster: GenSeq[S]): S = cluster.reduce(sumVectors[T, S](_, _))
	/**
	 * Reduce Array of multiple vectors
	 */
	def reduceMultipleVectorsMatrice[S <: Seq[Double]](a: GenSeq[S], b: GenSeq[S]) = a.zip(b).map{ case (c, d) => sumVectors[Double, S](c, d) }.asInstanceOf[S]
	/**
	 * Make the dot product of the difference v1 - v2
	 * @return dor product of difference
	 */
	def diffDotProduct[S <: Seq[Double]](v1: S, v2: S) =
	{
		var sum = 0D
		v1.zip(v2).foreach{ case (a, b) => sum += pow(a - b, 2) }
		sum		
	}
	/**
	 * @return the centroid of the given cluster composed by real vectors
	 */
	def obtainMean[S <: Seq[Double]](cluster: GenSeq[S]): S = sumColumnMatrix[Double, S](cluster).map( _ / cluster.size ).asInstanceOf[S]
	/**
	 * @return the centroid of the given cluster composed by binary vectors
	 */
	def obtainMode[S <: Seq[Int]](cluster: GenSeq[S]): S = sumColumnMatrix[Int, S](cluster).map( v => if( 2 * v >= cluster.size ) 1 else 0 ).asInstanceOf[S]
}