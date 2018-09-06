package clustering4ever.util

import scala.reflect.ClassTag
import scala.math.sqrt
import scala.collection.GenSeq
import spire.math.{Numeric => SNumeric}
/**
 * @author Beck Gaël
 * Object which gather common operation on Seq[N] and GenSeq[Seq[N]]
 */
object SumVectors {
	/**
	 * Sum two vector of Numeric into one
	 */
	def sumVectors[N, S <: Seq[N]](a: S, b: S)(implicit num: Numeric[N]): S = a.zip(b).map{ case (c, d) => num.plus(c, d) }.asInstanceOf[S]
	/**
	 * Reduce an Array[Array[N]] into an Array[N]
	 */
	def sumColumnMatrix[N: Numeric, S <: Seq[N]](cluster: GenSeq[S]): S = cluster.reduce(sumVectors[N, S](_, _))
	/**
	 * Reduce Array of multiple vectors
	 */
	def reduceMultipleVectorsMatrice[S <: Seq[Double]](a: GenSeq[S], b: GenSeq[S]) = a.zip(b).map{ case (c, d) => sumVectors[Double, S](c, d) }.asInstanceOf[S]
	/**
	 * Make the dot product of the difference dot1 - dot2
	 * @return dor product of difference
	 */
	def diffDotProduct[S <: Seq[Double]](dot1: S, dot2: S) = {
		var s = 0D
		var i = 0
		while( i < dot1.size ) {
			val diff = dot1(i) - dot2(i)
			s += diff * diff
			i += 1
		}
		s		
	}
	/**
	 * @return the centroid of the given cluster composed by real vectors
	 */
	def obtainMean[S <: Seq[Double]](cluster: GenSeq[S]): S = sumColumnMatrix[Double, S](cluster).map(_ / cluster.size).asInstanceOf[S]
	/**
	 * @return the centroid of the given cluster composed by binary vectors
	 */
	def obtainMode[S <: Seq[Int]](cluster: GenSeq[S]): S = sumColumnMatrix[Int, S](cluster).map( v => if( 2 * v >= cluster.size ) 1 else 0 ).asInstanceOf[S]

	def norm[S <: Seq[Double]](dot1: S): Double = {
	  var s = 0D
	  var i = 0
	  while( i < dot1.size ) {
	    val v = dot1(i)
	    s += v * v
	    i += 1
	  }
	  sqrt(s)
	}

	def dotProd[S <: Seq[Double]](dot1: S, dot2: S): Double = {
		var dp = 0D
		var i = 0
		while( i < dot1.size ) dp += dot1(i) * dot2(i)
		dp
	}
}