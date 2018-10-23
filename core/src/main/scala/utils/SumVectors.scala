package clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.reflect.ClassTag
import scala.math.sqrt
import scala.collection.GenSeq
import spire.math.{Numeric => SNumeric}

/**
 * Object which gather common operation on Seq[N] and GenSeq[Seq[N]]
 */
object SumVectors {
	/**
	 * Sum two vector of Numeric into one
	 */
	def sumVectors[@specialized(Int, Double) N, V <: Iterable[N]](a: V, b: V)(implicit num: SNumeric[N]): V = a.zip(b).map{ case (c, d) => num.plus(c, d) }.asInstanceOf[V]
	/**
	 * Reduce an Array[Array[N]] into an Array[N]
	 */
	def sumColumnMatrix[@specialized(Int, Double) N: SNumeric, V <: Iterable[N]](cluster: GenSeq[V]): V = cluster.reduce(sumVectors[N, V](_, _))
	/**
	 * Reduce Array of multiple vectors
	 */
	def reduceMultipleVectorsMatrice[V <: Seq[Double], It <: Iterable[V]](a: It, b: It) = a.zip(b).map{ case (c, d) => sumVectors[Double, V](c, d) }.asInstanceOf[It]
	/**
	 * Make the dot product of the difference dot1 - dot2
	 * @return dor product of difference
	 */
	def diffDotProduct[V <: Seq[Double]](dot1: V, dot2: V) = {
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
	 *
	 */
	def norm[V <: Seq[Double]](dot1: V): Double = {
	  var s = 0D
	  var i = 0
	  while( i < dot1.size ) {
	    val v = dot1(i)
	    s += v * v
	    i += 1
	  }
	  sqrt(s)
	}
	/**
	 *
	 */
	def dotProd[V <: Seq[Double]](dot1: V, dot2: V): Double = {
		var dp = 0D
		var i = 0
		while( i < dot1.size ) {
			dp += dot1(i) * dot2(i)
			i += 1
		}
		dp
	}
}