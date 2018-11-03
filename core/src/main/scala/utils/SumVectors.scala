package clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.reflect.ClassTag
import scala.math.sqrt
import scala.collection.GenSeq
import scala.language.higherKinds
import spire.math.{Numeric => SNumeric}
import clustering4ever.scala.measurableclass.BinaryScalarVector
/**
 *
 */
object VectorsBasicOperationsImplicits {
	/**
	 *
	 */
	implicit def addClassicVectors[@specialized(Int, Double) N, V[N] <: Seq[N]](v1: V[N], altVectors: V[N])(implicit num: SNumeric[N]): V[N] = v1.zip(altVectors).map{ case (a, b) => num.plus(a, b) }.asInstanceOf[V[N]]
	/**
	 *
	 */
	implicit def addMixtVectors[Vb[Int] <: Seq[Int], Vs[Double] <: Seq[Double]](v1: BinaryScalarVector[Vb[Int], Vs[Double]], altVectors: BinaryScalarVector[Vb[Int], Vs[Double]]): BinaryScalarVector[Vb[Int], Vs[Double]] = {	
		val binaryPart = addClassicVectors(v1.binary, altVectors.binary)
		val scalarPart = addClassicVectors(v1.scalar, altVectors.scalar)
		new BinaryScalarVector(binaryPart, scalarPart)
	}
}
/**
 * Object which gather common operation on Vectors of any nature, aka scalar, binary, mixt
 */
object SumVectors {

	import VectorsBasicOperationsImplicits._
	/**
	 * add two vector no mather their types
	 */
	def sumVectors[V](v1: V, altVectors: V)(implicit f: (V, V) => V): V = f.apply(v1, altVectors)
	/**
	 * Reduce an Array[Array[N]] into an Array[N]
	 */
	def sumColumnMatrix[V](cluster: GenSeq[V])(implicit f: (V, V) => V): V = cluster.reduce(sumVectors(_, _))
	/**
	 * Reduce Array of multiple vectors
	 */
	def reduceMultipleVectorsMatrice[V[Double] <: Seq[Double], It[V] <: Seq[V]](a: It[V[Double]], b: It[V[Double]]) = a.zip(b).map{ case (c, d) => sumVectors(c, d) }.asInstanceOf[It[V[Double]]]
	/**
	 *
	 */
	def norm[V <: Seq[Double]](dot: V): Double = {
	  var s = 0D
	  var i = 0
	  while( i < dot.size ) {
	    val v = dot(i)
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