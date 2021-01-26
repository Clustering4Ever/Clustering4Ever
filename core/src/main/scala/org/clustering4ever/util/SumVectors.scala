package org.clustering4ever.util

/**
 * @author Beck Gaël
 */
import org.clustering4ever.roottraits.{BinaryVector, MixedVector, ScalarVector}
import spire.math.{Numeric => SNumeric}

import scala.collection.{GenSeq, mutable}
import scala.language.higherKinds
import scala.math.sqrt
import scala.reflect.ClassTag
/**
 *
 */
object VectorsAddOperationsImplicits extends Serializable {
	/**
	 * Check perf before use it
	 */
	final private def addRawSimpleVectorOld[@specialized(Float, Double, Int, Long) N: ClassTag](v1: Array[N], v2: Array[N])(implicit num: SNumeric[N]): Array[N] = {
		// val res = Array.ofDim[N](v1.size)
		// @annotation.tailrec
		// def go(i: Int): Unit = {
		// 	res(i) = num.plus(v1(i), v2(i))
		// 	if(i < v1.size - 1) go(i + 1)
		// }
		// go(0)
		// res
	    val output = Array.ofDim[N](v1.length)
	    var i = 0
	    while (i < v1.length) {
	      output(i) = num.plus(v1(i), v2(i))
	      i += 1
	    }
	    output
	}
	/**
	 * new faster version
	 */
	final def addRawSimpleVector[N : ClassTag](v1: Array[N], v2: Array[N])(implicit num: SNumeric[N]): Array[N] = {
	  val output = Array.ofDim[N](v1.length)
	  v1.indices.foreach(i => output(i) = num.plus(v1(i), v2(i)) )
	  output
	}
	/**
	 *
	 */
	final def addRawSimpleVector(v1: Array[Double], v2: Array[Double]): Array[Double] = {
		val output = Array.ofDim[Double](v1.length)
		var i = 0
		while (i < v1.length) {
			output(i) = v1(i) + v2(i)
			i += 1
		}
		output
	}
	/**
	 *
	 */
	final def addRawSimpleVector(v1: Array[Int], v2: Array[Int]): Array[Int] = {
		val output = Array.ofDim[Int](v1.length)
		var i = 0
		while (i < v1.length) {
			output(i) = v1(i) + v2(i)
			i += 1
		}
		output
	}

	/**
	 *
	 */
	final implicit def addRawScalarVectors(v1: Array[Double], v2: Array[Double]): Array[Double] = addRawSimpleVector(v1, v2)
	/**
	 *
	 */
	final implicit def addRawBinaryVectors(v1: Array[Int], v2: Array[Int]): Array[Int] = addRawSimpleVector(v1, v2)
	/**
	 *
	 */
	final implicit def addScalarVectors(v1: ScalarVector, v2: ScalarVector): ScalarVector = ScalarVector(addRawScalarVectors(v1.vector, v2.vector))
	/**
	 *
	 */
	final implicit def addBinaryVectors(v1: BinaryVector, v2: BinaryVector): BinaryVector = BinaryVector(addRawBinaryVectors(v1.vector, v2.vector))
	/**
	 *
	 */
	final implicit def addMixedVectors(v1: MixedVector, v2: MixedVector): MixedVector = {
		val binaryPart = addRawBinaryVectors(v1.binary, v2.binary)
		val scalarPart = addRawScalarVectors(v1.scalar, v2.scalar)
		MixedVector(binaryPart, scalarPart)
	}
}

/**
 * Object which gather common operation on Vectors of any nature, aka scalar, binary, mixt
 */
object SumVectors extends Serializable {
	/**
	 * add two vector no mather their types
	 */
	final def sumVectors[V](v1: V, v2: V)(implicit f: (V, V) => V): V = f(v1, v2)
	/**
	 * Reduce an Array[Array[N]] into an Array[N]
	 */
	final def sumColumnMatrix[V](cluster: GenSeq[V])(implicit f: (V, V) => V): V = cluster.reduce(sumVectors(_, _))
	/**
	 * Reduce Seq of multiple vectors
	 */
	final def sumAlignedVectorsMatrice[S[X] <: Seq[X]](a: S[Array[Double]], b: S[Array[Double]])(implicit f: (Array[Double], Array[Double]) => Array[Double]) = {
		val range = a.indices
		val builder = a.genericBuilder[Array[Double]].asInstanceOf[mutable.Builder[Array[Double], S[Array[Double]]]]
		builder.sizeHint(a.size)
		@annotation.tailrec
		def go(i: Int): Unit = {
			builder += sumVectors(a(i), b(i))
			if(i < range.size - 1) go(i + 1)
		}
		go(0)
		range.foreach( i => builder += sumVectors(a(i), b(i)) )
		builder.result
	}
	/**
	 *
	 */
	final def dotProduct[N](dot1: Array[N], dot2: Array[N])(implicit num: SNumeric[N]): N = {
		@annotation.tailrec
		def go(i: Int, sum: N): N = {
			val res = num.plus(sum, num.times(dot1(i), dot2(i)))
			if(i < dot1.length - 1) go(i + 1, res)
			else res
		}
		go(0, num.zero)
	}
	/**
	 *
	 */
	final def euclideanNorm(dot: Array[Double]): Double = sqrt(dotProduct(dot, dot))
	/**
	 *
	 */
	final def euclideanNorm(dot: ScalarVector): Double = euclideanNorm(dot.vector)
}