package clustering4ever.util

import scala.reflect.ClassTag
import scala.math.pow
import scala.collection.{immutable, GenSeq}

/**
 * @author Beck Gaël
 * Object which gather common operation on arrays and datasets matrix
 */
object SumArrays
{
	/**
	 * Sum two vector of Numeric into one
	 */
	def sumArraysNumerics[T: ClassTag](a: GenSeq[T], b: GenSeq[T])(implicit num: Numeric[T]): GenSeq[T] =
		a.zip(b).map{ case (c, d) => num.plus(c, d) }
	/**
	 * Sum two vector of Numeric into one
	 */
	// def sumArraysNumericsGen[T: ClassTag, V >: GenSeq[T] <: GenSeq[T]](a: V, b: V)(implicit num: Numeric[T]): V =
	def sumArraysNumericsGen[T: ClassTag, V <: GenSeq[T]](a: V, b: V)(implicit num: Numeric[T]): V =
		a.zip(b).map{ case (c, d) => num.plus(c, d) }.asInstanceOf[V]
	/**
	 * Reduce an Array[Array[T]] into an Array[T]
	 */
	def sumColumnArrays[T: ClassTag](cluster: GenSeq[GenSeq[T]])(implicit num: Numeric[T]): GenSeq[T] =
		cluster.reduce(sumArraysNumerics[T](_, _))
	/**
	 * Reduce an Array[Array[T]] into an Array[T]
	 */
	// def sumColumnArraysGen[T: ClassTag, V >: GenSeq[T] <: GenSeq[T]](cluster: GenSeq[V])(implicit num: Numeric[T]): V =
	def sumColumnArraysGen[T: ClassTag, V <: GenSeq[T]](cluster: GenSeq[V])(implicit num: Numeric[T]): V =
		cluster.reduce(sumArraysNumericsGen[T, V](_, _))
	/**
	 * Reduce Array of multiple vectors
	 */
	def reduceMultipleVectorsMatrice(a: GenSeq[GenSeq[Double]], b: GenSeq[GenSeq[Double]]) =
		a.zip(b).map{ case (c, d) => sumArraysNumerics[Double](c, d) }
	/**
	 * Make the dot product of the difference v1 - v2
	 * @return dor product of difference
	 */
	def diffDotProduct(v1: GenSeq[Double], v2: GenSeq[Double]) =
		v1.zip(v2).map{ case (a, b) => pow(a - b, 2) }.sum		
	/**
	 * Return the centroid of the given cluster
	 */
	def obtainMean(cluster: GenSeq[GenSeq[Double]]): GenSeq[Double] =
		sumColumnArrays[Double](cluster).map( _ / cluster.size )
	/**
	 * Return the centroid of the given cluster
	 */
	// def obtainMeanGen[V >: GenSeq[Double] <: GenSeq[Double]](cluster: GenSeq[V]): V =
	def obtainMeanGen[V <: GenSeq[Double]](cluster: GenSeq[V]): V =
		sumColumnArraysGen[Double, V](cluster).map( _ / cluster.size ).asInstanceOf[V]
	/**
	 * Compute the mode of a cluster
	 */
	def obtainMode(cluster: GenSeq[GenSeq[Int]]): GenSeq[Int] =
		sumColumnArrays[Int](cluster).map( v => if( 2 * v >= cluster.size ) 1 else 0 )
	/**
	 * Return the centroid of the given cluster
	 */
	// def obtainModeGen[V >: GenSeq[Int] <: GenSeq[Int]](cluster: GenSeq[V]): V =
	def obtainModeGen[V <: GenSeq[Int]](cluster: GenSeq[V]): V =
		sumColumnArraysGen[Int, V](cluster).map( v => if( 2 * v >= cluster.size ) 1 else 0 ).asInstanceOf[V]
}