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
	def sumArraysNumerics[T](a: Seq[T], b: Seq[T])(implicit num: Numeric[T], ct: ClassTag[T]): Seq[T] =
		a.zip(b).map{ case (c, d) => num.plus(c, d) }
	/**
	 * Reduce an Array[Array[T]] into an Array[T]
	 */
	def sumColumnArrays[T](cluster: GenSeq[Seq[T]])(implicit num: Numeric[T], ct: ClassTag[T]): Seq[T] =
		cluster.reduce(sumArraysNumerics(_, _))
	/**
	 * Return the centroid of the given cluster
	 */
	def obtainMean(cluster: GenSeq[Seq[Double]]): Seq[Double] =
		sumColumnArrays[Double](cluster).map( _ / cluster.size )
	/**
	 * Reduce Array of multiple vectors
	 */
	def reduceMultipleVectorsMatrice(a: Seq[Seq[Double]], b: Seq[Seq[Double]]) =
		a.zip(b).map{ case (c, d) => sumArraysNumerics[Double](c, d) }
	/**
	 * Make the dot product of the difference v1 - v2
	 * @return dor product of difference
	 */
	def diffDotProduct(v1: Seq[Double], v2: Seq[Double]) =
		v1.zip(v2).map{ case (a, b) => pow(a - b, 2) }.sum		
	/**
	 * Compute the mode of a cluster
	 */
	def obtainMode(cluster: GenSeq[Seq[Int]]): Seq[Int] =
		sumColumnArrays[Int](cluster).map( v => if( 2 * v >= cluster.size ) 1 else 0 )
}