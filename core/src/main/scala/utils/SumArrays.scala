package clustering4ever.util

import _root_.scala.reflect.ClassTag
import _root_.scala.math.pow

/**
 * @author Beck Gaël
 * Object which gather common operation on arrays and datasets matrix
 **/
object SumArrays
{
	/**
	 * Sum two vector of Numeric into one
	 **/
	def sumArraysNumerics[T](a: Array[T], b: Array[T])(implicit num: Numeric[T], ct: ClassTag[T]) : Array[T] =
	{
		for( i <- a.indices.toArray ) yield( num.plus(a(i), b(i)) )
	}
	/**
	 * Reduce an Array[Array[T]] into an Array[T]
	 **/
	def sumColumnArrays[T](cluster: Seq[Array[T]])(implicit num: Numeric[T], ct: ClassTag[T]): Array[T] =
	{
		cluster.reduce(sumArraysNumerics(_, _))
	}
	/**
	 * Return the centroid of the given cluster
	 **/
	def obtainMean(cluster: Seq[Array[Double]]): Array[Double] =
	{
		sumColumnArrays(cluster).map( _ / cluster.size )
	}
	/**
	 * Reduce Array of multiple vectors
	 **/
	def reduceMultipleVectorsMatrice(a: Array[Array[Double]], b: Array[Array[Double]]) =
	{
		for( i <- a.indices.toArray ) yield sumArraysNumerics(a(i), b(i))
	}
	/**
	 * Make the dot product of the difference v1 - v2
	 * @return dor product of difference
	 **/
	def diffDotProduct(v1: Array[Double], v2: Array[Double]) =
	{
		( for( i <- v1.indices.toArray ) yield (v1(i) - v2(i)) ).map(pow(_, 2)).sum			
	}
}