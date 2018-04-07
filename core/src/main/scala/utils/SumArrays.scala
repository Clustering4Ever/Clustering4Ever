package clustering4ever.util

import _root_.scala.reflect.ClassTag

/**
 * @author Beck Gaël
 * Object which gather common operation on arrays and datasets matrix
 **/
object SumArrays
{
	/**
	 * Sum two vector of Numeric into one
	 **/
	def sumArraysNumerics[T](a: Array[T], b: Array[T])(implicit num: Numeric[T], ct: ClassTag[T]) : Array[T] = for( i <- a.indices.toArray ) yield( num.plus(a(i), b(i)) )

	/**
	 * Reduce an Array[Array[T]] into an Array[T]
	 **/
	def sumColumnArrays[T](cluster: Array[Array[T]])(implicit num: Numeric[T], ct: ClassTag[T]): Array[T] = cluster.reduce(sumArraysNumerics(_, _))

	/**
	 * Return the centroid of the given cluster
	 **/
	def obtainCentroid[T](cluster: Array[Array[T]])(implicit num: Numeric[T], ct: ClassTag[T]): Array[Double] = sumColumnArrays(cluster).map( t => num.toDouble(t) / cluster.size )

}