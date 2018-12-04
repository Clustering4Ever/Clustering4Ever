package org.clustering4ever.scala.clustering.tensor
/**
 * @author ANDRIANTSIORY Dina Faneva, Beck Gaël
 */
import scala.reflect.ClassTag
import breeze.linalg.{DenseVector, DenseMatrix}
import spire.math.{Numeric => SNumeric}
import scala.collection.mutable
/**
 *
 */
object TensorCommons {

	def obtainTopkIndices[@specialized(Int, Double) N](vector: DenseVector[N], k: Int)(implicit num: SNumeric[N], ev: ClassTag[N]): Array[Int] = vector.toArray.zipWithIndex.sortWith( (x, y) => num.gt(x._1, y._1) ).take(k).map(_._2)
}