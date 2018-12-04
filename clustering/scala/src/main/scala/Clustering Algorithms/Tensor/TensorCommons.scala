package org.clustering4ever.scala.clustering.tensor
/**
 * @author ANDRIANTSIORY Dina Faneva, Beck Gaël
 */
import scala.reflect.ClassTag
import breeze.linalg.{DenseVector, DenseMatrix}
import spire.math.{Numeric => SNumeric}
import scala.collection.mutable._
/**
 *
 */
object TensorCommons {

  // The index of top k element of the vector

	def obtainTopkIndices[@specialized(Int, Double) N](vector: DenseVector[N], k: Int)(implicit num: SNumeric[N], ev: ClassTag[N]): Array[Int] = vector.toArray.zipWithIndex.sortWith( (x, y) => num.gt(x._1, y._1) ).take(k).map(_._2)

  //Represent the data as a tensor. 
	def dataToTensor( data: Array[Array[Double]], n1: Int, n2: Int, n3: Int): ArrayBuffer[DenseMatrix[Double]] ={
		val r = data.length
		val c = data.head.length
		def datatomatrix(buf: Array[Array[Double]], m: DenseMatrix[Double]): DenseMatrix[Double] = {
		  for (i <- 0 until r) {
		    for (j <- 0 until c) {
		      m(i,j) = data(i)(j)
		    }
		  }
		  m
		}
		var dm = datatomatrix(data, DenseMatrix.zeros[Double](r,c))

		def todm(ds: DenseMatrix[Double], t: ArrayBuffer[DenseMatrix[Double]], a: Int, b: Int, c: Int): ArrayBuffer[DenseMatrix[Double]] = {
		  var h = 0
		  for (k <- 0 until c) {
		    var m = DenseMatrix.zeros[Double](a, b)
		    for (i <- 0  until a){
		        for (j <- 0 until b){
		         h = j + ( k * b )
		         m(i, j) = ds(i, h)
		        }
		      }
		  t += m
		}
		  t
		}

		val tens = ArrayBuffer[DenseMatrix[Double]]()
		val tensor = todm(dm, tens, n1, n2, n3)
		tensor

	}	

}