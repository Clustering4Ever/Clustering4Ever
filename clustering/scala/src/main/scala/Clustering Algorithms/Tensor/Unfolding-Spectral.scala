package org.clustering4ever.scala.clustering.tensor
/**
 * @author ANDRIANTSIORY Dina Faneva, Beck Gaël
 */
import scala.collection.mutable
import breeze.linalg.svd.SVD
import breeze.stats.mean
import breeze.linalg._
import scala.math._
import org.clustering4ever.clustering.ClusteringAlgorithm
/**
 *
 */
class UnfoldingSpectral(val k1: Int, val k2: Int) extends ClusteringAlgorithm {
  /**
   * This function compute the ufolding mode-3 of the principal tensor
   * By a bijection map between the set of trajectory and the column of a new matrix
   */
  @annotation.tailrec
  private final def unfolding(t: mutable.ArrayBuffer[DenseMatrix[Double]], m: DenseMatrix[Double], i: Int, j: Int, k: Int): DenseMatrix[Double] = {
    val n2 = t.head.cols
      if( j < t.head.cols && k < t.length ) {
        m(k, (i * n2) + j) = t(k)(i, j)
        unfolding(t, m, i, j, k + 1)
      }
      else if( k == t.length && j < t.head.cols ) {
        unfolding(t, m, i, j + 1 , 0)
      }
      else if( i < t.head.rows - 1 ) {
        unfolding(t, m, i + 1, 0, 0)
      }
      else {
        m
      }
  }
  /**
   * Binary matrix: by applying the inverse of the bijection map between the set of trajectory and the column of the matrix
   */
  @annotation.tailrec
  private final def matrixBinary(m: DenseMatrix[Int], l: Array[Int], i: Int): DenseMatrix[Int] = {
    val n2 = m.cols.toDouble
    if( i < l.length ) {
      if( l(i) % n2 == 0 ) {
        val k = (l(i) / n2).toInt 
        val j = 0  
        m(k, j) = 1
        matrixBinary(m, l, i + 1)
      }
      else {
        val k = floor(l(i) / n2).toInt
        val j = (l(i) - (n2 * k)).toInt
        m(k, j) = 1
        matrixBinary(m, l, i + 1)
      }
    }
    else {
      m
    }
  }

  def run(data: mutable.ArrayBuffer[DenseMatrix[Double]]) = {

    val m = data.length
    val n1 = data.head.rows
    val n2 = data.head.cols

    // Creation of matrix unfolding mode-3
    val tUf = unfolding(data, DenseMatrix.zeros[Double](m, n1 * n2), 0, 0, 0)

    // Take the first eigenvectors correspond of the highest eigenvalues
    val svd.SVD(u, s, v) = svd(tUf)
    val u1 = (v(0, ::).t).map(abs(_))  // convert all element of this vector to positive
  
    val indiceAll = TensorCommons.obtainTopkIndices[Double](u1, k1*k2)
    val matrixIndex =  matrixBinary(DenseMatrix.zeros[Int](n1, n2), indiceAll, 0)

    // Selecting k1 rows and k2 columns of A with the most number of 1's
    val sl = sum(matrixIndex(*, ::)).map(_.toDouble)  //sum for each line
    val indiceRow = TensorCommons.obtainTopkIndices[Double](sl, k1)
    val sc = sum(matrixIndex(::, *)).t.map(_.toDouble)  //sum for each column, sum(A(::,*)) is a line matrix
    val indiceColumn = TensorCommons.obtainTopkIndices[Double](sc, k2)

    new TensorBiclusteringModel(indiceRow.distinct.sorted, indiceColumn.distinct.sorted)
    
    Array(indiceRow, indiceColumn)
  }
}
/**
 *
 */
object UnfoldingSpectral{

  def train(k1: Int, k2: Int, data: mutable.ArrayBuffer[DenseMatrix[Double]]) = (new UnfoldingSpectral(k1, k2)).run(data)

}
