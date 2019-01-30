package org.clustering4ever.scala.clustering.tensor
/**
 * @author ANDRIANTSIORY Dina Faneva, Beck Gaël
 */
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import breeze.linalg.svd.SVD
import breeze.stats._
/**
 * This function return eigenvalues( not eigenvector )
 */
class EigenValue(val k: Int) extends Serializable {

  def run(data: mutable.ArrayBuffer[DenseMatrix[Double]]) = {

    val m = data.length
    /**
     * number of individuals
     */
    val n1 = data.head.rows
    /**
     * number of columns
     */
    val n2 = data.head.cols
    val timeColumn = DenseMatrix.zeros[Double](m,n2)  
    val timeRow = DenseMatrix.zeros[Double](m,n1)

    @annotation.tailrec
    def matriceColumnSet(t:mutable.ArrayBuffer[DenseMatrix[Double]], m:DenseMatrix[Double], c:DenseMatrix[Double], i: Int, j: Int , k: Int): DenseMatrix[Double] = {
      if (j < t.head.cols && k < t.length) {
        m(k, j) = t(k)(i, j)
        matriceColumnSet(t, m, c, i, j, k + 1)
      }
      else if (k == t.length && j < t.head.cols) {
        matriceColumnSet(t, m, c, i, j + 1 , 0)
      }
      else if (i < t.head.rows - 1) {
        c += cov(m)
        matriceColumnSet(t, m, c, i + 1, 0, 0)
      }
      else {
        c += cov(m)
      }
    }

    @annotation.tailrec
    def matriceRowSet(t: mutable.ArrayBuffer[DenseMatrix[Double]], m: DenseMatrix[Double], c: DenseMatrix[Double], i: Int, j: Int , k: Int): DenseMatrix[Double] = {
      if (i < t.head.rows && k < t.length) {
        m(k, i) = t(k)(i, j)
        matriceRowSet(t, m, c, i, j, k + 1)
      }
      else if (k == t.length && i < t.head.rows) {
        matriceRowSet(t, m, c, i + 1, j , 0)
      }
      else if (j < t.head.cols - 1){
        c += cov(m)
        matriceRowSet(t, m, c, 0, j + 1, 0)
      }
      else {
        c += cov(m)
      }
    }

    val columnMatrix = matriceColumnSet(data, timeColumn, DenseMatrix.zeros[Double](n2,n2), 0, 0, 0 )
    val svd.SVD(u1,eigValue,eigVector) = svd(columnMatrix)
    val columnEigvalue = eigValue.toArray   

    val rowMatrix = matriceRowSet(data, timeRow, DenseMatrix.zeros[Double](n1,n1), 0, 0, 0 )
    val svd.SVD(u2,eigValue2,eigVector2) = svd(rowMatrix)
    val rowEigvalue = eigValue2.toArray

    Array(rowEigvalue.take(5), columnEigvalue.take(5))
  
  }
}  
 
object EigenValue extends Serializable {
  
  def train(k: Int, data: mutable.ArrayBuffer[DenseMatrix[Double]]) = (new EigenValue(k)).run(data)

}