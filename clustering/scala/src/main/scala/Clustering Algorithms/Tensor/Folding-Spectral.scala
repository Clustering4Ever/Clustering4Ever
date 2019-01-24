package org.clustering4ever.scala.clustering.tensor
/**
 * @author ANDRIANTSIORY Dina Faneva, Beck Gaël
 */
import scala.collection.mutable
import breeze.linalg.svd.SVD
import breeze.stats.mean
import breeze.linalg._
import scala.math._
import org.clustering4ever.clustering.ClusteringAlgorithmGeneric
/**
 * Suppose that we have the matrix T_1 and T_2 from a higher data of dimension n1xn2xn3
 */
class TensorFoldSpectral(val k1: Int, val k2: Int) extends ClusteringAlgorithmGeneric {

  def run(data: mutable.ArrayBuffer[DenseMatrix[Double]]) = {

    val m = data.length
    val n1 = data.head.rows
    val n2 = data.head.cols
    val timeColumn = DenseMatrix.zeros[Double](m, n2)  
    val timeRow = DenseMatrix.zeros[Double](m, n1)

     // function take all the lateral slice T(i,:,:)
     @annotation.tailrec
     def matriceColumnset(t: mutable.ArrayBuffer[DenseMatrix[Double]], m: DenseMatrix[Double], c: DenseMatrix[Double], i: Int, j: Int , k: Int): DenseMatrix[Double] = {
      if(j < t.head.cols && k < t.length) {
        m(k, j) = t(k)(i, j)
        matriceColumnset(t, m, c, i, j, k + 1)
      }
      else if(k == t.length && j < t.head.cols) {
        matriceColumnset(t, m, c, i, j + 1, 0)
      }
      else if(i < t.head.rows - 1) {
        c += cov(m)   // the covariance matrix of m
        matriceColumnset(t, m, c, i + 1, 0, 0)
      }
      else {
        c += cov(m)
      }
    }
    

    // function take all the horizontal slice T(:,j,:)
    @annotation.tailrec
    def matriceRowset(t: mutable.ArrayBuffer[DenseMatrix[Double]], m: DenseMatrix[Double], c: DenseMatrix[Double], i: Int, j: Int , k: Int): DenseMatrix[Double] = {
      if(i < t.head.rows && k < t.length) {
        m(k,i) = t(k)(i, j)
        matriceRowset(t,m, c, i, j, k + 1)
      }
      else if(k == t.length && i < t.head.rows) {
        matriceRowset(t, m, c, i + 1, j, 0)
      }
      else if( j < t.head.cols - 1 ) {
        c += cov(m)
        matriceRowset(t, m, c, 0, j + 1, 0)
      }
      else {
        c += cov(m)
      }
    }

    val columnMatrix = matriceColumnset(data, timeColumn, DenseMatrix.zeros[Double](n2, n2), 0, 0, 0 )
    val rowMatrix = matriceRowset(data, timeRow, DenseMatrix.zeros[Double](n1, n1), 0, 0, 0 )

    //extraction of the top eigenvector and the eigenvalues 
     def topEigenVector(matrix: DenseMatrix[Double]) = {
      val svd.SVD(u, eigValue, eigVector) = svd(matrix) // X is a symetric matrix
      val u1 = (eigVector(0, ::).t).map(abs(_))   // top components eigenvector
      (u1, eigValue)
    }

    // the top eigenvector of matrix_rowset  
    val (eigenvectorRow, eigValueRow) = topEigenVector(rowMatrix)

    //the top eigenvector of matrix_columnset
    val (eigenvectorColumn, eigValueCol) = topEigenVector(columnMatrix)

    //set of indices of rows belonging to the first cluster
    val indiceRow = TensorCommons.obtainTopkIndices[Double](eigenvectorRow, k1) 

    //set of indices of columns belonging to the first cluster
    val indiceColumn = TensorCommons.obtainTopkIndices[Double](eigenvectorColumn, k2)

    new TensorBiclusteringModel(indiceRow.distinct.sorted, indiceColumn.distinct.sorted)
    
    Array(indiceRow, indiceColumn)
  }
}
/**
 *
 */
object TensorFoldSpectral {
  
  def train(k1: Int, k2: Int, data: mutable.ArrayBuffer[DenseMatrix[Double]]) = (new TensorFoldSpectral(k1, k2)).run(data)

}
 
