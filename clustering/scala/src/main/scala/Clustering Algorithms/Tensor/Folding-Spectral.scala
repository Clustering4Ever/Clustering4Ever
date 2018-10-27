package clustering4ever.scala.clustering.tensor
/**
 * @author ANDRIANTSIORY Dina Faneva, Beck Gaël
 */
import scala.collection.mutable
import breeze.linalg.svd.SVD
import breeze.stats.mean
import breeze.linalg._
import scala.math._
import clustering4ever.clustering.ClusteringAlgorithms
/**
 * Suppose that we have the matrix T_1 and T_2 from a higher tensor of dimension n1xn2xn3
 */
class TensorFoldSpectral(val k1: Int, val k2: Int, val tensor: mutable.ListBuffer[DenseMatrix[Double]]) extends ClusteringAlgorithms {

  def run() = {

    val m = tensor.length
    val n1 = tensor.head.rows
    val n2 = tensor.head.cols
    val timeColumn = DenseMatrix.zeros[Double](m, n2)  
    val timeRow = DenseMatrix.zeros[Double](m, n1)

    @annotation.tailrec
    def matrice1(t: mutable.ListBuffer[DenseMatrix[Double]], m: DenseMatrix[Double], c: DenseMatrix[Double], i: Int, j: Int, k: Int): DenseMatrix[Double] = {
      if (j < t.head.cols && k < t.length) {
        m(k,j) = t(k)(i,j)
        matrice1(t, m, c, i, j, k + 1)
      }
      else if (k == t.length && j < t(0).cols) {
        matrice1(t, m, c, i, j + 1 , 0)
      }
      else if (i < t(0).rows - 1){
        c += m.t * m
        matrice1(t, m, c, i + 1, 0, 0)
      }
      else {
        c += (m.t * m)
        c
      }
    }

    @annotation.tailrec
    def matrice2(t: mutable.ListBuffer[DenseMatrix[Double]], m: DenseMatrix[Double], c: DenseMatrix[Double], i: Int, j: Int , k: Int): DenseMatrix[Double] = {
      if (i < t.head.rows && k < t.length){
        m(k,i) = t(k)(i,j)
        matrice2(t,m, c, i, j, k + 1)
      }
      else if (k == t.length && i < t(0).rows){
        matrice2(t, m, c, i + 1, j , 0)
      }
      else if (j < t(0).cols - 1){
        c += m.t * m
        matrice2(t, m, c, 0, j + 1, 0)
      }
      else {
        c += (m.t * m)
        c
      }
    }

    val columnMatrix = matrice1(tensor, timeColumn, DenseMatrix.zeros[Double](n2, n2), 0, 0, 0 )
    val rowMatrix = matrice2(tensor, timeRow, DenseMatrix.zeros[Double](n1, n1), 0, 0, 0 )

     def topEigenVector(matrix: DenseMatrix[Double]) = {
      val svd.SVD(u, eigValue, eigVector) = svd(matrix) // X is a symetric matrix
      val u1 = (eigVector(0, ::).t).map(abs(_))   // top components eigenvector
      (u1, eigValue)
    }

    // the top eigenvector of matrix C2  
    val (eigenvectorRow, eigValueRow) = topEigenVector(rowMatrix)

    //the top eigenvector of matrix C1
    val (eigenvectorColumn, eigValueCol) = topEigenVector(columnMatrix)

    //set of indices of rows belonging to the first cluster
    val indiceRow = TensorCommons.obtainTopkIndices[Double](eigenvectorRow, k1) 

    //set of indices of columns belonging to the first cluster
    val indiceColumn = TensorCommons.obtainTopkIndices[Double](eigenvectorColumn, k2)

    new TensorBiclusteringModel(indiceRow.distinct.sorted, indiceColumn.distinct.sorted)
  }
}
/**
 *
 */
object TensorFoldSpectral {
  
  def train(k1: Int, k2: Int, tensor: mutable.ListBuffer[DenseMatrix[Double]]) = (new TensorFoldSpectral(k1, k2, tensor)).run()

}
 