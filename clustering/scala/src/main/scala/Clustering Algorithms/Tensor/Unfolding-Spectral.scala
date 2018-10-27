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

class UnfoldingSpectral(val k1: Int, val k2: Int, val tensor: mutable.ListBuffer[DenseMatrix[Double]]) extends ClusteringAlgorithms {

  //  Matricisation of tensor / Unfolding mode-3
  @annotation.tailrec
  private final def unfolding(tensor: mutable.ListBuffer[DenseMatrix[Double]], m: DenseMatrix[Double], i: Int, j: Int, k: Int): DenseMatrix[Double] = {
    val n2 = tensor.head.cols
      if( j < tensor.head.cols && k < tensor.length ) {
        m(k, (i * n2) + j) = tensor(k)(i, j)
        unfolding(tensor, m, i, j, k + 1)
      }
      else if( k == tensor.length && j < tensor.head.cols ) {
        unfolding(tensor, m, i, j + 1 , 0)
      }
      else if( i < tensor(0).rows - 1 ) {
        unfolding(tensor, m, i + 1, 0, 0)
      }
      else {
        m
      }
  }

  //Binary matrix
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

  def run() = {

    val m = tensor.length
    val n1 = tensor.head.rows
    val n2 = tensor.head.cols

    /*Creation of matrix unfolding mode-3*/
    val tUf = unfolding(tensor, DenseMatrix.zeros[Double](m, n1 * n2), 0, 0, 0)

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
    
  }
}
/**
 *
 */
object UnfoldingSpectral{

  def train(k1: Int, k2: Int, tensor: mutable.ListBuffer[DenseMatrix[Double]]) = (new UnfoldingSpectral(k1, k2, tensor)).run()

}
