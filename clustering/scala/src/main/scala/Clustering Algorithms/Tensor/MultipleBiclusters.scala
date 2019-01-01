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
import org.clustering4ever.clustering.ClusteringAlgorithm
/**
 *
 */
class MultipleBiclusters(val k1: Array[Int], val k2: Array[Int]) extends ClusteringAlgorithm {

  def run(data: mutable.ArrayBuffer[DenseMatrix[Double]])/*: Unit*/ = {

    val m = data.size
    val n1 = data.head.rows
    val n2 = data.head.cols
    val timeColumn = DenseMatrix.zeros[Double](m, n2)  
    val timeRow = DenseMatrix.zeros[Double](m, n1)
    var columnMatrix = DenseMatrix.zeros[Double](n2,n2)
    var rowMatrix = DenseMatrix.zeros[Double](n1,n1)

     @annotation.tailrec
     def matriceColumnSet(t: mutable.ArrayBuffer[DenseMatrix[Double]], m: DenseMatrix[Double], c: DenseMatrix[Double], i: Int, j: Int , k: Int): 
        DenseMatrix[Double] = {
        if(j < t.head.cols && k < t.length) {
        m(k, j) = t(k)(i,j)
        matriceColumnSet(t,m, c, i, j, k + 1)
      }
      else if(k == t.length && j < t.head.cols) {
        matriceColumnSet(t, m, c, i, j+1 , 0)
      }
      else if(i < t.head.rows - 1) {
        c += cov(m)
        matriceColumnSet(t, m, c, i+1, 0, 0)
      }
      else {
        c += cov(m)
      }
    }

    @annotation.tailrec
    def matriceRowSet(t:mutable.ArrayBuffer[DenseMatrix[Double]], m:DenseMatrix[Double], c:DenseMatrix[Double], i: Int, j: Int , k: Int): DenseMatrix[Double] = {
      if (i < t.head.rows && k < t.length){
        m(k,i) = t(k)(i,j)
        matriceRowSet(t,m, c, i, j, k + 1)
      }
      else if (k == t.length && i < t(0).rows){
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

    
    columnMatrix = matriceColumnSet(data, timeColumn, columnMatrix, 0, 0, 0 )
    val svd.SVD(u1, eigValue, eigVector) = svd(columnMatrix)
    val columnEigvalue = eigValue.toArray
    val columnEigvector = eigVector.t
         
    rowMatrix = matriceRowSet(data, timeRow, rowMatrix, 0, 0, 0 )
    val svd.SVD(u2,eigValue2,eigVector2) = svd(rowMatrix)
    val rowEigvalue = eigValue2.toArray
    val rowEigvector = eigVector2.t

    @annotation.tailrec
    def obtainTopkIndicesDM(m: DenseMatrix[Double], k: Array[Int], v: mutable.ListBuffer[Array[Int]], i: Int): mutable.ListBuffer[Array[Int]] = {  
      if (i < k.length){
        val g = k(i)
        v += m(::,i).map(math.abs(_)).toArray.zipWithIndex.sortWith((x, y) => x._1 > y._1).take(g).map(_._2)          
        obtainTopkIndicesDM(m, k, v, i+1)
      }
      else {
         v
      }
    }

    val uu = ListBuffer[Array[Int]]()
    val row = obtainTopkIndicesDM(rowEigvector, k1, uu, 0)


    val ww = ListBuffer[Array[Int]]()
    val column = obtainTopkIndicesDM(columnEigvector, k2, ww, 0)

    @annotation.tailrec
     def intersection(int_row:Array[Int], a: ListBuffer[Array[Int]], int_column:Array[Int], b: ListBuffer[Array[Int]], i: Int):
        (Array[Int],Array[Int]) ={
          if (i < a.length){
           val intRow = int_row.intersect(a(i))      // Intersection is a decreasing function
           val intCol = int_column.intersect(b(i))
           intersection(intRow, a, intCol, b, i+1)
          }
          else {
           (int_row, int_column)
          }
        }
          
   // val Array(rowIntersection, columnIntersection) = intersection(row.head, row, column.head, column, 0)
   val rowColsIntersection = intersection(row.head, row, column.head, column, 0)
   
    @annotation.tailrec
    def result(r: mutable.ListBuffer[Array[Int]], c: mutable.ListBuffer[Array[Int]], rc:mutable.ListBuffer[Array[Array[Int]]], i:Int, l: Int ): mutable.ListBuffer[Array[Array[Int]]] = {
      if (i < l){
        rc += Array(r(i), c(i)) 
        result(r, c, rc, i+1, l)
      } 
      else {
        rc
      }
    }
    val fin = mutable.ListBuffer[Array[Array[Int]]]()

    val resultats = result(row, column, fin, 0, k1.length)

    //new TensorBiclusteringModel(rowIndexes, columnIndexes)

   // (resultats, Array(rowIntersection, columnIntersection) ) 
    (resultats, rowColsIntersection) 
    //Unit
  }
}  
 
object MultipleBiclusters {
  
  def train(k1:Array[Int], k2:Array[Int], data: mutable.ArrayBuffer[DenseMatrix[Double]])/*:
   TensorBiclusteringModel */= (new MultipleBiclusters(k1, k2)).run(data)

}