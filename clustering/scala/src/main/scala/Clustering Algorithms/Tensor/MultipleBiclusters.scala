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

  def run(data: mutable.ArrayBuffer[DenseMatrix[Double]]): TensorBiclusteringModel = {

    val m = data.size
    val n1 = data.head.rows
    val n2 = data.head.cols
    val timeColumn = DenseMatrix.zeros[Double](m, n2)  
    val timeRow = DenseMatrix.zeros[Double](m, n1)
    var columnMatrix = DenseMatrix.zeros[Double](n2,n2)
    var rowMatrix = DenseMatrix.zeros[Double](n1,n1)

     @annotation.tailrec
     def matrice1(t: mutable.ArrayBuffer[DenseMatrix[Double]], m: DenseMatrix[Double], c: DenseMatrix[Double], i: Int, j: Int , k: Int): 
        DenseMatrix[Double] = {
        if(j < t.head.cols && k < t.length) {
        m(k, j) = t(k)(i,j)
        matrice1(t,m, c, i, j, k + 1)
      }
      else if(k == t.length && j < t.head.cols) {
        matrice1(t, m, c, i, j+1 , 0)
      }
      else if(i < t.head.rows - 1) {
        c += cov(m)
        matrice1(t, m, c, i+1, 0, 0)
      }
      else {
        c += cov(m)
      }
    }


    @annotation.tailrec
    def matrice2(t:mutable.ArrayBuffer[DenseMatrix[Double]], m:DenseMatrix[Double], c:DenseMatrix[Double], i: Int, j: Int , k: Int): 
        DenseMatrix[Double] = {
        if (i < t(0).rows && k < t.length){
        m(k,i) = t(k)(i,j)
        matrice2(t,m, c, i, j, k + 1)
      }
      else if (k == t.length && i < t(0).rows){
        matrice2(t, m, c, i+1, j , 0)
      }

      else if (j < t(0).cols - 1){
        c += cov(m)
        matrice2(t, m, c, 0, j+1, 0)
      }else {
        c += cov(m)
      }
    }

    columnMatrix = matrice1(data, timeColumn, columnMatrix, 0, 0, 0 )
    val svd.SVD(u1, eigValue, eigVector) = svd(columnMatrix)
    val columnEigvalue = eigValue.toArray
    val columnEigvector = eigVector.t
         
    rowMatrix = matrice2(data, timeRow, rowMatrix, 0, 0, 0 )
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
    def intersection(intRow: Array[Int], a: ListBuffer[Array[Int]], intColumn: Array[Int], b: ListBuffer[Array[Int]], i: Int):  Array[Array[Int]] ={
      if (i < a.length){
       val intRow2 = intRow.intersect(a(i))      // Intersection is a decreasing function
       val intCol = intColumn.intersect(b(i))
        println("\n\n Bicluster number "+ i +": \n individuals "+ row(i).toSet + "\n features "+ column(i).toSet )
        intersection(intRow2, a, intCol, b, i+1)
      }
      else {
       Array(intRow, intColumn) 
      }
    }
          
    val Array(rowIndexes, columnIndexes) = intersection(row.head, row, column.head, column, 0)
    // println("\n Individuals intersection  is :"+ intersections(0).toSet + " \n length = "+ intersections(0).length + "\n Features  intersection :" + intersections(1).toSet +" \n length = "+ intersections(1).length )
   
    //The top five of eigenvalues 
    // println("\n Top five eigenvalues of the matrix C1: "+ rowEigvalue.take(5).toList + "\n Top five Eigenvalues of the matrix C2: "+ columnEigvalue.take(5).toList)
    
    new TensorBiclusteringModel(rowIndexes, columnIndexes)

  }
}  
 
object MultipleBiclusters {
  
  def train(k1:Array[Int], k2:Array[Int], data: mutable.ArrayBuffer[DenseMatrix[Double]]): TensorBiclusteringModel = (new MultipleBiclusters(k1, k2)).run(data)

}