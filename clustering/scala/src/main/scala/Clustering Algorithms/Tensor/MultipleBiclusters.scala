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
class MultipleBiclusters(val k1: Array[Int], val k2: Array[Int]
					) extends ClusteringAlgorithm {

 def run(data: mutable.ArrayBuffer[DenseMatrix[Double]]) = {


      val m = data.length
      val n1 = data.head.rows  //nomber of individuals
      val n2 = data.head.cols   //number of columns
      val timeColumn = DenseMatrix.zeros[Double](m,n2)  
      val timeRow = DenseMatrix.zeros[Double](m,n1)


     @annotation.tailrec
     def matriceColumnSet(t:mutable.ArrayBuffer[DenseMatrix[Double]], m:DenseMatrix[Double], c:DenseMatrix[Double], i: Int, j: Int , k: Int): 
        DenseMatrix[Double] = {
        if (j < t.head.cols && k < t.length){
        m(k,j) = t(k)(i,j);
        matriceColumnSet(t,m, c, i, j, k + 1);
      }
      else if (k == t.length && j < t.head.cols){
        matriceColumnSet(t, m, c, i, j+1 , 0);
      }

      else if (i < t.head.rows - 1){
        c += cov(m)
        matriceColumnSet(t, m, c, i+1, 0, 0);
      }else {
        c += cov(m)
      }
    }

    @annotation.tailrec
    def matriceRowSet(t:mutable.ArrayBuffer[DenseMatrix[Double]], m:DenseMatrix[Double], c:DenseMatrix[Double], i: Int, j: Int , k: Int): 
        DenseMatrix[Double] = {
        if (i < t.head.rows && k < t.length){
        m(k,i) = t(k)(i,j);
        matriceRowSet(t,m, c, i, j, k + 1);
      }
      else if (k == t.length && i < t.head.rows){
        matriceRowSet(t, m, c, i+1, j , 0);
      }

      else if (j < t(0).cols - 1){
        c += cov(m)
        matriceRowSet(t, m, c, 0, j+1, 0);
      }else {
        c += cov(m)
      }
    }


    val columnMatrix = matriceColumnSet(tensor, timeColumn, DenseMatrix.zeros[Double](n2,n2), 0, 0, 0 )
    val svd.SVD(u1,eigValue,eigVector) = svd(columnMatrix)
   // val column_eigvalue = eigValue.toArray
    val column_eigvector = eigVector.t
         
 

    val rowMatrix = matriceRowSet(tensor, timeRow, DenseMatrix.zeros[Double](n1,n1), 0, 0, 0 )
    val svd.SVD(u2,eigValue2,eigVector2) = svd(rowMatrix)
   // val row_eigvalue = eigValue2.toArray
    val row_eigvector = eigVector2.t
       @annotation.tailrec
     def geTTheTopkIndices(m: DenseMatrix[Double], k: Array[Int], v: ListBuffer[Array[Int]],  i: Int): ListBuffer[Array[Int]] = {
      
        if (i < k.length){
          val g = k(i)
          v += m(::,i).map(x => abs(x)).toArray.zipWithIndex.sortWith((x, y) => x._1 > y._1).take(g).map(x => x._2) 
                   
          geTTheTopkIndices(m, k, v, i+1)
        }
        else {
          v
        }
      }
      
      var uu = ListBuffer[Array[Int]]()
      val row = geTTheTopkIndices(row_eigvector, k1, uu, 0)


      var ww = ListBuffer[Array[Int]]()
      val column = geTTheTopkIndices(column_eigvector, k2, ww, 0)

    
    def intersection(int_row:Array[Int], a: ListBuffer[Array[Int]], int_column:Array[Int], b: ListBuffer[Array[Int]], i: Int):
    Array[Array[Int]] ={
      if (i < a.length){
       val intRow = int_row.intersect(a(i))      // Intersection is a decreasing function
       val intCol = int_column.intersect(b(i))
       // println("\n\n Bicluster number "+ i +": \n individuals "+ row(i).toSet + "\n features "+ column(i).toSet )
       intersection(intRow, a, intCol, b, i+1)
      }
      else {
       Array(int_row, int_column) 
      }
    }
          
    val intersections = intersection(row(0), row, column(0), column, 0)
    //println("\n Individuals intersection  is :"+ intersections(0).toSet + " \n length = "+ intersections(0).length+"\n Features  intersection :" + intersections(1).toSet +" \n length = "+ intersections(1).length )
   
    //The top five of eigenvalues 
    //println("\n Top five eigenvalues of the matrix C1: "+ row_eigvalue.take(5).toList+"\n Top five Eigenvalues of the matrix C2: "+ column_eigvalue.take(5).toList)
    def result(r: ListBuffer[Array[Int]], c: ListBuffer[Array[Int]], rc:ListBuffer[Array[Array[Int]]], i:Int, l: Int ): ListBuffer[Array[Array[Int]]] = {
      if (i < l){
        rc += Array(r(i), c(i)) 
        result(r, c, rc, i+1, l)
      } 
      else {
        rc
      }
    }
    val fin = ListBuffer[Array[Array[Int]]]()  
    val resultats = result(row, column, fin, 0, k1.length)
    (resultats, intersections)
  
  }
}  
 
object MultipleBiclusters {
  
  def train(k1:Array[Int], k2:Array[Int], data: mutable.ArrayBuffer[DenseMatrix[Double]]) = (new MultipleBiclusters(k1, k2)).run(data)

}