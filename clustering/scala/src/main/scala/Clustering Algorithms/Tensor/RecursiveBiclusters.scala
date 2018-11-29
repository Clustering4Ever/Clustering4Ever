 package org.clustering4ever.scala.clustering.tensor
/**
 * @author ANDRIANTSIORY Dina Faneva, Beck Gaël
 */
  import scala.collection.mutable.ListBuffer
  import scala.collection.mutable
  import breeze.linalg.svd.SVD
  import breeze.stats.mean
  import breeze.linalg._
  import scala.math._


class RecursiveBiclusters ( val l1: Array[Int], 
                                    val l2: Array[Int]){

  def run(data: mutable.ArrayBuffer[DenseMatrix[Double]]) = {
    
    def one_bicluster(k1: Int, k2: Int, tensor1: mutable.ArrayBuffer[DenseMatrix[Double]])={
      
      val mt = tensor1.length
      val n1 = tensor1(0).rows  //nomber of individuals
      val n2 = tensor1(0).cols   //number of columns
      val timeColumn = DenseMatrix.zeros[Double](mt,n2) 
      val timeRow = DenseMatrix.zeros[Double](mt,n1)


     @annotation.tailrec
     def matriceColumnSet(t:ListBuffer[DenseMatrix[Double]], m:DenseMatrix[Double], c:DenseMatrix[Double], i: Int, j: Int , k: Int): 
        DenseMatrix[Double] = {
        if (j < t(0).cols && k < t.length){
        m(k,j) = t(k)(i,j);
        matriceColumnSet(t,m, c, i, j, k + 1)
      }
      else if (k == t.length && j < t(0).cols){
        matriceColumnSet(t, m, c, i, j+1 , 0)
      }

      else if (i < t(0).rows - 1){
        c += cov(m)
        matriceColumnSet(t, m, c, i+1, 0, 0)
      }else {
        c += cov(m)
      }
    }

    @annotation.tailrec
    def matriceRowSet(t:ListBuffer[DenseMatrix[Double]], m:DenseMatrix[Double], c:DenseMatrix[Double], i: Int, j: Int , k: Int): 
        DenseMatrix[Double] = {
        if (i < t(0).rows && k < t.length){
        m(k,i) = t(k)(i,j)
        matriceRowSet(t,m, c, i, j, k + 1)
      }
      else if (k == t.length && i < t(0).rows){
        matriceRowSet(t, m, c, i+1, j , 0)
      }

      else if (j < t(0).cols - 1){
        c += cov(m)
        matriceRowSet(t, m, c, 0, j+1, 0)
      }else {
        c += cov(m)
      }
    }

    val columnMatrix = matriceColumnSet(tensor1, timeColumn, DenseMatrix.zeros[Double](n2,n2), 0, 0, 0 )
    val svd.SVD(u1,eigValue,eigVector) = svd(columnMatrix)
   // val column_eigvalue = eigValue.toArray
    val column_eigvector = eigVector.t
         
 
    val rowMatrix = matriceRowSet(tensor1, timeRow, DenseMatrix.zeros[Double](n1,n1), 0, 0, 0 )
    val svd.SVD(u2,eigValue2,eigVector2) = svd(rowMatrix)
  //  val row_eigvalue = eigValue2.toArray
    val row_eigvector = eigVector2.t
   
     def geTTheTopkIndices(m: DenseMatrix[Double], k: Int):
    Array[Int] = {
      
         m(::,0).map(x => abs(x)).toArray.zipWithIndex.sortWith((x, y) => x._1 > y._1).take(k).map(x => x._2) 
  
      }
    
      val row = geTTheTopkIndices(row_eigvector, k1)

      val column = geTTheTopkIndices(column_eigvector, k2)
      
     println("\n\n bicluster number "+": \n rows "+ row.toList + "\n column "+ column.toList )
  
    def annulation(t:mutable.ArrayBuffer[DenseMatrix[Double]],jf: Array[Int], js: Array[Int], m: Int): 
    mutable.ArrayBuffer[DenseMatrix[Double]] = { 
  
      for (k <- 0 until m) {
          for (i <- jf){
            for (j <- js){
                  t(k)(i, j) = 0 
              }
          }
      }
      t
    } 
      var tensor_remain = annulation(tensor1, row, column, mt)
      
      (tensor_remain, Array(row, column) )
    }
    
    var r1 =ListBuffer[mutable.ArrayBuffer[DenseMatrix[Double]]]()
    r1 += tens
    var result = ListBuffer[Array[Array[Int]]]()
    
    for (nombre <- 0 until l1.length) {
      val (t1, re1) = one_bicluster(l1(nombre), l2(nombre), r1(nombre))
      r1 += t1
      result += re1
      }
    result
  }
}  

/**
 *
 */
object RecursiveBiclusters {
  
  def train(k1: Array[Int], k2: Array[Int], data: mutable.ArrayBuffer[DenseMatrix[Double]]) = (new RecursiveBiclusters(k1, k2)).run(data)

}