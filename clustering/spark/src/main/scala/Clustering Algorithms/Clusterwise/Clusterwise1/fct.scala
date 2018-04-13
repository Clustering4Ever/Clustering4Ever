package clustering4ever.spark.clustering.clusterwise

import math.{pow,sqrt}
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import breeze.linalg._
import breeze.stats.mean
import scala.util.Sorting.quickSort
import org.apache.spark.rdd.RDD
import scala.util.Random
import org.apache.spark.SparkContext

object MyFct extends Serializable
{
  def dudiY(dsY:Array[Array[Double]], n0:Int) =
  {
    val colw = dsY.head.size
    val dsY0 = dsY.map(_.head)
    val roww = dsY0.map( x => 1D / n0 )
    val ds = dsY0.zip(roww).map( x => x._1 * sqrt(x._2) )
    val eigValue = ds.map(pow(_,2)).sum
    eigValue
  }

  def dudi(dataY:Array[Array[Double]]) =
  {  
    val lw = 1D / dataY.size
    val slw = breeze.numerics.sqrt(lw)
    val datay2 = dataY.map(_.map(_ * slw))
    val bm = DenseMatrix(datay2:_*)
    val df = bm.t * bm
    val d = eigSym(df)
    d.eigenvalues
  }

  def getW(xK: ArrayBuffer[Array[Double]], lX: ArrayBuffer[Double], nbRows: Int, nbColsX: Int) =
  {
    val crossprod1dataX = xK.map( x =>
    {   val arr = x
        (for(i<-x.indices) yield(arr.map(y=>y*x(i)))).toArray
    })
    .reduce( _.zip(_).map( x => x._1.zip(x._2).map( y => y._1 + y._2) ) )

    val prepareMatrix = crossprod1dataX.reduce(_++_)
    val denseMat = new DenseMatrix(nbColsX,nbColsX,prepareMatrix)
    val svd1 = svd(denseMat)
    val vals = svd1.S
    val valvt = svd1.Vt
    val valu = svd1.U
    val vtmat = new DenseMatrix(nbColsX,nbColsX,valvt.toArray)
    val umat = new DenseMatrix(nbColsX,nbColsX,valu.toArray)
    val vtmatt = vtmat.t
    val valsinv = vals.map(1D / _)
    val tol = 1.490116e-08
    val maxval = tol * vals(0)
    val valsPos = vals.map( x => if( x > maxval ) true else false ).toArray
    var ginv0 = if ( valsPos.filter(_ == true).size == 0 )
    {
      DenseMatrix.zeros[Double](nbColsX,nbRows)
    }
    else
    {
      val valsPosId = valsPos.zipWithIndex.filter(_._1 == false).map(_._2)
      val rez003 = valsPos.filter(_ == false)
      // Delete columns of valsPOsId
      val vtmatt2 = vtmatt.delete(valsPosId,breeze.linalg.Axis._1)
      val umat2 = umat.delete(valsPosId,breeze.linalg.Axis._1)
      val valsinv2 = new DenseVector(valsinv.toArray.zipWithIndex.filter( x => ! valsPosId.contains(x._2) ).map(_._1))
      val umatt2 = umat2.t
      val buff2 = ArrayBuffer.empty[Double]
      for( i <- 0 until umatt2.cols )
      {
        buff2 ++= (valsinv2 :* umatt2(::, i)).toArray
      }
      val middlemat2 = new DenseMatrix(umatt2.rows, umatt2.cols, buff2.toArray)
      vtmatt2 * middlemat2
    }

    val nbrows = ginv0.rows
    val arreyGinv = ginv0.toArray
    val groupedCols = arreyGinv.grouped(nbrows).toArray
    val bcMatrix3 = groupedCols.map(DenseVector(_))
    val size01 = ginv0.cols
    val dataXginvCp3 = xK.map( x => for( i <- 0 until size01 ) yield (DenseVector(x) dot bcMatrix3(i)) )
    val w = lX.zip(dataXginvCp3).map( x => x._2.map(_ * x._1) ).reduce( _.zip(_).map( x => x._1 + x._2) )
    w
  }

  def prepareMovingGroup(classedDS: Array[ArrayBuffer[(Int, (Array[Double], Array[Double], Int))]], classMovingGroup: Int, idsPoints: Array[Int], g: Int) =
  {
    val bufferDSperClass = ArrayBuffer.empty[Array[(Int, (Array[Double], Array[Double], Int))]]
    val elemsToReplace = classedDS(classMovingGroup).filter( x => idsPoints.contains(x._1) )
    for( j <- 0 until g)
    {
      if( j == classMovingGroup )
      {
        val classWithoutElem = classedDS(j).toBuffer -- elemsToReplace
        bufferDSperClass += classWithoutElem.toArray
      }
      else
      {
        bufferDSperClass += (classedDS(j).toBuffer ++ elemsToReplace).toArray
      }
    }
    bufferDSperClass.toArray
  }

  def pearson(a: Vector[Double], b: Vector[Double]): Double =
  {
      val n = a.length
      val dot = a.dot(b)
      val adot = a.dot(a)
      val bdot = b.dot(b)
      val amean = mean(a)
      val bmean = mean(b)
      val denum = ( sqrt(adot - n * amean * amean)  * sqrt(bdot - n * bmean * bmean) )
      val num = (dot - n * amean * bmean )
      val coef = if(denum == 0) 0D else num / denum
      coef
  }


}