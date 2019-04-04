package Preprocessing
import java.util.Calendar

import breeze.linalg._
import breeze.linalg.svd.SVD
import breeze.linalg.PCA

case class PCAModel(final val dm: DenseMatrix[Double]){
  def project(point: DenseVector[Double],component: Int ): DenseVector[Double] ={
    val eigenvec = dm.delete( (component + 1) until dm.cols , Axis._1)

    eigenvec * point
  }
  def project(matrix: DenseMatrix[Double],component: Int): DenseMatrix[Double] ={
    val eigenvec = dm.delete( (component + 1) until dm.cols , Axis._1)
    matrix * eigenvec
  }
  def project(data: Array[Array[Double]],component: Int): DenseMatrix[Double] ={
    val dm = DenseMatrix(data:_*)
    project(dm,component)

  }

}
object PCA extends App{

   def fit(data: Array[Array[Double]], component: Int = 0) = {


    val dm = DenseMatrix(data:_*)

     val dim = if(component == 0) dm.cols  else component


    val d = zeroMean(dm)
    val SVD(_, _, v) = svd(d.t)
    val eigenv = v(0 until dim, ::)
    val filter = eigenv.t * eigenv
    PCAModel(filter * d)
  }

  private def mean(v: Vector[Double]) = (v.valuesIterator.sum) / v.size

  private def zeroMean(m: DenseMatrix[Double]) = {
    val copy = m.copy
    for (c <- 0 until m.cols) {
      val col = copy(::, c)
      val colMean = mean(col)
      //col -= colMean
      copy(::, c) -= colMean
    }
    copy
  }

  val bufferedSource = scala.io.Source.fromFile("C:\\Users\\ATTAOUI\\Documents\\SS_GStream\\resources\\waveform.txt")
  val lines = (for (line <- bufferedSource.getLines()) yield line).toSeq
  val data = lines.map(x => x.split(',').map(_.toDouble)).toArray
  val pca = fit(data).project(data,2)
  println(pca)

}