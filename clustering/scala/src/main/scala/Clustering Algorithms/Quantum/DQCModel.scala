package Quantum
import breeze.numerics.{exp, sqrt}
import org.apache.spark.rdd.RDD
import breeze.linalg.{Axis, DenseMatrix, DenseVector, Vector, min, squaredDistance, sum}
import org.apache.spark.{SparkConf, SparkContext}
import smile.plot.Palette

import scala.collection.mutable.ArrayBuffer
import scala.math.abs
class DQCModel (
  var W: ArrayBuffer[Double],
  var delta: ArrayBuffer[Double],
  var V: ArrayBuffer[Double]
  ) extends Serializable {


    def this() = this(
      W = ArrayBuffer(),
      delta = ArrayBuffer(),
      V = ArrayBuffer()
    )
  def pointToObjet(e: Array[Double], dim: Int, labId: Int) = {
    val dataPart = e.take(e.size - labId) //labId=-2 because the 2 last columns represent labels & id
    val part1 = Vector(dataPart.take(dim))
    val etiq = e.drop(dim).map(_.toInt)
    new pointObj(part1, etiq(0), etiq(1))
  }
def calculSim(data: Array[Array[Double]]):Array[Array[Double]]={

  val n = data.length
  var sim = Array.ofDim[Double](n,n)
  for( i <- 0 until n){
  sim(i) = data.map(x => euclideanDistance(data(i),x)).sorted
  }
  sim
}
  def normalize(rdd: Array[Array[Double]],dim:Int): Array[Array[Double]] ={
    val n = rdd.length.toInt
    val sum = Array.fill[Double](dim)(0)

    for(i <- 0 until dim){
      for(j <- 0 until n){
        sum(i) = rdd(j)(i) * rdd(j)(i)
    }
      sum(i) /= n
      println(sum(i))
  }

    for(i <- 0 until n) {
      for (j <- 0 until dim) {
        rdd(i)(j) /= sqrt(sum(j))
      }

    }

    rdd
  }
  def Parzen(data: Array[Array[Double]],D: Array[Array[Double]],sigma:Double, dim: Int) ={

    val n = data.length
    var W1 =  collection.mutable.ArrayBuffer.fill(n)(0.0)
    var delta =  collection.mutable.ArrayBuffer.fill(n)(0.0)





    for (i <- 0 until n) {
      var point = D(i)
      for (j <- 0 until n) {
        var dist = euclideanDistance(point,data(j))
        W1(i) +=  exp(-dist/(2*sigma*sigma))
        delta(i) += dist * W1(i)
      }

    }
    this.W=W1
    this.delta=delta
    this
  }
def repRow(point:DenseVector[Double],reps:Int,dim:Int): DenseMatrix[Double] ={
  var w = DenseMatrix.zeros[Double](reps,dim)
  for(i<-0 until reps){
    for(j<-0 until dim){
      w(i,j) = point(j)
    }
  }
  w
}
  def wave(data: Array[Array[Double]],D: Array[Array[Double]],sigma:Double, dim: Int) ={
    //this.Parzen(data,D,sigma,dim)
    val n = data.length
    val q= 1/(2*sigma*sigma)
    val P = DenseVector.zeros[Double](n)
    var dP = DenseVector.zeros[Double](n)
    var v = DenseVector.zeros[Double](n)

    var dV1 = DenseMatrix.zeros[Double](n,dim)
    var dV2 = DenseMatrix.zeros[Double](n,dim)
    var dV3 = DenseMatrix.zeros[Double](n,dim)

    for(i <-0 until n){
      val point = data(i)
      val results = onePointWave(point,data,D,sigma,dim)

      P(i) = results._1
      dP(i) = results._2
      v(i) = -dim / 2 + q * dP(i)/P(i)
      for(j <- 0 until dim ){
        dV1(i,j) = results._3(j)
        dV2(i,j) = results._4(j)
      }
    }

    val E = min(v)
    v -= E
    for(i <- 0 until n) {
      for (j <- 0 until dim) {

        dV3(i, j) = -q * dV1(i, j) + (v(i) - E + (dim + 2) / 2) * dV2(i,j)
      }
    }

    dV3
  }
  def onePointWave(point :Array[Double],rdd: Array[Array[Double]],D: Array[Array[Double]],sigma:Double, dim: Int) ={

    val n = rdd.length
    val q= 1/(2*sigma*sigma)

    val data = DenseMatrix(D:_*)
    var dV1 = DenseVector.zeros[Double](dim)
    var dV2 = DenseVector.zeros[Double](dim)
    var single_laplace = DenseVector.zeros[Double](n)
    var squaredDis = DenseMatrix(Seq.fill(n)(point): _*) //repRow(DenseVector(point),n,dim)

    var unsquaredDis =  squaredDis - data
    squaredDis = unsquaredDis *:* unsquaredDis
    var D2 = sum(squaredDis, Axis._1)
    val single_point = exp(-q*D2)
    for(i <-0 until dim) {
      single_laplace = single_laplace + squaredDis(::, i) * single_point
    }

    for(i <-0 until dim){
      var curr_col = unsquaredDis(::, i)
      dV1(i) = dV1(i) + sum(curr_col * single_laplace)
      dV2(i) = dV2(i) + sum(curr_col * single_point)
    }
    var P = sum(single_point)
    var dP2 = sum(single_laplace)

    (P, dP2, dV1, dV2)

  }

  // euclideanDistance
  def euclideanDistance(a: Array[Double], b: Array[Double]): Double = {
    var sum = 0.0
    if(Option(b).isDefined) {
      val values = (a zip b).map { case (x, y) => x - y }
      val size = values.size

      var i = 0
      while (i < size) {
        sum += values(i) * values(i)
        i += 1
      }
    }
    sum
  }
  def gradientDescent(precision: Double, previousStepSize: Double, curX: Double,gamma:Double): Double = {
    if (previousStepSize > precision) {
      val newX = curX + -gamma * curX //df(curX) == V(X)
      gradientDescent(precision, abs(newX - curX), newX,gamma)
    } else curX
  }
  @annotation.tailrec
  final def gradDesc(rdd: Array[Array[Double]],D: Array[Array[Double]],sigma:Double,eta : Double, etaDecay : Double, init_steps: Int, steps: Int,dim: Int):Array[Array[Double]] ={

    val n = rdd.length
    val q= 1/(2*sigma*sigma)
    val v = wave(rdd, D , sigma,dim)
    //val sum = v.map(x => sum(x*x)).sum
     //v= breeze.linalg.normalize(v, Axis._0, 1.0)
    /*D.zipWithIndex().map({case(obj,index) =>
      val cst = eta * v(index.toInt)
      obj.pointPartNum.map(x => x - cst)})
    */

    for (i<-0 until n){
      for (j <- 0 until dim){

        val cst =  v(i,j) * eta
        //println(v(i,j)+" "+cst)
        D(i)(j) -= cst
         //d(i).pointPartNum = (d(i).pointPartNum.toArray zip v).map {case(x,y) => x-y}.toVector
      }
    }

    if (steps <= 1) {
      //val win2 = smile.plot.plot(d.map(x=>x.pointPartNum.toArray),'#')
          D
    }
    else {
       println(steps)
       gradDesc(rdd, D, sigma, eta * etaDecay, etaDecay, init_steps, (steps - 1),dim)
    }
  }
def assignClusters(rdd:Array[Array[Double]],sigma:Double):Array[Int] ={

  val n = rdd.length

  var labels = Array.fill[Int](n)(0)
  var label = 1
  val min_d = 1.08

  for(i <- 0 until n){
    if(labels(i) == 0){
      labels(i)=label
      for(j <- i+1 until n){
        if(labels(j) == 0) {
          var dist = euclideanDistance(rdd(i), rdd(j))
            if (dist < min_d) {
            labels(j) = label
          }
        }
      }
      label+=1
    }
  }
  println(label)
  labels
}

}

