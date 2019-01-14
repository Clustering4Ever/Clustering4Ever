package Quantum
import batchStream.DQC.sc
import breeze.numerics.{exp, sqrt}
import org.apache.spark.rdd.RDD
import breeze.linalg.{Vector, squaredDistance}

import scala.collection.mutable.ArrayBuffer
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
  def normalize(rdd: RDD[pointObj],dim:Int): RDD[pointObj] ={
    val n = rdd.count().toInt
    var sum = Array.ofDim[Double](dim)
    val data = rdd.collect()
    for(i <- 0 until dim){sum(i) = data(i).pointPartNum.toArray.sum}
    for(i <- 0 until n) {
      for (j <- 0 until dim) {
        data(i).pointPartNum(j) /= sum(j)
      }

    }
    sc.parallelize(data)
  }
  def Parzen(rdd: RDD[pointObj],sigma:Double, dim: Int) ={

    val n = rdd.count().toInt
    var W1 =  collection.mutable.ArrayBuffer.fill(n)(0.0)
    var delta =  collection.mutable.ArrayBuffer.fill(n)(0.0)

    val data = this.normalize(rdd,dim).collect()


    for (i <- 0 until n) {
      var point = data(i)
      var sum = 0.0
      for (j <- 0 until n) {
        var dist = euclideanDistance(point.pointPartNum.toArray,data(j).pointPartNum.toArray)
        W1(i) +=  exp(-dist/(2*sigma*sigma))
        delta(i) += dist * exp(-dist/2*sigma*sigma)
      }
      println(delta(i))

    }
    this.W=W1
    this.delta=delta
    this
  }

  def wave(rdd: RDD[pointObj],sigma:Double, dim: Int) ={
    this.Parzen(rdd,sigma,dim)
    val n = rdd.count().toInt
    var V =  collection.mutable.ArrayBuffer.fill(n)(0.0)

    for(i<-0 until n){
      V(i)=(1/(2*sigma*sigma*this.W(i)))*this.delta(i)
      //println(V(i))

    }
    val energy = V.reduceLeft(_ min _)
    V.map(a => a-energy)
    this.V=V
    this
  }

  // euclideanDistance
  def euclideanDistance(a: Array[Double], b: Array[Double]): Double = {
    val values = (a zip b).map{ case (x, y) => x - y }
    val size = values.size
    var sum = 0.0
    var i = 0
    while (i < size) {
      sum += values(i) * values(i)
      i += 1
    }
    sqrt(sum)
  }

}

