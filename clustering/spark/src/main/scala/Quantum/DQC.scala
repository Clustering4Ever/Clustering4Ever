package batchStream

import java.util.Calendar

import Quantum.{DQCModel, pointObj}
import breeze.linalg.{Vector, squaredDistance}
import breeze.numerics.{exp, pow, sqrt}
import org.apache.log4j.{Level, Logger}
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.rdd.RDD

import scala.collection.mutable.ArrayBuffer

object DQC extends App{

  Logger.getLogger("org").setLevel(Level.OFF)
  Logger.getLogger("akka").setLevel(Level.OFF)

  val path="C:/Users/ATTAOUI/Documents/SS_GStream/"
  val sparkConf = new SparkConf().setAppName(this.getClass().getName())
  sparkConf.setMaster("local[2]")
  val now = Calendar.getInstance().getTime()

  val sc = new SparkContext(sparkConf)

  val DSname="waveform"
  var data1 = sc.textFile(path+"resources/"+DSname+".txt").map(x => x.split(',').map(_.toDouble))
  val labId = 2
  val dim = data1.take(1)(0).size - labId
  val n = data1.count().toInt
  val sigma = 0.8//pow(4/(dim+2)/n,1/dim+4)
  var model: DQCModel = new DQCModel()


  val data = data1.map(e =>
    model.pointToObjet(e, dim, labId))
    model.wave(data,sigma,dim)


}
