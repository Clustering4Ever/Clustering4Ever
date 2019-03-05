package batchStream

import java.util.Calendar

import Quantum.{DQCModel, pointObj}
import breeze.linalg.{DenseMatrix, Vector, squaredDistance, svd}
import breeze.numerics.{exp, pow, sqrt}
import org.apache.log4j.{Level, Logger}
import org.apache.spark.mllib.linalg.{Matrix, SingularValueDecomposition, Vectors}
import org.apache.spark.mllib.linalg.distributed.RowMatrix
import org.apache.spark.{SparkConf, SparkContext, rdd}
import org.apache.spark.streaming.{Milliseconds, StreamingContext}
import smile.plot.Palette
import smile.util.pdist
import smile.mds.sammon
import smile.validation.{AdjustedRandIndex, MutualInformationScore}

import scala.util.control.Breaks._
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
  val v= data1.collect()(0).length
  var labels =  data1.map(x=> x(v-2).toInt).collect()
  val n = data1.count().toInt
  val sigma = 0.8 //pow(4/(dim+2)/n,1/dim+4)
  var model: DQCModel = new DQCModel()
  val steps = 200
  val data = data1.map(_.take(v-2))

var datacollected = data.collect()
/*  val data2d = sammon(pdist(datacollected, false), 2).getCoordinates
  //sc.parallelize(data2d).coalesce(1).saveAsTextFile(path+"results/Quantum-waveform")

  val data2 = sc.textFile(path+"resources/"+DSname+".txt").map(line => Vectors.dense(line.split (",").map(_.toDouble).distinct))
  val mat: RowMatrix = new RowMatrix(data2)
  //val svd: SingularValueDecomposition[RowMatrix, Matrix] = mat.computeSVD(2, computeU = true)

  //val pc: Matrix = mat.computePrincipalComponents(4)

  // var A = new DenseMatrix(data2d)
  // val svd.SVD(u, s, v) = svd(A)

  val win = smile.plot.plot(model.normalize(data2d,2), labels,'#',Palette.COLORS)

  val eta = sigma/2.5
  val etaDecay = 0.6

 // val data3 = svd.U.rows.map(_.toArray).collect()
 // println(data3(0).length+" "+data3(0)(0))

  val D = model.gradDesc(data2d, data2d, sigma, eta, etaDecay, steps, steps, 2)

 // D.foreach(x=>println(x.toVector))
  var labelspredicted = model.assignClusters(model.normalize(D,2),sigma)
  //val D2d = sammon(pdist(D, false), 2).getCoordinates
  val win2 = smile.plot.plot(D, labelspredicted,'#',Palette.COLORS)
  val win3 = smile.plot.plot(D, labels,'#', Palette.COLORS)
  val arand = new AdjustedRandIndex
  val nmi = new MutualInformationScore

  println(nmi.measure(labels, labelspredicted))
  println(arand.measure(labels, labelspredicted))

  //val onepoint = model.wave(datacollected,datacollected,sigma,dim)
  //println(onepoint)
// val sim = model.calculSim(model.normalize(datacollected,dim))
 //println(sim(0)(1)+" "+sim(0)(2))
*/
  val data2d = sammon(pdist(datacollected, false), 2).getCoordinates
  val win = smile.plot.plot(model.normalize(data2d,2), labels,'#',Palette.COLORS)

  }
