package batchStream

import java.util.Calendar

import Quantum.{DQCModel, pointObj}
import breeze.linalg.{DenseMatrix, Vector, squaredDistance, svd}
import breeze.numerics.{exp, pow, sqrt}

import scala.util.control.Breaks._
import scala.collection.mutable.ArrayBuffer

object DQC extends App{


  val now = Calendar.getInstance().getTime()


  val DSname="waveform"
  val bufferedSource = scala.io.Source.fromFile("C:\\Users\\ATTAOUI\\Documents\\SS_GStream\\resources\\DS11.txt")
  val lines = (for (line <- bufferedSource.getLines()) yield line).toSeq
  val data = lines.map(x => x.split(',').map(_.toDouble)).toArray
  val n = data.size

  val labId = 2
  val v =  data(0).length
  var labels =  data.map(x=> x(v - 2).toInt)
  var model: DQCModel = new DQCModel()
  val data1 = data.map(_.take(v - 2))



  val sigma = 0.8 //pow(4/(dim+2)/n,1/dim+4)
  val eta = sigma/2.5
  val etaDecay = 0.6
  val steps = 200
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
  //val D2d = sammon(pdist(D, false), 2).getCoordinates
  val D = model.gradDesc(data, data, sigma, eta, etaDecay, steps, steps, 2)

}
