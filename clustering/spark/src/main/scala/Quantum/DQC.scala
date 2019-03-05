package batchStream

import java.util.Calendar

import Quantum.{DQCModel, pointObj}
import breeze.linalg.{Vector, squaredDistance}
import breeze.numerics.{exp, pow, sqrt}
import org.apache.log4j.{Level, Logger}
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


  /*val data = data1.map(e =>
    model.pointToObjet(e, dim, labId))*/
    //model.wave(data,data,sigma,dim)
  val gamma = 0.01
  val precision = 0.00001
  val steps = 200

  val previousStepSize = 1 / precision
  //val gradient = model.V.map(x=>model.gradientDescent(precision, previousStepSize, x,gamma))

 /* var x = model.normalize(data,dim).collect().map(x=>x.pointPartNum)
  var c=0
  val beta = 0.15
  var labels = Array.fill[Int](n)(0)
  var nodes = ArrayBuffer[Vector[Double]]()
  var m = n
  var iter = 0
 while(m>0 && iter < 5){
   iter+=1
    c += 1
   while(x(model.V.indexOf(model.V.min))==null) {
     val vmin = model.V.indexOf(model.V.min)
     model.V(vmin)=Double.MaxValue
   }

     val vmin = model.V.indexOf(model.V.min)
     val xmin = x(vmin).toArray
     x(vmin) = null
     nodes .+= (x(vmin))
     val dist = x.map(y => if(y!=null) model.euclideanDistance(xmin,y.toArray) else Double.MaxValue)
     for(i<-0 until dist.length){
        if(dist(i)<beta && dist(i)>0){
          labels(i)=c
          x(i)=null
          m-=1
        }
      }
  }
  val v= data1.collect()(0).length
  var datacollected = data1.map(_.take(v-2)).collect()
*/
  val data = data1.map(_.take(v-2))

var datacollected = data.collect()
  val data2d = sammon(pdist(datacollected, false), 2).getCoordinates
  sc.parallelize(data2d).coalesce(1).saveAsTextFile(path+"results/Quantum-waveform")

  //val win = smile.plot.plot(data2d, labels,'#',Palette.COLORS)

  val eta = sigma/2.5
  val etaDecay = 0.6

  val D = model.gradDesc(data, datacollected, sigma, eta, etaDecay, steps, steps, dim)

 // D.foreach(x=>println(x.toVector))
  var labelspredicted = model.assignClusters(model.normalize(D,dim),sigma)
  val D2d = sammon(pdist(D, false), 2).getCoordinates
  val win2 = smile.plot.plot(D2d, labels,'#', Palette.COLORS)
  val arand = new AdjustedRandIndex
  val nmi = new MutualInformationScore

  println(nmi.measure(labels, labelspredicted))
  println(arand.measure(labels, labelspredicted))
  //val onepoint = model.wave(datacollected,datacollected,sigma,dim)
  //println(onepoint)



  }
