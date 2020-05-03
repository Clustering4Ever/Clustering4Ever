package org.clustering4ever.spark.clustering.s2gstream
/**
 * @author Attaoui Walid
 * @author Beck Gaël
 * @author Ghesmoune Mohammed
 */
import java.io.{FileWriter, Serializable}
import java.util.Calendar
import breeze.linalg.sum
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.rdd.RDD
import org.apache.spark.streaming.dstream.DStream
import scala.collection.mutable
import org.clustering4ever.spark.streamclustering.{Prototype, PointObj}
/** 
 *
 */
class S2gstream(
  val voisinage: Int,
  var decayFactor: Double,
  var lambdaAge : Double,
  var nbNodesToAdd: Int,
  var minWeight: Double,
  var maxAge: Int,
  var alphaErr: Double,
  var d: Double
  ) extends Serializable {


  def this() = this(voisinage = 0, 
    decayFactor = 0.9, 
    lambdaAge = 1.2, 
    nbNodesToAdd = 3, 
    minWeight = 1,
    maxAge = 250, 
    alphaErr = 0.5,
    d = 0.99)

  var model: S2gstreamModel = new S2gstreamModel()

  def getModel: S2gstreamModel = model

  // Set the 'max inserted nodes' parameter.
  def setMaxInsert(insert: Int): this.type = {
    nbNodesToAdd = insert
    this
  }


  // Set the decay factor
  def setDecayFactor(alpha: Double): this.type = {
    decayFactor = alpha
    this
  }

  // Set the lambdaAge factor
  def setLambdaAge(lambda: Double): this.type = {
    lambdaAge = lambda
    this
  }
  // Set the 'min weight' of nodes parameter.
  def setMinWeight(w: Double): this.type = {
    minWeight = w
    this
  }

  // Set the 'maxAge' parameter.
  def setMaxAge(a: Int): this.type = {
    maxAge = a
    this
  }

  // Set the alpha parameter.
  def setAlphaErr(a: Double): this.type = {
    alphaErr = a
    this
  }

  // Set the d parameter.
  def setD(dValue: Double): this.type = {
    d = dValue
    this
  }

  // Initializing the model.
  def initModelObj(txt: RDD[Array[Double]], dim: Int): S2gstream = {
    val nodes2 = txt.take(2)
    val node1 = nodes2(0)
    val node2 = nodes2(1)
    model.init2NodesObj(node1, node2, dim, 1)
    this
  }


  // Training on the model.
  def trainOnObj(
    data: DStream[PointObj], 
    gstream: S2gstream, 
    dirSortie: String, 
    dim: Int, 
    nbWind: Int,
    streamData : Array[RDD[PointObj]],
    trueLabels : Array[Int],
    arrayData : Array[Array[Double]]
  ): S2gstreamModel = {

    val k = 60
    val b = 3
    val p = Array(10, 10)
    var bloc = Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    val rnd = new scala.util.Random
    var alpha = Array.ofDim[Double](k, b)
    var beta = Array.ofDim[Double](k, dim)
    var alphagen = Array.ofDim[Double](b)
    var betagen = Array.ofDim[Double](dim)
    var it = 0
    var eta = 11D
    var lambda = 7D
    
    //initialiser les poids sur les bloc et les variables
    for (i <- 0 until k) {

      @annotation.tailrec
      def goSumal(j: Int, sum: Double): Double = {
        if (j < b) {
          val rndDble = rnd.nextDouble()
          alpha(i)(j) = rndDble
          goSumal(j + 1, sum + rndDble)
        }
        else sum
      }

      val sumal = goSumal(0, 0D)

      var sumb = 0D
      val dimRange = 0 until dim

      for (j <- dimRange) {
        beta(i)(j) = rnd.nextDouble()
        sumb += beta(i)(j)
      }

      dimRange.foreach(beta(i)(_) /= sumb)

      (0 until b).foreach(alpha(i)(_) /= sumal)

    }

    alphagen = alphagen.map(_ => rnd.nextDouble())
    betagen = betagen.map(_ => rnd.nextDouble())

    var sumb = sum(betagen)
    var sumal = sum(alphagen)
    betagen.map(_ / sumb)
    alphagen.map(_ / sumal)

    //initialiser les blocs
    val now = Calendar.getInstance().getTime()

    var timeUpdates = mutable.ArrayBuffer(0L)
    var kk = 1
    var t2 = 0D

    streamData.foreach{ rdd =>
      rdd.cache
      val count = rdd.count()
      if (count > 0) {
        val initialTimeUpdate = System.currentTimeMillis()
        println("\n<<<<<<<<<<<<<<<< >>>>>>>>>>>>>>>--S2gstream--(batch: "+ kk +" )..."+" rdd.count: "+ count +" \n")

        model = model.updateObj(rdd, gstream, kk, dim, alpha, beta, alphagen, betagen, b, lambda, eta, bloc, p)
        model = model.updateFeatureWeight(rdd, gstream, alpha, beta, alphagen, betagen, dim, b, lambda, eta, bloc, p, count.toInt)
        model = model.updateGroupWeight(rdd, gstream, alpha, beta, alphagen, betagen, dim, b, lambda, eta, bloc, p, count.toInt)

        it += 1
        timeUpdates += (timeUpdates(timeUpdates.size - 1) + (System.currentTimeMillis() - initialTimeUpdate))
       
        if (timeUpdates.length > 100) timeUpdates.remove(0)

        // Uncomment to Save the  global and local weights

        /*for (i <- 0 until model.nodes.size) {
          rdd.context.parallelize(alpha(i).map(_.toString)).coalesce(1).saveAsTextFile(dirSortie+"/alpha-weights/iter"+it+"/Noeud "+i)
          rdd.context.parallelize(beta(i).map(_.toString)).coalesce(1).saveAsTextFile(dirSortie+"/beta-weights/iter"+it+"/Noeud "+i)
        }
       
        rdd.context.parallelize(alphagen.map(_.toString)).coalesce(1).saveAsTextFile(dirSortie+"/alphagen-weights/iter"+it)
        rdd.context.parallelize(betagen.map(_.toString)).coalesce(1).saveAsTextFile(dirSortie+"/betagen-weights/iter"+it)
        */

        val proto: String = model.toStringProto.mkString("\n")
        val fw = new FileWriter(dirSortie + "Prototypes", false)
        fw.write(proto)
        fw.close

        kk += 1

      }

      else {
        println("Elapsed time: " + (t2) + "ms")
        println("-- S2gstream: empty rdd -- rdd.count : "+count)
      }

    }

    model

  }

}
