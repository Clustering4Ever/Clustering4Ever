package org.clustering4ever.clustering.stream.batchstream

/**
	  * Copyright: please refer to the README.md file
	  * User: ghesmoune
	  * Date: 01/01/2016
	  * Project : Square Predict (http://square-predict.net/)
	  * */ 

import org.apache.spark.rdd.RDD
import org.apache.spark.streaming.dstream.DStream
import org.clustering4ever.clustering.stream.s2gstream.PointObj

import scala.collection.mutable.ArrayBuffer

class BatchStream(
    var voisinage: Int, 
    var decayFactor: Double, 
    var lambdaAge : Double, 
    var nbNodesToAdd: Int, 
    var minWeight: Double   , 
    var maxAge: Int, 
    var alphaErr: Double, 
    var d: Double) extends Serializable {
  
  def this() = this(voisinage = 0, decayFactor = 0.9, lambdaAge = 1.2, nbNodesToAdd = 3, minWeight = 1, maxAge = 250, alphaErr = 0.5, d = 0.99)
  
  var model: BatchStreamModel = new BatchStreamModel()
      
  def getModel: BatchStreamModel = model

  
  // Initializing the model.
  def initModelObj(txt: RDD[Array[Double]], dim: Int): BatchStream = { 
    val nodes2 = txt.take(2)
    val node1 = nodes2(0)
    val node2 = nodes2(1)
    model.init2NodesObj(node1, node2, dim, 1)
    this 
  }  
 

  // Training on the model.
  def trainOnObj(data: DStream[PointObj], gstream: BatchStream, dirSortie: String, dim: Int, nbWind: Int) = {
    val timeUpdates = ArrayBuffer[Long](0L)
    var kk = 1
     data.foreachRDD{ rdd =>
      if ( rdd.count() > 0 ) {
        val initialTimeUpdate = System.currentTimeMillis()
        println("\n<<<<<<<<<<<<<<<< >>>>>>>>>>>>>>>--BatchStream--(batch: " + kk + " )..." + " rdd.count: " + rdd.count() + " \n")
        
        // Update model without var affectation
        model = model.updateObj(rdd, gstream, kk, dim)

        timeUpdates += (timeUpdates(timeUpdates.size - 1) + (System.currentTimeMillis() - initialTimeUpdate))
        if (timeUpdates.length > 100) timeUpdates.remove(0)
        
        if ( (kk == 1) | (kk == nbWind / 9) | (kk == 2 * nbWind / 9) | (kk == 3 * nbWind / 9) | (kk == 4 * nbWind / 9) | (kk == 5 * nbWind / 9) | (kk == 6 * nbWind / 9) | (kk == 7 * nbWind / 9) | (kk == 8 * nbWind / 9) | (kk > (8 * nbWind / 9) + 10 & kk % 10 == 0) | (kk >= nbWind - 2) ) {
          rdd.context.parallelize(model.toStringProto).saveAsTextFile(dirSortie+"/Prototypes-"+kk)
          rdd.context.parallelize(model.toStringOutdatedProto).saveAsTextFile(dirSortie+"/OutdatedProtos-"+kk)
          rdd.context.parallelize(model.edges).saveAsTextFile(dirSortie+"/Edges-"+kk)
          rdd.context.parallelize(model.clusterWeights).saveAsTextFile(dirSortie+"/Weights-"+kk)           
          rdd.context.parallelize(timeUpdates).saveAsTextFile(dirSortie+"/timeUpdates-"+kk)          
        }
        kk += 1
      }
      else println("-- BatchStream: empty rdd -- rdd.count : "+rdd.count())
    }
    model
  }

}
