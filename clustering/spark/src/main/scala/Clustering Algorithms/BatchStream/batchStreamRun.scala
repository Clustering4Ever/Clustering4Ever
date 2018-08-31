/*
package org.lipn.clustering.BatchStream

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.streaming.{Seconds, StreamingContext, Milliseconds}
import org.apache.spark.streaming.dstream.DStream
import scala.collection.mutable.ArrayBuffer

object BatchStreamRun {

	def main(args: Array[String]) {	  
		/**
		* Copyright: please refer to the README.md file
		* User: ghesmoune
		* Date: 01/01/2016
		* Project : Square Predict (http://square-predict.net/)
		* */ 
		val master = args(0)
		val dirData = args(1)
		val dirSortie = args(2)
		val DSname = args(3)
		val separator = args(4)
		val decayFactor = args(5).toDouble
		val lambdaAge = args(6).toDouble
		val nbNodesToAdd = args(7).toInt
		val nbWind = args(8).toInt
			  
		val sparkConf = new SparkConf().setAppName(this.getClass().getName())
		sparkConf.setMaster(master)

		val sc = new SparkContext(sparkConf)
		val interval = 100
		val batchInterval = Milliseconds(interval)
		val ssc = new StreamingContext(sc, batchInterval)

		// 'points2' contains the first two data-points used for initialising the model
		val points2 = sc.textFile(dirData+"/nodes2.txt").map(x => x.split(separator).map(_.toDouble))

		// Create a DStreams that reads batch files from dirData
		val stream = ssc.textFileStream(dirData).map(x => x.split(separator).map(_.toDouble))
		// Create a DStreams that will connect to a socket hostname:port
		//val stream = ssc.socketTextStream("localhost", 9999).map(x => x.split(separator).map(_.toDouble)) //localhost or 10.32.2.153 for Teralab

		val labId = 2 //TODO: change -1 to -2 when you add the id to the file (last column) //-2 because the last 2 columns represent label & id
		val dim = points2.take(1)(0).size - labId
		var gstream = new BatchStream()
			.setDecayFactor(decayFactor)
			.setLambdaAge(lambdaAge)
			.setMaxInsert(nbNodesToAdd)

		// converting each point into an object
		val dstreamObj = stream.map( e =>
		gstream.model.pointToObjet(e, dim, labId)
	)
    
    // dstreamObj.cache() //TODO: to save in memory
    
    // initialization of the model by creating a graph of two nodes (the first 2 data-points)
    gstream.initModelObj(points2, dim)
    
    // training on the model
    gstream.trainOnObj(dstreamObj, gstream, dirSortie+"/"+DSname+"-"+nbNodesToAdd, dim, nbWind)
    
    ssc.start()
    ssc.awaitTermination() //stop(true, true)
	  
  }
}
*/