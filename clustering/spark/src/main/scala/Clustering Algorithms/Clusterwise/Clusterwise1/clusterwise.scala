package clustering4ever.spark.clustering.clusterwise

import org.apache.spark.{SparkContext, SparkConf, HashPartitioner}
import scala.util.Random
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.math.{pow, sqrt}
import breeze.linalg.{DenseVector, DenseMatrix}
import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.clustering._
import org.apache.spark.mllib.linalg.{Vector, Vectors}



class Clusterwise(@transient val sc:SparkContext, val dataXY:RDD[(Int, (Array[Double],Array[Double]))])(var g: Int, var h: Int, var nbCV: Int, var init: Int, var perGroup: Boolean=false, var sizeBloc: Int = 10, var trainSize: Double = 0.9)(var k:Int) extends Serializable
{
	def clusterwise() =
	{
		dataXY.cache
		val n = dataXY.count.toInt
		val first = dataXY.first
		val q = first._2._2.size  // dimY
		val p = first._2._1.size  // dimX
		val initPerNode = 1
		
		val dp = sc.defaultParallelism

		val kkmeans = n / sizeBloc
		val sqrmseCal = ArrayBuffer.empty[Double]
		val corYpredCal = ArrayBuffer.empty[Double]
		val sqrmseVal = ArrayBuffer.empty[Double]
		val corYpredVal = ArrayBuffer.empty[Double]
		val rdsplitArr = Array.fill(nbCV)(1D / nbCV)
		val splits =  dataXY.randomSplit(rdsplitArr)
		splits.map(_.repartitionAndSortWithinPartitions(new HashPartitioner(dp)).cache)
  	  	val localSplits = splits.map(_.collect)
  	  	dataXY.unpersist(false)

  	  	val kmeans = new KMeans
  	  	val epsilon = 0.0001
  	  	val jmax = 20
  	  	val kmData = dataXY.map{ case (id, (x, y)) => Vectors.dense( x ++ y ) }.cache
  	  	val kmeansModel = kmeans.setEpsilon(epsilon).setK(kkmeans).setMaxIterations(jmax).run(kmData)

  	  	val labels = kmeansModel.predict(kmData)

  	  	val groupedData = if( perGroup ) sc.broadcast( HashMap( dataXY.zip(labels).map{ case ((id, (x, y)), label) => (id, label) }.collect:_*) ) else sc.broadcast(HashMap.empty[Int, Int])

		for( j <- 0 until nbCV )
		{
			// We have to sort the CV per ID in order to it work, the why is still to determine
			val trainXYpar = (for( u <- 0 until nbCV ) yield( if( u == j ) Array.empty[(Int, (Array[Double], Array[Double]))] else (localSplits(u)) )).reduce(_ ++ _).sortBy{ case (id, _) => id }.par
	 		val trainSize = trainXYpar.size
  	  		val testSize = splits(j).count.toInt

  	  		// (x - mu) / sd
  	  		val (preMeanX, preMeanY) = trainXYpar.map{ case (id, (x, y)) => (x, y) }.reduce((a, b) => (a._1.zip(b._1).map(x => x._1 + x._2), a._2.zip(b._2).map(x => x._1 + x._2)))
  	  		val meanX = preMeanX.map(_ / trainSize)
  	  		val meanY = preMeanY.map(_ / trainSize)

  	  		val (preSDX, preSDY) = trainXYpar.map{ case (id, (x, y)) => (x.zipWithIndex, y.zipWithIndex) }.map{ case (x, y) => (x.map{ case (v, idx) => pow(v - meanX(idx), 2) }, y.map{ case (v, idx) => pow(v - meanX(idx), 2) }) }.reduce( (a, b) => (a._1.zip(b._1).map(x => x._1 + x._2), a._2.zip(b._2).map(x => x._1 + x._2)) )
  	  		val sdX = preSDX.map(v => sqrt(v / (n - 1)))
  	  		val sdY = preSDY.map(v => sqrt(v / (n - 1)))

  	  		val trainXY = sc.broadcast( trainXYpar.map{ case (id, (x, y)) => (id, (x.zipWithIndex.map{ case (v, idx) => (v - meanX(idx)) / sdX(idx) }, y.zipWithIndex.map{ case (v, idx) => (v - meanX(idx)) / sdX(idx) }))}.toArray )

			// Launch Meta Reg on each partition
			val resRegOut = sc.parallelize( 1 to 10000, init).mapPartitions( it =>
			{

				val modelTrain = ArrayBuffer.empty[Array[Array[(Int,(Array[Double], Array[Double],Int))]]]
				val predFitted = ArrayBuffer.empty[Array[Array[(Int, Array[Double])]]]
				val prediction = ArrayBuffer.empty[ArrayBuffer[(Int, Int)]]
				val critReg = ArrayBuffer.empty[Array[Double]]
				val mapsRegCrit = ArrayBuffer.empty[HashMap[Int, Double]]
				val classedReg = ArrayBuffer.empty[Array[(Int, Int)]]
				val coIntercept = ArrayBuffer.empty[Array[Array[Double]]]
				val coXYcoef = ArrayBuffer.empty[Array[Array[Double]]]

				val regClass = new Regression(trainXY.value, h, g)(groupedData.value, kkmeans)
				  
				    for( i <- 0 until initPerNode )
				    {
					  	// For classic clusterwise with moove of one element
					  	if( ! perGroup )
					  	{
					  		val (_, predFitted0, coIntercept0, coXYcoef0, critReg0, mapsRegCrit0, classedReg0) = regClass.mbplsPerDot()
					  		predFitted += predFitted0
					  		coIntercept += coIntercept0
					  		coXYcoef += coXYcoef0
					  		critReg += critReg0
					  		mapsRegCrit += mapsRegCrit0
					  		classedReg += classedReg0
					  	}
					  	// For clusterwise per micro-cluster
					  	else
					  	{
					  		val (_, predFitted0, coIntercept0, coXYcoef0, critReg0, mapsRegCrit0, classedReg0) = regClass.mbplsPerGroup()
					  		predFitted += predFitted0
					  		coIntercept += coIntercept0
					  		coXYcoef += coXYcoef0
					  		critReg += critReg0
					  		mapsRegCrit += mapsRegCrit0
					  		classedReg += classedReg0
					  	}
					}
					  // Comparison of the predicted X.train and Y.train (normalized rmse, and cv-r2)
				      val minRegCritPerInit = mapsRegCrit.map(_.values.min)
				      val bestInitScore = minRegCritPerInit.min
				      val idxBestInit = minRegCritPerInit.indexOf(bestInitScore)
				      val bestClassifiedData = classedReg(idxBestInit)
				      val bestCoInterceptIn = coIntercept(idxBestInit)
				      val bestCoXYcoefIn = coXYcoef(idxBestInit)
				      val bestFitted = predFitted(idxBestInit)

			  	  Array((bestClassifiedData, bestInitScore, bestCoInterceptIn, bestCoXYcoefIn, bestFitted)).toIterator
		  	  
		  	}).collect

			/*********************************************************/
			/*  Selection of the results from the best intialization */
			/*********************************************************/
			val regScores = resRegOut.map{ case (_, bestInitScore, _, _, _) => bestInitScore }
			val idxBestRegScoreOut = regScores.indexOf(regScores.min)
			val mapBestClassifiedDataOut = HashMap(resRegOut(idxBestRegScoreOut)._1:_*)
			val bestCoInterceptOut = resRegOut(idxBestRegScoreOut)._3
			val bestCoXYcoefOut = resRegOut(idxBestRegScoreOut)._4
			val bestFittedOut = resRegOut(idxBestRegScoreOut)._5
			val dsLabeled = trainXY.value.map{ case (idx, (x, y)) => (idx, x, y, mapBestClassifiedDataOut(idx)) }

			/***********************************************************************************/
			/* 4. Compute the final G separate multiblock analyses (with the complete dataset) */
			/***********************************************************************************/

			val labeledRDD = sc.parallelize(dsLabeled).map{ case (idx, x, y, label) => (label, (idx, x, y))}.cache

			val finals = labeledRDD.partitionBy( new HashPartitioner(g) ).mapPartitions( it =>
			{
				val dataXYg = it.toArray.groupBy{ case (label, (idx, x, y)) => label }
				val classRegression = for( (label2, array2) <- dataXYg ) yield (
				{
					val ng = array2.size
					val _X = ArrayBuffer(array2.map{ case (_, (idx, x, _)) => (idx, x) }:_*)
					val _Y = ArrayBuffer(array2.map{ case (_, (_, _, y)) => y }:_*)
					val ktabXdudiY = Pls.ktabXdudiY(_X, _Y, ng)
					val (_, _XYcoef, _Intercept, _Pred) = Pls.pls(_X, _Y, ng, h, ktabXdudiY)
					(label2,( _Intercept.toArray, _XYcoef, _Pred))
				})
				classRegression.toIterator
			}).collectAsMap

			val bcLocalTrainRDD = sc.broadcast(labeledRDD.map{ case (label, (idx, x, y)) => (idx, (x, y, label))}.collect)

			val prediction = new Prediction(bcLocalTrainRDD, splits(j), k, g)

			val labelAndPrediction = prediction.cwPredictionKNNdistributed(finals)

			val yPredTrainSort = bestFittedOut.reduce(_ ++ _).toArray.sortBy(_._1)

			val trainY = trainXY.value.map{ case (idx, (x, y)) => y }
			val dimY = trainY.head.size

		 	val sdYtrain = trainY.map(_.zipWithIndex.map{ case (y, idx) => pow(y - meanY(idx), 2) }).reduce(_.zip(_).map( x => (x._1 + x._2) )).map( x => sqrt(x / (trainSize - 1)) )

		 	val sdYtest = splits(j).map{ case (idx, (x, y)) => y }.map(_.zipWithIndex.map{ case (y, idx) => pow(y - meanY(idx), 2) }).reduce(_.zip(_).map( x => x._1 + x._2 )).map(x => sqrt(x / (testSize - 1)))

			val sqrmseCalIn = if( q == 1 )
			{
			  	val sqrmse = trainY.zip(yPredTrainSort).map{ case ((tY, (idx, yPred))) => pow(tY.head - yPred.head, 2) }.sum / trainY.size / sdYtrain.head
				sqrmse
			}
			else
		 	{
			 	val meanY = trainY.reduce(_.zip(_).map( x => x._1 + x._2 )).map(_ / n)

			    val preColSum = trainY.zip(yPredTrainSort).map{ case ((tY, (idx, yPred))) => tY.zip(yPred).map( x => x._1 - x._2 ).map(pow(_, 2)) }
			    val colSum = preColSum.reduce(_.zip(_).map( x => x._1 + x._2 ))
			    val sqrmseYCal = colSum.map( _ / trainY.size ).zip(sdYtrain).map{ case (v, sdy) => v/sdy}
			    val meanSqrmseYCal = sqrmseYCal.reduce(_+_) / sqrmseYCal.size
				meanSqrmseYCal
			}						

			val testAndPredRDD = splits(j).repartitionAndSortWithinPartitions(new HashPartitioner(dp)).zip(labelAndPrediction.repartitionAndSortWithinPartitions(new HashPartitioner(dp))).cache

			val sqrmseValIn = if( q == 1 )
			{
				testAndPredRDD.map{ case ((idx, (x, y)), (idx2, (label, ypred))) => pow(y.head - ypred(0), 2) }.sum / testSize / sdYtrain.head
			}
			else
			{
				val sqrmseYVal = testAndPredRDD.map{ case ((idx, (x, y)), (idx2, (label, ypred))) => y.zip(ypred.toArray).map{ case (yTest, yPred) => pow(yTest - yPred, 2) } }.reduce(_.zip(_).map( x => x._1 + x._2 )).zipWithIndex.map{ case (value, idx) => value / testSize / sdYtest(idx) }
				val sqrmseYValMean = sqrmseYVal.reduce(_ + _) / q
				sqrmseYValMean
			}

			sqrmseCal += sqrmseCalIn
			sqrmseVal += sqrmseValIn
		}
		(sqrmseCal, sqrmseVal)	
	}
}
