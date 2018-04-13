package clusterwise

import org.apache.spark.{SparkContext, SparkConf, HashPartitioner}
import scala.util.Random
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.math.{pow, sqrt}
import breeze.linalg.{DenseVector, DenseMatrix}
import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.clustering._
import org.apache.spark.mllib.linalg.{Vector, Vectors}
import smile.clustering.kmeans



class Clusterwise(@transient val sc:SparkContext, val dataXY:RDD[(Int, (Array[Double],Array[Double]))])(var g:Int, var h:Int, var nbCV:Int, var init:Int, var perGroup:Boolean=false, var sizeBloc:Int=10, var nbBucketLSH:Int=10, var lshPred:Boolean=false, var trainSize:Double=0.9)(var k:Int, var condFact:Int) extends Serializable
{
	def clusterwise() =
	{
		dataXY.cache
		val n = dataXY.count.toInt
		val first = dataXY.first
		val q = first._2._2.size  // dimY
		val p = first._2._1.size  // dimX		
		val dp = sc.defaultParallelism

		// Lsh constante
		val w0 = 1.0
		//val b0 = Random.nextDouble * w0
		val tabHash = Lsh.tabHash(100, p + q)
		var nbBloc = (n / sizeBloc).toInt
		val sqrmseCal = ArrayBuffer.empty[Double]
		val corYpredCal = ArrayBuffer.empty[Double]
		val sqrmseVal = ArrayBuffer.empty[Double]
		val corYpredVal = ArrayBuffer.empty[Double]
		val modelingTimeBuf = ArrayBuffer.empty[Double]
		val predictTimeBuf = ArrayBuffer.empty[Double]
		val idLabelized = ArrayBuffer.empty[Array[(Int, Int)]]
		val rdsplitArr = Array.fill(nbCV)(1.0 / nbCV)

		val splits0 =  dataXY.randomSplit(rdsplitArr)
		val splits = splits0.map(_.repartitionAndSortWithinPartitions(new HashPartitioner(dp)).cache)
  	  	val localSplits = splits.map(_.collect)

  	  	val ts1 = System.nanoTime

  	  	var groupedData = if( perGroup && condFact == 1)
  	  	{
	  	  	val kmeans = new KMeans
	  	  	val epsilon = 0.0001
	  	  	val jmax = 20
  	  		val kkmeans = nbBloc
	  	  	val kmData = dataXY.map{ case (_, (x, y)) => Vectors.dense( x ++ y ) }.cache
	  	  	val kmeansModel = kmeans.setEpsilon(epsilon).setK(kkmeans).setMaxIterations(jmax).run(kmData)
			val labelsIn = kmeansModel.predict(kmData)
  	  		val groupedDataIn = HashMap(dataXY.zip(labelsIn).map{ case ((id, (x, y)), label) => (id, label) }.collect:_*)
			sc.broadcast( groupedDataIn )
		}
		else sc.broadcast( HashMap.empty[Int, Int] )
		
  	  		
  	  	dataXY.unpersist(false)


  	  	val ts2 = System.nanoTime
  	  	val kmeansTime = (ts2 - ts1).toDouble / 1000000000
  	  	println("kmeans done in " + kmeansTime + " s")

		type x_y = (Array[Double], Array[Double]) 
		val reduceXY : (x_y, x_y) => x_y = (a, b) =>
			( a._1.zip(b._1).map( x => x._1 + x._2), a._2.zip(b._2).map( x => x._1 + x._2) )

		for( j <- 0 until nbCV )
		{
			// We have to sort the CV per ID in order to it work, the why is still to determine
			val trainXYparPreCond = (for( u <- 0 until nbCV ) yield( if( u == j ) Array.empty[(Int, (Array[Double], Array[Double]))] else (localSplits(u)) )).reduce(_ ++ _).sortBy{ case (id, _) => id }.par


			val readyForKmeans = trainXYparPreCond.map{ case (_, (x, y)) => x ++ y }.toArray
	 		val trainSizePreCond = trainXYparPreCond.size

			val trainXYpar = if( condFact == 1 ) trainXYparPreCond
							 else
							 {
		  	  					val jmax = 20
								val kMeansClusters = kmeans(readyForKmeans, trainSizePreCond / condFact, jmax)
								val clustersLabels = kMeansClusters.getClusterLabel
							 	trainXYparPreCond.zip(clustersLabels).groupBy{ case (_, label) => label }.toArray.par.map{ case (label, microCluster) => microCluster.map{ case ((_, (x, y)), _) => (x, y) }.reduce( reduceXY(_, _) ) }.zipWithIndex.map(_.swap)
							 }
	 		val trainSize = trainXYpar.size
/*
			if( perGroup && condFact != 1 )
			{
		  	  	val kmeans = new KMeans
		  	  	val epsilon = 0.0001
		  	  	val jmax = 20
	  	  		val kkmeans = (trainSize / sizeBloc).toInt
		  	  	val kmData = sc.parallelize(trainXYpar.toArray.toSeq).map{ case (_, (x, y)) => Vectors.dense( x ++ y ) }.cache
		  	  	val kmeansModel = kmeans.setEpsilon(epsilon).setK(kkmeans).setMaxIterations(jmax).run(kmData)
				val labelsIn = kmeansModel.predict(kmData).collect
	  	  		val groupedDataIn = HashMap(trainXYpar.zip(labelsIn).map{ case ((id, _), label) => (id, label) }.toArray.toSeq:_*)
				groupedData.destroy
				groupedData = sc.broadcast( groupedDataIn )
			}
*/
			if( perGroup && condFact != 1 )
			{
		  	  	val jmax = 20
	  	  		nbBloc = (trainSize / sizeBloc).toInt
	  	  		val kkmeans = nbBloc
				val kMeansClusters = kmeans(readyForKmeans, kkmeans, jmax)
				val clustersLabels = kMeansClusters.getClusterLabel
	  	  		val groupedDataIn = HashMap(trainXYpar.zip(clustersLabels).map{ case ((id, _), label) => (id, label) }.toArray.toSeq:_*)
				groupedData.destroy
				groupedData = sc.broadcast( groupedDataIn )
			}


  	  		val testSize = splits(j).count.toInt
  	  		// (x - mu) / sd
  	  		val (preMeanX, preMeanY) = trainXYpar.map{ case (_, (x, y)) => (x, y) }.reduce( reduceXY(_, _) )
  	  		val meanX = preMeanX.map(_ / trainSize)
  	  		val meanY = preMeanY.map(_ / trainSize)

  	  		val (preSDX, preSDY) = trainXYpar.map{ case (_, (x, y)) => (x.zipWithIndex, y.zipWithIndex) }.map{ case (x, y) => (x.map{ case (v, idx) =>  pow(v - meanX(idx), 2) }, y.map{ case (v, idx) => pow(v - meanX(idx), 2) }) }.reduce( reduceXY(_, _) )
  	  		val sdX = preSDX.map( v => sqrt(v / (n - 1)))
  	  		val sdY = preSDY.map( v => sqrt(v / (n - 1)))

  	  		// Center Reduct
  	  		val trainXY = sc.broadcast( trainXYpar.map{ case (id, (x, y)) => (id, (x.zipWithIndex.map{ case (v, idx) => (v - meanX(idx)) / sdX(idx) }, y.zipWithIndex.map{ case (v, idx) => (v - meanX(idx)) / sdX(idx) }))}.toArray )
  	  		//val trainXY = sc.broadcast( trainXYpar.toArray )

  	  		val t01 = System.nanoTime

		 // Launch Meta Reg on each partition
			val resRegOut = sc.parallelize( 1 to 1992, init).mapPartitions( it => {

				val modelTrain = ArrayBuffer.empty[Array[Array[(Int,(Array[Double], Array[Double],Int))]]]
				val predFitted = ArrayBuffer.empty[Array[Array[(Int, Array[Double])]]]
				val prediction = ArrayBuffer.empty[ArrayBuffer[(Int, Int)]]
				val critReg = ArrayBuffer.empty[Array[Double]]
				val mapsRegCrit = ArrayBuffer.empty[HashMap[Int, Double]]
				val classedReg = ArrayBuffer.empty[Array[(Int, Int)]]
				val coIntercept = ArrayBuffer.empty[Array[Array[Double]]]
				val coXYcoef = ArrayBuffer.empty[Array[Array[Double]]]

				val regClass = new Regression(trainXY.value, h, g)(groupedData.value, nbBloc)
				  
			  	//Pour un element
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
				// Comparison of the predicted X.train and Y.train (normalized rmse, and cv-r2)
				// On prend le meilleur score de regression
				//val predictionMapsPerNode = prediction.map(_.toMap)
				//val labelizedTestPerNode = for(i<-predictionMapsPerNode.indices) yield(splits(j).value.map(x => (x._2,x._1,predictionMapsPerNode(i)(x._2))))

				val minRegCritPerInit = mapsRegCrit.map(_.values.min)
				val bestInitScore = minRegCritPerInit.min
				val idxBestInit = minRegCritPerInit.indexOf(bestInitScore)
				val bestClassifiedData = classedReg(idxBestInit)
				val bestCoInterceptIn = coIntercept(idxBestInit)
				val bestCoXYcoefIn = coXYcoef(idxBestInit)
				val bestFitted = predFitted(idxBestInit)

			  	Iterator((bestClassifiedData, bestInitScore, bestCoInterceptIn, bestCoXYcoefIn, bestFitted))
		  	  
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
			//val dsLabeled = trainXY.value.map{ case (idx, (x, y)) => (idx, x, y, mapBestClassifiedDataOut(idx))}
		
			/***********************************************************************************/
			/* 4. Compute the final G separate multiblock analyses (with the complete dataset) */
			/***********************************************************************************/

			val labeledRDD = sc.parallelize(trainXY.value).map{ case (id, (x, y)) => (mapBestClassifiedDataOut(id), (id, x, y)) }
			//.map{ case (idx, x, y, label) => (label, (idx, x, y))}.cache


			// Keep labeled data
			idLabelized += labeledRDD.map{ case (label, (id, _, _)) => (id, label) }.sortBy(_._1).collect

			val finals = labeledRDD.partitionBy( new HashPartitioner(g) ).mapPartitions( it => {
					val dataXYg = it.toArray.groupBy{ case (label, (idx, x, y)) => label }
					//val classRegression = for( (label2, array2) <- dataXYg ) yield(
					val classRegression = dataXYg.map{ case (label2, array2) =>
					{
						val ng = array2.size
						val lw = 1.0 / ng
						val tmpBuffer = ArrayBuffer(array2:_*)
						val _X = tmpBuffer.map{ case (_, (idx, x, _)) => (idx, x) }
						val _Y = tmpBuffer.map{ case (_, (_, _, y)) => y }
						val ktabXdudiY = Mbpls.ktabXdudiY(_X, _Y, ng)
						val (_, _XYcoef, _Intercept, _Pred) = Mbpls.mbpls(_X, _Y, lw, ng, h, ktabXdudiY)

						(label2,( _Intercept, _XYcoef, _Pred))
					}}
					classRegression.toIterator
				}).collectAsMap//.sortBy{ case (label, _, _, _) => label }

/*
			val (_XYcoef, _Intercept, _Pred) = finals.values.head
			println("**************")
			_Intercept.toArray.foreach(println)
			println("**************")
			_XYcoef.toArray.foreach(println)
			println("**************")
*/
			val t02 = System.nanoTime
			val modelingTime = (t02 - t01).toDouble / 1000000000
			modelingTimeBuf += modelingTime

			println("Modeling : "+ modelingTime + " s")

			/********************************************************************************************************/
			/* 										Test the model on testing set 									*/
			/********************************************************************************************************/

			val ttest1 = System.nanoTime

			val bcLocalTrainRDD = sc.broadcast( labeledRDD.map{ case (label, (idx, x, y)) => (idx, (x, y, label))}.collect )

			val prediction = new Prediction(bcLocalTrainRDD, splits(j), k, g)(w0)

			val labelAndPrediction = prediction.cwPredictionKNNdistributed(tabHash, nbBucketLSH, lshPred, finals)

			val yPredTrainSort = bestFittedOut.reduce(_ ++ _).toArray.sortBy(_._1)

			val ttest2 = System.nanoTime
			val predictTime = (ttest2 - ttest1).toDouble / 1000000000
			predictTimeBuf += predictTime
			/********************************************************************************************************/
			/*										Measure quality of prediction 									*/
			/********************************************************************************************************/

			val trainY = trainXY.value.map{ case (_, (_, y)) => y }
			//val dimY = trainY.head.size
		 	//val meanY = trainY.reduce(_.zip(_).map( x=> x._1+x._2)).map(_/n)

		 	//val sdYtrain = trainY.map(_.zipWithIndex).map(_.map( y=> (pow(y._1-meanY(y._2), 2),y._2))).reduce(_.zip(_).map( x=> (x._1._1 + x._2._1, x._1._2))).map(x => sqrt(x._1 / (trainSize - 1)))
		 	val sdYtrain = trainY.map(_.zipWithIndex).map(_.map( y => pow(y._1 - meanY(y._2), 2))).reduce(_.zip(_).map( x => (x._1 + x._2))).map(x => sqrt(x / (trainSize - 1)))

		 	//val sdYtest = splits(j).map{ case (idx, (x, y)) => y}.map(_.zipWithIndex).map(_.map( y=> (pow(y._1-meanY(y._2),2),y._2))).reduce(_.zip(_).map(x => (x._1._1 + x._2._1, x._1._2))).map(x => sqrt(x._1 / (testSize - 1)))
		 	val sdYtest = splits(j).map{ case (_, (_, y)) => y }.map(_.zipWithIndex).map( _.map{ case(y, meanId) => pow(y - meanY(meanId), 2) }).reduce(_.zip(_).map( x => x._1 + x._2 )).map( x => sqrt(x / (testSize - 1)))

			val sqrmseCalIn = if( q == 1 )
			{
			  	val sqrmse = trainY.zip(yPredTrainSort).map{ case ((tY, (_, yPred))) => pow(tY(0) - yPred(0), 2)}.reduce(_+_) / trainY.size / sdYtrain(0)
				//corYpredCal += pow(MyFct.pearson(DenseVector(trainY.map(_(0))), DenseVector(yPredTrainSort.map(_._2(0)))), 2) 
				sqrmse
			}
			else
		 	{
			 	val meanY = trainY.reduce(_.zip(_).map( x => x._1 + x._2 )).map(_ / n)

			    val preColSum = trainY.zip(yPredTrainSort).map{ case ((tY, (_, yPred))) => tY.zip(yPred).map( x => x._1 - x._2 ).map(pow(_, 2)) }
			    val colSum = preColSum.reduce(_.zip(_).map( x => x._1 + x._2 ))
			    val sqrmse_y_cal = colSum.map( _ / trainY.size ).zip(sdYtrain).map{ case (v, sdy) => v / sdy }
			    val mean_sqrmse_y_cal = sqrmse_y_cal.reduce(_+_) / sqrmse_y_cal.size
			    /*
			    val corYpredCalIn = for( i <- 0 until dimY) yield( pow(MyFct.pearson(DenseVector(trainY.map(_(i))), DenseVector(yPredTrainSort.map(_._2(i)))), 2) )
				val corYpredCalInMean = corYpredCalIn.reduce(_+_) / corYpredCalIn.size
				corYpredCal += corYpredCalInMean
				*/
				mean_sqrmse_y_cal
			}						

			val testAndPredRDD = splits(j)/*.repartitionAndSortWithinPartitions(new HashPartitioner(dp))*/.zip(labelAndPrediction.repartitionAndSortWithinPartitions(new HashPartitioner(dp))).cache

			val sqrmseValIn = if( q == 1 ) testAndPredRDD.map{ case ((idx, (x, y)), (idx2, (label, ypred))) => pow(y(0) - ypred(0), 2) }.reduce(_ + _) / testSize / sdYtrain(0)
				else
				{
					val sqrmse_y_val = testAndPredRDD.map{ case ((idx, (x, y)), (idx2, (label, ypred))) => y.zip(ypred.toArray).map{ case (yTest, yPred) => pow(yTest - yPred, 2) } }.reduce(_.zip(_).map( x => x._1 + x._2 )).zipWithIndex.map{ case (value, idx) => value / testSize / sdYtest(idx) }
					val sqrmse_y_val_mean = sqrmse_y_val.reduce(_ + _) / q
					sqrmse_y_val_mean
				}

/*
			val pearsonCoef = testAndPredRDD.map{ case ((idx, (x, y)), (idx2, (label, ypred))) => {
				val dot = for(i <- y.indices) yield( y(i) * ypred(i) )
				val yDot = for(i <- y) yield( i * i )
				val yPredDot = for(i <- ypred.toArray) yield( i * i )
				Array(dot.toArray, yDot.toArray, yPredDot.toArray, y, ypred.toArray)
				}}.reduce(_.zip(_).map( dots => dots._1.zip(dots._2).map( x => x._1 + x._2 )))

			val numeratorPearson = for( i <- 0 until q ) yield( pearsonCoef(0)(i) - ( ( pearsonCoef(3)(i) * pearsonCoef(4)(i) ) / testSize ) )
			
			val denumPearson = for( i <- 0 until q ) yield( sqrt( ( pearsonCoef(1)(i) - ( pow(pearsonCoef(3)(i), 2) / testSize ) ) * ( pearsonCoef(2)(i) - ( pow(pearsonCoef(4)(i), 2) / testSize ) ) ) )

			val squaredPearsonCor = numeratorPearson.zip(denumPearson).map{ case (num, denum) => pow(num / denum, 2) }
			val meanPearsonCor = squaredPearsonCor.reduce(_ + _) / q

			corYpredVal += meanPearsonCor
*/
/*
			{
				import smile.plot._
				import java.awt.Color
				val testPred = testAndPredRDD.map{ case ((idx, (x, y)), (idx2, (label, ypred))) => (y, ypred, x) }.collect
				val testV = testPred.map(_._1)
				val predV = testPred.map(_._2.toArray)
				val originalV = testPred.map(_._3)
				val labels = Array.fill(testV.size)(0) ++ Array.fill(testV.size)(1) //++ Array.fill(testV.size)(2)
				val testpred = testV ++ predV //++ originalV
				plot(testpred, labels, '*', Array(Color.RED, Color.BLUE, Color.GREEN))

			}
		/* PLot simData */
		import smile.plot._
		import java.awt.Color
		val yTrue = testAndPredRDD.map{ case ((idx, (x, y)), (idx2, (label, ypred))) => y }.collect
		val yPred = testAndPredRDD.map{ case ((idx, (x, y)), (idx2, (label, ypred))) => ypred.toArray }.collect

		val toPlot = yTrue ++ yPred
		val labels = Array.fill(yPred.size)(0) ++ Array.fill(yPred.size)(1)
		plot(toPlot, labels, '*', Array(Color.RED, Color.BLUE))
*/
		/* PLot k500 */
		import smile.plot._
		import java.awt.Color
		val yTrue = testAndPredRDD.map{ case ((idx, (x, y)), (idx2, (label, ypred))) => x ++ y }.collect
		val yPred = testAndPredRDD.map{ case ((idx, (x, y)), (idx2, (label, ypred))) => x ++ ypred.toArray }.collect

		testAndPredRDD.map{ case ((idx, (x, y)), (idx2, (label, ypred))) => label }.distinct.collect.foreach(println)

		val toPlot = yTrue ++ yPred
		val labels = Array.fill(yPred.size)(0) ++ Array.fill(yPred.size)(1)
		plot(toPlot, labels, '*', Array(Color.RED, Color.BLUE))

			sqrmseCal += sqrmseCalIn
			sqrmseVal += sqrmseValIn
		}
//	(sqrmseCal, corYpredCal, sqrmseVal, corYpredVal)	
	(sqrmseCal, Array(8.8), sqrmseVal, Array(8.8), modelingTimeBuf, kmeansTime, predictTimeBuf, idLabelized)	
	}
}
