package clustering4ever.spark.clustering.clusterwise

import org.apache.spark.{SparkContext, SparkConf, HashPartitioner}
import _root_.scala.util.Random
import _root_.scala.collection.mutable.{ArrayBuffer, HashMap}
import _root_.scala.math.{pow, sqrt}
import breeze.linalg.{DenseVector, DenseMatrix}
import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.clustering._
import org.apache.spark.mllib.linalg.{Vector, Vectors}
import _root_.scala.annotation.meta.param
import org.apache.spark.broadcast.Broadcast
import _root_.clustering4ever.scala.clustering.kmeans.KMeans

class Clusterwise(
	@(transient @param) sc: SparkContext,
	val dataXY: Seq[(Int, (Array[Double],Array[Double]))],
	var g: Int,
	var h: Int,
	var nbCV: Int,
	var init: Int,
	var perGroup: Boolean,
	var sizeBloc: Int,
	var trainSize0: Double,
	var k: Int,
	var numberMaxBatch: Int	
) extends Serializable
{
	def run =
	{
		val n = dataXY.size
		val first = dataXY.head
		val q = first._2._2.size  // dimY
		val p = first._2._1.size  // dimX		
		val dp = sc.defaultParallelism

		var nbBloc = (n / sizeBloc).toInt
		val sqrmseCal = ArrayBuffer.empty[Double]
		val corYpredCal = ArrayBuffer.empty[Double]
		val sqrmseVal = ArrayBuffer.empty[Double]
		val corYpredVal = ArrayBuffer.empty[Double]
		val modelingTimeBuf = ArrayBuffer.empty[Double]
		val predictTimeBuf = ArrayBuffer.empty[Double]
		val idLabelized = ArrayBuffer.empty[Array[(Int, Int)]]
		val interceptXYcoefPredByClass = ArrayBuffer.empty[_root_.scala.collection.Map[Int, (Array[Double], breeze.linalg.DenseMatrix[Double], Array[(Int, Array[Double])])]]
		val bcTrainData = ArrayBuffer.empty[Broadcast[Array[(Int, (Array[Double], Array[Double], Int))]]]

		def reduceXY(a: (Array[Double], Array[Double]), b: (Array[Double], Array[Double])) : (Array[Double], Array[Double]) =
			( a._1.zip(b._1).map( x => x._1 + x._2), a._2.zip(b._2).map( x => x._1 + x._2) )


		// Center reduction of dataset
		// (x - mu) / sd
  		val (preMeanX, preMeanY) = dataXY.map{ case (_, (x, y)) => (x, y) }.reduce( reduceXY(_, _) )
  		val meanX = preMeanX.map(_ / n)
  		val meanY = preMeanY.map(_ / n)

  		val (preSDX, preSDY) = dataXY.map{ case (_, (x, y)) => (x.zipWithIndex, y.zipWithIndex) }
  			.map{ case (x, y) => (x.map{ case (v, idx) =>  pow(v - meanX(idx), 2) }, y.map{ case (v, idx) => pow(v - meanX(idx), 2) }) }
  			.reduce(reduceXY)
  		val sdX = preSDX.map( v => sqrt(v / (n - 1)))
  		val sdY = preSDY.map( v => sqrt(v / (n - 1)))

  		val standardizationParameters = (meanX, meanY, sdX, sdY)

  		// Center Reduct
  		val centerReductRDD = if( true )
  		{
  			dataXY.map{ case (id, (x, y)) =>
  			(
  				id,
  				(
  					x.zipWithIndex.map{ case (v, idx) => (v - meanX(idx)) / sdX(idx) },
  					y.zipWithIndex.map{ case (v, idx) => (v - meanY(idx)) / sdY(idx) }
  				)
  			)}
  		}
  		else dataXY

  	  	val groupedData = if( perGroup )
		{
  	  		val kkmeans = nbBloc
	  	  	val kmData = centerReductRDD.map{ case (_, (x, y)) => x ++ y }
	  	  	val epsilon = 0.0001
	  	  	val iterMax = 100
	  	  	val kmeans = new KMeans(kmData, kkmeans, epsilon, iterMax)
	  	  	val kmeansModel = kmeans.run()
  	  		val groupedDataIn = HashMap(centerReductRDD.map{ case (id, (x, y)) => (id, kmeansModel.predict(x ++ y)) }:_*)
  	  		groupedDataIn
		}
		else
		{
			HashMap.empty[Int, Int]
		}

		val splits = scala.util.Random.shuffle(centerReductRDD).grouped(centerReductRDD.size / nbCV).map(_.toArray).toArray

  	  	val dividedInSecond = 1000000000

		val trainDS = for( j <- 0 until nbCV ) yield ((for( u <- 0 until nbCV if( u != j )) yield (splits(u))).reduce(_ ++ _).sortBy{ case (id, _) => id })

		val bcTrainDS = sc.broadcast(trainDS)
		val bcGroupedData = sc.broadcast(groupedData)

	  	val t01 = System.nanoTime

	 // Launch Meta Reg on each partition
		val resRegOut = sc.parallelize( 1 to 88888, init * nbCV).mapPartitionsWithIndex( (idx, it) => it.map( x => idx % nbCV ) ).mapPartitions( it =>
		{

			val idxCV = it.next

			val modelTrain = ArrayBuffer.empty[Array[Array[(Int,(Array[Double], Array[Double],Int))]]]
			val predFitted = ArrayBuffer.empty[Array[Array[(Int, Array[Double])]]]
			val prediction = ArrayBuffer.empty[ArrayBuffer[(Int, Int)]]
			val critReg = ArrayBuffer.empty[Array[Double]]
			val mapsRegCrit = ArrayBuffer.empty[HashMap[Int, Double]]
			val classedReg = ArrayBuffer.empty[Array[(Int, Int)]]
			val coIntercept = ArrayBuffer.empty[Array[Array[Double]]]
			val coXYcoef = ArrayBuffer.empty[Array[Array[Double]]]

			val regClass = new Regression(bcTrainDS.value(idxCV), h, g)(bcGroupedData.value, nbBloc)
			  
		  	//Pour un element
		  	if( ! perGroup )
		  	{
		  		val (_, predFitted0, coIntercept0, coXYcoef0, critReg0, mapsRegCrit0, classedReg0) = regClass.plsPerDot
		  		predFitted += predFitted0
		  		coIntercept += coIntercept0
		  		coXYcoef += coXYcoef0
		  		critReg += critReg0
		  		mapsRegCrit += mapsRegCrit0
		  		classedReg += classedReg0
		  	}
		  	else
		  	{
		  		val (_, predFitted0, coIntercept0, coXYcoef0, critReg0, mapsRegCrit0, classedReg0) = regClass.plsPerGroup
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

		  	Iterator((idxCV, (bestClassifiedData, bestInitScore, bestCoInterceptIn, bestCoXYcoefIn, bestFitted)))

  		}).collect

		val grouped0 = resRegOut.groupBy{ case (idxCV, _) => idxCV }.map{ case (idxCV, rest) => (idxCV, rest.map(_._2)) }
		val regScores0 = grouped0.map{ case (idxCV, rest) => (idxCV, rest.map{ case (_, bestInitScore, _, _, _) => bestInitScore }) }
		val idxBestRegScoreOut = regScores0.map{ case (idxCV, regScores) => (idxCV, regScores.indexOf(regScores.min)) }.toMap
		val bestModelPerCV = grouped0.map{ case (idxCV, rest) => (idxCV, rest(idxBestRegScoreOut(idxCV))) }
		

		/*********************************************************/
		/*  Selection of the results from the best intialization */
		/*********************************************************/

		for( (idxCv, (bestClassifiedDataOut, _, bestCoInterceptOut, bestCoXYcoefOut, bestFittedOut)) <- bestModelPerCV )
		{
			val mapBestClassifiedDataOut = HashMap(bestClassifiedDataOut:_*)
		
			/***********************************************************************************/
			/* 4. Compute the final G separate multiblock analyses (with the complete dataset) */
			/***********************************************************************************/

			val labeledRDD = sc.parallelize(bcTrainDS.value(idxCv)).map{ case (id, (x, y)) => (mapBestClassifiedDataOut(id), (id, x, y)) }
			//.map{ case (idx, x, y, label) => (label, (idx, x, y))}.cache


			// Keep labeled data
			idLabelized += labeledRDD.map{ case (label, (id, _, _)) => (id, label) }.sortBy(_._1).collect

			val finals = labeledRDD.partitionBy( new HashPartitioner(g) ).mapPartitions( it => {
					val dataXYg = it.toArray.groupBy{ case (label, (idx, x, y)) => label }
					//val classRegression = for( (label2, array2) <- dataXYg ) yield(
					val classRegression = dataXYg.map{ case (label2, array2) =>
					{
						val ng = array2.size
						val lw = 1D / ng
						val tmpBuffer = ArrayBuffer(array2:_*)
						val _X = tmpBuffer.map{ case (_, (idx, x, _)) => (idx, x) }
						val _Y = tmpBuffer.map{ case (_, (_, _, y)) => y }
						val ktabXdudiY = PLS.ktabXdudiY(_X, _Y, ng)
						val (_, _XYcoef, _Intercept, _Pred) = PLS.runFinalMBPLS(_X, _Y, lw, ng, h, ktabXdudiY)

						(label2,( _Intercept, _XYcoef, _Pred))
					}}
					classRegression.toIterator
				}).collectAsMap//.sortBy{ case (label, _, _, _) => label }


			val t02 = System.nanoTime
			val modelingTime = (t02 - t01).toDouble / dividedInSecond
			modelingTimeBuf += modelingTime

			println("Modeling : "+ modelingTime + " s")

			/********************************************************************************************************/
			/* 										Test the model on testing set 									*/
			/********************************************************************************************************/

			val ttest1 = System.nanoTime

			val bcLocalTrainData = sc.broadcast( labeledRDD.map{ case (label, (idx, x, y)) => (idx, (x, y, label)) }.collect )

			val prediction = new Prediction(bcLocalTrainData, splits(idxCv), k, g)()

			//val labelAndPrediction = prediction.cwPredictionKNNdistributed(tabHash, nbBucketLSH, lshPred, finals)
			val labelAndPrediction = PredictionSimplified.cwPredictionKNNdistributed(bcLocalTrainData, splits(idxCv), k, g, finals)

			val yPredTrainSort = bestFittedOut.reduce(_ ++ _).toArray.sortBy(_._1)

			val ttest2 = System.nanoTime
			val predictTime = (ttest2 - ttest1).toDouble / dividedInSecond
			predictTimeBuf += predictTime
			/********************************************************************************************************/
			/*										Measure quality of prediction 									*/
			/********************************************************************************************************/

			val trainY = bcTrainDS.value(idxCv).map{ case (_, (_, y)) => y }
		 	val sdYtrain = trainY.map(_.zipWithIndex).map(_.map( y => pow(y._1 - meanY(y._2), 2))).reduce(_.zip(_).map( x => (x._1 + x._2))).map(x => sqrt(x / (bcTrainDS.value(idxCv).size - 1)))
		 	val testSet = splits(idxCv).cache
		 	val testSize = testSet.count
		 	val sdYtest = testSet.map{ case (_, (_, y)) => y }.map(_.zipWithIndex).map( _.map{ case(y, meanId) => pow(y - meanY(meanId), 2) }).reduce(_.zip(_).map( x => x._1 + x._2 )).map( x => sqrt(x / (testSize - 1)))

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
				mean_sqrmse_y_cal
			}						

			val testAndPredRDD = splits(idxCv).zip(labelAndPrediction.repartitionAndSortWithinPartitions(new HashPartitioner(dp))).cache


			// Check veracity of values
			val denormalizeData = testAndPredRDD.map{ case ((idx, (x, y)), (idx2, (label, ypred))) => (y, ypred.toArray) }
				.map{ case (y, ypred) => y.zip(ypred).zipWithIndex.map{ case ((yy, yypred), idx) => ( (yy * sdY(idx)) + meanY(idx), (yypred * sdY(idx)) + meanY(idx) ) }.head }
				//.map{ case (yy, yypred) => (if( yy == 1D ) 1 else 0, if( yypred >= 0.5 ) 1 else 0) }
				//.mkString(", ") }
				//((y.head * sdY.head) + meanY.head, (ypred.head * sdY.head) + meanY.head) }
				//.collect
				//.foreach(println)

			val sqrmseValoo = if( q == 1 ) denormalizeData.map{ case (y, ypred) => pow(y - ypred, 2) }.reduce(_ + _) / testSize / sdYtrain(0)
			println("sqrmseValoo : " + sqrmseValoo)

			val sqrmseValIn = if( q == 1 ) testAndPredRDD.map{ case ((idx, (x, y)), (idx2, (label, ypred))) => pow(y(0) - ypred(0), 2) }.reduce(_ + _) / testSize / sdYtrain(0)
			else
			{
				val sqrmseYVal = testAndPredRDD.map{ case ((idx, (x, y)), (idx2, (label, ypred))) => y.zip(ypred.toArray).map{ case (yTest, yPred) => pow(yTest - yPred, 2) } }
					.reduce( _.zip(_).map( x => x._1 + x._2 ) )
					.zipWithIndex
					.map{ case (value, idx) => value / testSize / sdYtest(idx) }
				val sqrmse_y_val_mean = sqrmseYVal.reduce(_ + _) / q
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
		/*
		import smile.plot._
		import java.awt.Color
		val yTrue = testAndPredRDD.map{ case ((idx, (x, y)), (idx2, (label, ypred))) => x ++ y }.collect
		val yPred = testAndPredRDD.map{ case ((idx, (x, y)), (idx2, (label, ypred))) => x ++ ypred.toArray }.collect

		testAndPredRDD.map{ case ((idx, (x, y)), (idx2, (label, ypred))) => label }.distinct.collect.foreach(println)

		val toPlot = yTrue ++ yPred
		val labels = Array.fill(yPred.size)(0) ++ Array.fill(yPred.size)(1)
		plot(toPlot, labels, '*', Array(Color.RED, Color.BLUE))
		*/
		sqrmseCal += sqrmseCalIn
		sqrmseVal += sqrmseValIn
		interceptXYcoefPredByClass += finals
		bcTrainData += bcLocalTrainData
	}

//	(sqrmseCal, corYpredCal, sqrmseVal, corYpredVal)	
	(
		sqrmseCal, 
		Array(8.8), 
		sqrmseVal, 
		Array(8.8), 
		modelingTimeBuf, 
		kmeansTime, 
		predictTimeBuf, 
		idLabelized, 
		interceptXYcoefPredByClass,
		standardizationParameters,
		centerReductRDD,
		bcTrainData
	)

	}
}


object Clusterwise extends ClusterwiseTypes with Serializable
{
	/**
	 *	Return 
	 *
	 *
	 */
	def run(
		@(transient @param) sc: SparkContext,
		dataXY: RDD[(ID, (Xvector, Yvector))],
		g: Int,
		h: Int,
		nbCV: Int,
		init: Int,
		perGroup: Boolean,
		sizeBloc: Int,
		nbBucketLSH: Int,
		lshPred: Boolean,
		trainSize: Double,
		k: Int,
		numberMaxBatch: Int
	) = 
	{
		val clusterwise = new Clusterwise(sc, dataXY, g, h, nbCV, init, perGroup, sizeBloc, nbBucketLSH, lshPred, trainSize, k, numberMaxBatch)
		clusterwise.run	
	}
}