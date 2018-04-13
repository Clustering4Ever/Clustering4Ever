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
	var trainSize0: Double,
	var k: Int,
	var standardized: Boolean,
	var sizeBloc: Int,
	var nbMaxAttemps: Int
) extends Serializable
{
	def run: (Array[Double], Array[Double], Array[ClusterwiseModel], (Array[Double], Array[Double], Array[Double], Array[Double])) =
	{
		val n = dataXY.size
		val first = dataXY.head
		val q = first._2._2.size  // dimY
		val p = first._2._1.size  // dimX		
		val dp = sc.defaultParallelism

		var nbBloc = (n / sizeBloc).toInt
		val sqRmseCal = ArrayBuffer.empty[Double]
		val sqRmseVal = ArrayBuffer.empty[Double]
		val clusterwiseModels = ArrayBuffer.empty[ClusterwiseModel]

		def reduceXY(a: (Array[Double], Array[Double]), b: (Array[Double], Array[Double])): (Array[Double], Array[Double]) =
		{
			( a._1.zip(b._1).map( x => x._1 + x._2), a._2.zip(b._2).map( x => x._1 + x._2) )
		}


		// Center reduction of dataset
  		val (preMeanX, preMeanY) = dataXY.map{ case (_, (x, y)) => (x, y) }.reduce(reduceXY)
  		val meanX = preMeanX.map(_ / n)
  		val meanY = preMeanY.map(_ / n)

  		val (preSDX, preSDY) = dataXY.map{ case (_, (x, y)) => (x.zipWithIndex, y.zipWithIndex) }
  			.map{ case (x, y) => (x.map{ case (v, idx) =>  pow(v - meanX(idx), 2) }, y.map{ case (v, idx) => pow(v - meanX(idx), 2) }) }
  			.reduce(reduceXY)
  		val sdX = preSDX.map( v => sqrt(v / (n - 1)))
  		val sdY = preSDY.map( v => sqrt(v / (n - 1)))

  		val standardizationParameters = (meanX, meanY, sdX, sdY)

  		// Center Reduct
  		val centerReductRDD = if( standardized )
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

  	  	val groupedData = if( sizeBloc != 1 )
		{
  	  		val kkmeans = nbBloc
	  	  	val kmData = centerReductRDD.map{ case (_, (x, y)) => x ++ y }
	  	  	val epsilon = 0.0001
	  	  	val iterMax = 100
	  	  	val kmeans = new KMeans(kmData, kkmeans, epsilon, iterMax)
	  	  	val kmeansModel = kmeans.run()
  	  		val groupedDataIn = HashMap(centerReductRDD.map{ case (id, (x, y)) => (id, kmeansModel.predict(x ++ y)) }:_*)
  	  		Some(groupedDataIn)
		}
		else
		{
			None
		}

		val splits = scala.util.Random.shuffle(centerReductRDD).grouped(centerReductRDD.size / nbCV).map(_.toArray).toArray

		val trainDS = for( j <- 0 until nbCV ) yield ((for( u <- 0 until nbCV if( u != j )) yield (splits(u))).reduce(_ ++ _).sortBy{ case (id, _) => id })

		val bcTrainDS = sc.broadcast(trainDS)
		val bcGroupedData = sc.broadcast(groupedData)

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

			val regClass = new Regression(bcTrainDS.value(idxCV), h, g)(bcGroupedData.value, nbBloc, nbMaxAttemps)
			  
		  	// Per one element
		  	if( sizeBloc == 1 )
		  	{
		  		val (_, predFitted0, coIntercept0, coXYcoef0, critReg0, mapsRegCrit0, classedReg0) = regClass.plsPerDot()
		  		predFitted += predFitted0
		  		coIntercept += coIntercept0
		  		coXYcoef += coXYcoef0
		  		critReg += critReg0
		  		mapsRegCrit += mapsRegCrit0
		  		classedReg += classedReg0
		  	}
		  	else
		  	{
		  		val (_, predFitted0, coIntercept0, coXYcoef0, critReg0, mapsRegCrit0, classedReg0) = regClass.plsPerGroup()
		  		predFitted += predFitted0
		  		coIntercept += coIntercept0
		  		coXYcoef += coXYcoef0
		  		critReg += critReg0
		  		mapsRegCrit += mapsRegCrit0
		  		classedReg += classedReg0
		  	}
			// Comparison of the predicted X.train and Y.train (standardized rmse, and cv-r2)
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

			val finals = labeledRDD.partitionBy( new HashPartitioner(g) ).mapPartitions( it => {
					val dataXYg = it.toArray.groupBy{ case (label, (idx, x, y)) => label }
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
				}).collectAsMap

			/********************************************************************************************************/
			/* 										Test the model on testing set 									*/
			/********************************************************************************************************/

			val bcLocalTrainData = sc.broadcast( labeledRDD.map{ case (label, (idx, x, y)) => (idx, (x, y, label)) }.collect )
			val clusterwiseModel = new ClusterwiseModel(bcLocalTrainData, finals)
			clusterwiseModels += clusterwiseModel

			val labelAndPrediction = clusterwiseModel.predictKNNLocal(splits(idxCv), k, g)

			val yPredTrainSort = bestFittedOut.reduce(_ ++ _).toArray.sortBy(_._1)

			/********************************************************************************************************/
			/*										Measure quality of prediction 									*/
			/********************************************************************************************************/
			val trainY = bcTrainDS.value(idxCv).map{ case (_, (_, y)) => y }
		 	val testSize = splits(idxCv).size

		 	val sdYtrain = trainY.map(_.zipWithIndex.map{ case (y, meanIdx) => pow(y - meanY(meanIdx), 2) }).reduce(_.zip(_).map( x => (x._1 + x._2))).map(x => sqrt(x / (bcTrainDS.value(idxCv).size - 1)))
		 	val sdYtest = splits(idxCv).map{ case (_, (_, y)) => y }.map(_.zipWithIndex.map{ case(y, meanIdx) => pow(y - meanY(meanIdx), 2) }).reduce(_.zip(_).map( x => x._1 + x._2 )).map( x => sqrt(x / (testSize - 1)))

			val sqRmseCalIn = if( q == 1 )
			{
			  	val sqrmse = trainY.zip(yPredTrainSort).map{ case ((trueY, (_, yPred))) => pow(trueY(0) - yPred.head, 2)}.sum / trainY.size / sdYtrain.head
				sqrmse
			}
			else
		 	{
			 	val meanY = trainY.reduce(_.zip(_).map( x => x._1 + x._2 )).map(_ / n)
			    val preColSum = trainY.zip(yPredTrainSort).map{ case ((trueY, (_, yPred))) => trueY.zip(yPred).map( x => x._1 - x._2 ).map(pow(_, 2)) }
			    val colSum = preColSum.reduce(_.zip(_).map( x => x._1 + x._2 ))
			    val sqRmseYCal = colSum.map( _ / trainY.size ).zip(sdYtrain).map{ case (v, sdy) => v / sdy }
			    val meanSqRmseYCal = sqRmseYCal.sum / sqRmseYCal.size
				meanSqRmseYCal
			}						

			val testAndPredData = splits(idxCv).zip(labelAndPrediction)

			val sqRmseValIn = if( q == 1 )
			{
				testAndPredData.map{ case ((idx, (x, y)), (idx2, (label, yPred))) => pow(y.head - yPred(0), 2) }.sum / testSize / sdYtrain(0)
			}
			else
			{
				val sqrmseYVal = testAndPredData.map{ case ((idx, (x, y)), (idx2, (label, yPred))) => y.zip(yPred.toArray).map{ case (yTest, yPred) => pow(yTest - yPred, 2) } }
					.reduce( _.zip(_).map( x => x._1 + x._2 ) )
					.zipWithIndex
					.map{ case (value, idx) => value / testSize / sdYtest(idx) }
				val sqRmseYValMean = sqrmseYVal.sum / q
				sqRmseYValMean
			}
			sqRmseCal += sqRmseCalIn
			sqRmseVal += sqRmseValIn
		}

		(
			sqRmseCal.toArray, 
			sqRmseVal.toArray,
			clusterwiseModels.toArray,
			standardizationParameters
		)

	}
}


object Clusterwise extends ClusterwiseTypes with Serializable
{
	/**
	 *
	 *
	 */
	def run(
		@(transient @param) sc: SparkContext,
		dataXY: Seq[(ID, (Xvector, Yvector))],
		g: Int,
		h: Int,
		nbCV: Int,
		init: Int,
		trainSize: Double,
		k: Int,
		standardized: Boolean = true,
		sizeBloc: Int = 1,
		nbMaxAttemps: Int = 30
	): (Array[Double], Array[Double], Array[ClusterwiseModel], (Array[Double], Array[Double], Array[Double], Array[Double])) = 
	{
		val clusterwise = new Clusterwise(sc, dataXY, g, h, nbCV, init, trainSize, k, standardized, sizeBloc, nbMaxAttemps)
		clusterwise.run	
	}
}