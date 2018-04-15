package clustering4ever.spark.clustering.clusterwise

import _root_.scala.util.Random
import _root_.scala.collection.mutable.{ArrayBuffer, HashMap}
import _root_.scala.math.{pow, sqrt}
import _root_.clustering4ever.scala.clustering.kmeans.KMeans
import _root_.clustering4ever.util.SumArrays
import _root_.scala.annotation.meta.param
import breeze.linalg.{DenseVector, DenseMatrix}
import org.apache.spark.{SparkContext, SparkConf, HashPartitioner}
import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.clustering._
import org.apache.spark.mllib.linalg.{Vector, Vectors}
import org.apache.spark.broadcast.Broadcast

class Clusterwise(
	@(transient @param) sc: SparkContext,
	val dataXY: Seq[(Int, (Array[Double], Array[Double]))],
	var g: Int,
	var h: Int,
	var nbCV: Int,
	var init: Int,
	var kPredict: Int,
	var standardized: Boolean,
	var sizeBloc: Int,
	var nbMaxAttemps: Int,
	var epsilonKmeans: Double,
	var iterMaxKmeans: Int
	) extends Serializable
{
	type SqRmseCal = Double
	type SqRmseVal = Double

	def run: (Array[(SqRmseCal, SqRmseVal)], Array[ClusterwiseModel]) =
	{
		val n = dataXY.size
		val first = dataXY.head
		val p = first._2._1.size  // dimX
		val q = first._2._2.size  // dimY
		val nbBloc = (n / sizeBloc).toInt
		val clusterwiseModels = ArrayBuffer.empty[ClusterwiseModel]

		def reduceXY(a: (Array[Double], Array[Double]), b: (Array[Double], Array[Double])): (Array[Double], Array[Double]) =
		{
			(SumArrays.sumArraysNumerics(a._1, b._1), SumArrays.sumArraysNumerics(a._2, b._2))
		}

  		val standardizationParameters = if( standardized )
  		{
	  		val (preMeanX, preMeanY) = dataXY.map{ case (_, (x, y)) => (x, y) }.reduce(reduceXY)
	  		val meanX = preMeanX.map(_ / n)
	  		val meanY = preMeanY.map(_ / n)

	  		val (preSDX, preSDY) = dataXY.map{ case (_, (x, y)) => (x.zipWithIndex, y.zipWithIndex) }
	  			.map{ case (x, y) => (x.map{ case (v, idx) =>  pow(v - meanX(idx), 2) }, y.map{ case (v, idx) => pow(v - meanX(idx), 2) }) }
	  			.reduce(reduceXY)
	  		val sdX = preSDX.map( v => sqrt(v / (n - 1)))
	  		val sdY = preSDY.map( v => sqrt(v / (n - 1)))
  			Some((meanX, meanY, sdX, sdY))
  		}
  		else
  		{
  			None
  		}
  		// Center Reduct
  		val centerReductRDD = if( standardized )
  		{
  			val (meanX, meanY, sdX, sdY) = standardizationParameters.get

  			dataXY.map{ case (id, (x, y)) =>
  			(
  				id,
  				(
  					x.zipWithIndex.map{ case (v, idx) => (v - meanX(idx)) / sdX(idx) },
  					y.zipWithIndex.map{ case (v, idx) => (v - meanY(idx)) / sdY(idx) }
  				)
  			)}
  		}
  		else
  		{
  			dataXY
  		}

  	  	val groupedData = if( sizeBloc != 1 )
		{
  	  		val kkmeans = nbBloc
	  	  	val kmData = centerReductRDD.map{ case (_, (x, y)) => x ++ y }
	  	  	val kmeans = new KMeans(kmData, kkmeans, epsilonKmeans, iterMaxKmeans)
	  	  	val kmeansModel = kmeans.run()
  	  		val groupedDataIn = HashMap(centerReductRDD.map{ case (id, (x, y)) => (id, kmeansModel.predict(x ++ y)) }:_*)
  	  		Some(groupedDataIn)
		}
		else
		{
			None
		}

		val splits = scala.util.Random.shuffle(centerReductRDD).grouped((centerReductRDD.size / nbCV) + 1).toArray

		println(splits.map(_.size).mkString(", "))
		
		val trainDSbuff = for( j <- 0 until nbCV ) yield ((for( u <- 0 until nbCV if( u != j )) yield (splits(u))).reduce(_ ++ _).sortBy{ case (id, _) => id })
		val trainDS = trainDSbuff.map(_.toArray)
		
		println("---------------")
		println(trainDS.map(_.size).mkString(", "))
		
		val bcTrainDS = sc.broadcast(trainDS)
		val bcGroupedData = sc.broadcast(groupedData)
		// Launch Meta Reg on each partition
		val resRegOut = sc.parallelize( 1 to 8888, init * nbCV).mapPartitionsWithIndex( (idx, it) =>
		{
			val idxCV = idx % nbCV
			val modelTrain = ArrayBuffer.empty[Array[Array[(Int,(Array[Double], Array[Double],Int))]]]
			val predFitted = ArrayBuffer.empty[Array[Array[(Int, Array[Double])]]]
			val prediction = ArrayBuffer.empty[ArrayBuffer[(Int, Int)]]
			val critReg = ArrayBuffer.empty[Array[Double]]
			val mapsRegCrit = ArrayBuffer.empty[HashMap[Int, Double]]
			val classedReg = ArrayBuffer.empty[Array[(Int, Int)]]
			val coIntercept = ArrayBuffer.empty[Array[Array[Double]]]
			val coXYcoef = ArrayBuffer.empty[Array[Array[Double]]]
			val regClass = new ClusterwiseCore(bcTrainDS.value(idxCV), bcGroupedData.value, h, g, nbBloc, nbMaxAttemps)
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

		val aggregateByCVIdx = resRegOut.groupBy{ case (idxCV, _) => idxCV }.map{ case (idxCV, aggregate) => (idxCV, aggregate.map(_._2)) }
		val regScores0 = aggregateByCVIdx.map{ case (idxCV, aggregate) => (idxCV, aggregate.map{ case (_, bestInitScore, _, _, _) => bestInitScore }) }
		val idxBestRegScoreOut = regScores0.map{ case (idxCV, regScores) => (idxCV, regScores.indexOf(regScores.min)) }.toMap
		val bestModelPerCV = aggregateByCVIdx.map{ case (idxCV, aggregate) => (idxCV, aggregate(idxBestRegScoreOut(idxCV))) }

		/*********************************************************/
		/*  Selection of the results from the best intialization */
		/*********************************************************/

		def computeRmseCalAndVal(idxCv: Int, bestClassifiedDataOut: Array[(Int, Int)], bestCoInterceptOut: Array[Array[Double]], bestCoXYcoefOut: Array[Array[Double]], bestFittedOut: Array[Array[(Int, Array[Double])]]) =
		{
			val mapBestClassifiedDataOut = HashMap(bestClassifiedDataOut:_*)
		
			/***********************************************************************************/
			/* 4. Compute the final G separate multiblock analyses (with the complete dataset) */
			/***********************************************************************************/
			val labeledRDD = bcTrainDS.value(idxCv).map{ case (id, (x, y)) => (mapBestClassifiedDataOut(id), (id, x, y)) }

			val finals = labeledRDD.groupBy{ case (clusterID, (idx, x, y)) => clusterID }.map{ case (clusterID, aggregate) =>
			{
				val ng = aggregate.size
				val lw = 1D / ng
				val tmpBuffer = ArrayBuffer(aggregate:_*)
				val xDs = tmpBuffer.map{ case (_, (idx, x, _)) => (idx, x) }
				val yDs = tmpBuffer.map{ case (_, (_, _, y)) => y }
				val ktabXdudiY = PLS.ktabXdudiY(xDs, yDs, ng)
				val (_, xyCoef, intercept, prediction) = PLS.runFinalMBPLS(xDs, yDs, lw, ng, h, ktabXdudiY)
				(clusterID,( intercept, xyCoef, prediction))
			}}.toMap

			/********************************************************************************************************/
			/* 										Test the model on testing set 									*/
			/********************************************************************************************************/

			val bcLocalTrainData = sc.broadcast(labeledRDD.map{ case (label, (idx, x, y)) => (idx, (x, y, label)) })
			val clusterwiseModel = new ClusterwiseModel(bcLocalTrainData, finals, standardizationParameters)
			clusterwiseModels += clusterwiseModel

			val testY = splits(idxCv).toArray
			val labelAndPrediction = clusterwiseModel.predictKNNLocal(testY, kPredict, g)
			val yPredTrainSort = bestFittedOut.reduce(_ ++ _).toArray.sortBy(_._1)

			/********************************************************************************************************/
			/*										Measure quality of prediction 									*/
			/********************************************************************************************************/
			val trainY = bcTrainDS.value(idxCv).map{ case (_, (_, y)) => y }
		 	val testSize = testY.size

		 	val (meanX, meanY, sdX, sdY) = standardizationParameters.get

		 	val meanTrain = trainY.reduce(SumArrays.sumArraysNumerics(_, _)).map(_ / trainY.size)

		 	val sdYtrain = trainY.map(_.zipWithIndex.map{ case (y, meanIdx) => pow(y - /*meanTrain*/meanY(meanIdx), 2) }).reduce(SumArrays.sumArraysNumerics(_, _)).map(x => sqrt(x / (bcTrainDS.value(idxCv).size - 1)))
		 	
		 	val meanTest = testY.map(_._2._2).reduce(SumArrays.sumArraysNumerics(_, _)).map(_ / testSize)
		 	
		 	val sdYtest = testY.map{ case (_, (_, y)) => y }.map(_.zipWithIndex.map{ case(y, meanIdx) => pow(y - /*meanTest*/meanY(meanIdx), 2) }).reduce(SumArrays.sumArraysNumerics(_, _)).map( x => sqrt(x / (testSize - 1)))

			val sqRmseCalIn = if( q == 1 )
			{
			  	val sqrmse = trainY.zip(yPredTrainSort).map{ case ((trueY, (_, yPred))) => pow(trueY(0) - yPred.head, 2)}.sum / trainY.size / sdYtrain.head
				sqrmse
			}
			else
		 	{
			    val preColSum = trainY.zip(yPredTrainSort).map{ case ((trueY, (_, yPred))) => trueY.zip(yPred).map( x => pow(x._1 - x._2, 2) ) }
			    val colSum = preColSum.reduce(SumArrays.sumArraysNumerics(_, _))
			    val sqRmseYCal = colSum.map( _ / trainY.size ).zip(sdYtrain).map{ case (v, sdy) => v / sdy }
			    val meanSqRmseYCal = sqRmseYCal.sum / sqRmseYCal.size
				meanSqRmseYCal
			}						

			val testAndPredData = testY.zip(labelAndPrediction)

			val sqRmseValIn = if( q == 1 )
			{
				testAndPredData.map{ case ((idx, (x, y)), (idx2, (label, yPred))) => pow(y.head - yPred(0), 2) }.sum / testSize / sdYtrain(0)
			}
			else
			{
				val sqrmseYVal = testAndPredData.map{ case ((idx, (x, y)), (idx2, (label, yPred))) => y.zip(yPred.toArray).map{ case (yTest, yPred) => pow(yTest - yPred, 2) } }
					.reduce(SumArrays.sumArraysNumerics(_, _))
					.zipWithIndex
					.map{ case (value, idx) => value / testSize / sdYtest(idx) }
				val sqRmseYValMean = sqrmseYVal.sum / q
				sqRmseYValMean
			}
			(sqRmseCalIn, sqRmseValIn)
		}

		val rmseCalAndVal = for( (idxCv, (bestClassifiedDataOut, _, bestCoInterceptOut, bestCoXYcoefOut, bestFittedOut)) <- bestModelPerCV.toArray ) yield computeRmseCalAndVal(idxCv, bestClassifiedDataOut, bestCoInterceptOut, bestCoXYcoefOut, bestFittedOut)

		(
			rmseCalAndVal,
			clusterwiseModels.toArray
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
		k: Int,
		standardized: Boolean = true,
		sizeBloc: Int = 1,
		nbMaxAttemps: Int = 30,
		epsilonKmeans: Double = 0.00001,
		iterMaxKmeans: Int = 100
	): (Array[(Double, Double)], Array[ClusterwiseModel]) = 
	{
		val clusterwise = new Clusterwise(sc, dataXY, g, h, nbCV, init, k, standardized, sizeBloc, nbMaxAttemps, epsilonKmeans, iterMaxKmeans)
		clusterwise.run	
	}
}