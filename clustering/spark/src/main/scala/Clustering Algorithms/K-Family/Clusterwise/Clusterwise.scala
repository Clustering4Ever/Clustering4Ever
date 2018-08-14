package clustering4ever.spark.clustering.clusterwise

import scala.util.Random
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.collection.immutable
import scala.math.{pow, sqrt}
import scala.annotation.meta.param
import breeze.linalg.{DenseVector, DenseMatrix}
import org.apache.spark.{SparkContext, SparkConf, HashPartitioner}
import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.clustering._
import org.apache.spark.mllib.linalg.{Vector, Vectors}
import org.apache.spark.broadcast.Broadcast
import clustering4ever.scala.clustering.kmeans.KMeans
import clustering4ever.util.SumArrays
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.util.GenerateDatasets

class Clusterwise(
	@(transient @param) sc: SparkContext,
	val dataXY: immutable.Seq[(Int, (immutable.Seq[Double], immutable.Seq[Double]))],
	var g: Int,
	var h: Int,
	var nbCV: Int,
	var init: Int,
	var kPredict: Int,
	var standardized: Boolean,
	var sizeBloc: Int,
	var nbMaxAttemps: Int,
	var epsilonKmeans: Double,
	var iterMaxKmeans: Int,
	logOn: Boolean = false
	) extends Serializable
{
	type SqRmseCal = Double
	type SqRmseVal = Double

	def run: (immutable.Seq[(SqRmseCal, SqRmseVal)], immutable.Seq[ClusterwiseModel]) =
	{
		val n = dataXY.size
		val first = dataXY.head
		val p = first._2._1.size  // dimX
		val q = first._2._2.size  // dimY
		val kmeansKValue = (n / sizeBloc).toInt
		val clusterwiseModels = ArrayBuffer.empty[ClusterwiseModel]

		def reduceXY(a: (immutable.Seq[Double], immutable.Seq[Double]), b: (immutable.Seq[Double], immutable.Seq[Double])): (immutable.Seq[Double], immutable.Seq[Double]) =
		{
			(SumArrays.sumArraysNumerics[Double](a._1, b._1), SumArrays.sumArraysNumerics[Double](a._2, b._2))
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

  	  	val microClusterByIdAndNumbers = if( sizeBloc != 1 )
		{
	  	  	val kmData = centerReductRDD.map{ case (id, (x, y)) => GenerateDatasets.obtainSimpleRealClusterizable(id, x ++ y) }
	  	  	val kmeansModel = KMeans.run(kmData, kmeansKValue, epsilonKmeans, iterMaxKmeans, new Euclidean(true))
	  	  	val unregularClusterIdsByStandardClusterIDs = kmeansModel.centers.keys.zipWithIndex.toMap
	  	  	val microClusterNumbers = kmeansModel.centers.size
	  	  	val clusterizedData = centerReductRDD.map{ case (id, (x, y)) => (id, unregularClusterIdsByStandardClusterIDs(kmeansModel.centerPredict(x ++ y))) }
  	  		val microClusterByIdIn = immutable.HashMap(clusterizedData:_*)
  	  		Some((microClusterByIdIn, microClusterNumbers))
		}
		else
		{
			None
		}

		val splits = scala.util.Random.shuffle(centerReductRDD).grouped((centerReductRDD.size / nbCV) + 1).toVector
		val trainDS = for( j <- 0 until nbCV ) yield ((for( u <- 0 until nbCV if( u != j )) yield splits(u)).flatten.sortBy{ case (id, _) => id }).toVector
		val broadcastedTrainData = sc.broadcast(trainDS)
		val broadcastedMicroClusterByIdAndNumbers = sc.broadcast(microClusterByIdAndNumbers)
		// Launch Meta Reg on each partition
		val resRegOut = sc.parallelize( 1 to 8888, init * nbCV).mapPartitionsWithIndex( (idx, it) =>
		{
			val idxCV = idx % nbCV
			val predFittedBuff = ArrayBuffer.empty[immutable.Seq[immutable.Seq[(Int, immutable.Seq[Double])]]]
			val critRegBuff = ArrayBuffer.empty[immutable.Seq[Double]]
			val mapsRegCritBuff = ArrayBuffer.empty[HashMap[Int, Double]]
			val classedRegBuff = ArrayBuffer.empty[immutable.Seq[(Int, Int)]]
			val coInterceptBuff = ArrayBuffer.empty[immutable.Seq[Array[Double]]]
			val coXYcoefBuff = ArrayBuffer.empty[immutable.Seq[immutable.Seq[Double]]]
		  	// Clusterwise
		  	if( sizeBloc == 1 )
		  	{
		  		val (_, predFitted, coIntercept, coXYcoef, critReg, mapsRegCrit, classedReg) = ClusterwiseCore.plsPerDot(broadcastedTrainData.value(idxCV), h, g, nbMaxAttemps, logOn)
		  		predFittedBuff += predFitted
		  		coInterceptBuff += coIntercept
		  		coXYcoefBuff += coXYcoef
		  		critRegBuff += critReg
		  		mapsRegCritBuff += mapsRegCrit
		  		classedRegBuff += classedReg
		  	}
		  	// Clusterwise mb
		  	else
		  	{
		  		val (_, predFitted, coIntercept, coXYcoef, critReg, mapsRegCrit, classedReg) = ClusterwiseCore.plsPerMicroClusters(broadcastedTrainData.value(idxCV), broadcastedMicroClusterByIdAndNumbers.value.get._1, h, g, broadcastedMicroClusterByIdAndNumbers.value.get._2, nbMaxAttemps, logOn)
		  		predFittedBuff += predFitted
		  		coInterceptBuff += coIntercept
		  		coXYcoefBuff += coXYcoef
		  		critRegBuff += critReg
		  		mapsRegCritBuff += mapsRegCrit
		  		classedRegBuff += classedReg
		  	}
			// Comparison of the predicted X.train and Y.train (standardized rmse, and cv-r2)
			val minRegCritPerInit = mapsRegCritBuff.map(_.values.min)
			val bestInitScore = minRegCritPerInit.min
			val idxBestInit = minRegCritPerInit.indexOf(bestInitScore)
			val bestClassifiedData = classedRegBuff(idxBestInit)
			val bestCoInterceptIn = coInterceptBuff(idxBestInit)
			val bestCoXYcoefIn = coXYcoefBuff(idxBestInit)
			val bestFitted = predFittedBuff(idxBestInit)

		  	Iterator((idxCV, (bestClassifiedData, bestInitScore, bestCoInterceptIn, bestCoXYcoefIn, bestFitted)))
  		}).collect

		val aggregateByCVIdx = resRegOut.groupBy{ case (idxCV, _) => idxCV }.map{ case (idxCV, aggregate) => (idxCV, aggregate.map(_._2)) }
		val regScores0 = aggregateByCVIdx.map{ case (idxCV, aggregate) => (idxCV, aggregate.map{ case (_, bestInitScore, _, _, _) => bestInitScore }) }
		val idxBestRegScoreOut = regScores0.map{ case (idxCV, regScores) => (idxCV, regScores.indexOf(regScores.min)) }.toMap
		val bestModelPerCV = aggregateByCVIdx.map{ case (idxCV, aggregate) => (idxCV, aggregate(idxBestRegScoreOut(idxCV))) }

		/*********************************************************/
		/*  Selection of the results from the best intialization */
		/*********************************************************/

		def computeRmseCalAndVal(idxCV: Int, bestClassifiedDataOut: immutable.Seq[(Int, Int)], bestCoInterceptOut: immutable.Seq[Array[Double]], bestCoXYcoefOut: immutable.Seq[immutable.Seq[Double]], bestFittedOut: immutable.Seq[immutable.Seq[(Int, immutable.Seq[Double])]]) =
		{
			val mapBestClassifiedDataOut = HashMap(bestClassifiedDataOut:_*)
		
			/***********************************************************************************/
			/* 4. Compute the final G separate multiblock analyses (with the complete dataset) */
			/***********************************************************************************/
			val labeledRDD = broadcastedTrainData.value(idxCV).map{ case (id, (x, y)) => (mapBestClassifiedDataOut(id), (id, x, y)) }

			val modelByCluster = labeledRDD.groupBy{ case (clusterID, (idx, x, y)) => clusterID }.map{ case (clusterID, aggregate) =>
			{
				val ng = aggregate.size
				val lw = 1D / ng
				val tmpBuffer = ArrayBuffer(aggregate:_*)
				val xDs = tmpBuffer.map{ case (_, (idx, x, _)) => (idx, x) }
				val yDs = tmpBuffer.map{ case (_, (_, _, y)) => y }
				val ktabXdudiY = PLS.ktabXdudiY(xDs, yDs, ng)
				val (_, xyCoef, intercept, prediction) = PLS.runFinalPLS(xDs, yDs, lw, ng, h, ktabXdudiY)
				(clusterID, (intercept, xyCoef, prediction))
			}}.toMap

			/********************************************************************************************************/
			/* 										Test the model on testing set 									*/
			/********************************************************************************************************/
			val trainedData = labeledRDD.map{ case (label, (idx, x, y)) => (idx, (x, y, label)) }
			val clusterwiseModel = new ClusterwiseModel(trainedData, modelByCluster, standardizationParameters)
			clusterwiseModels += clusterwiseModel

			val testY = splits(idxCV)//.map{ case (id, (x, y)) => (id, (x.toArray, y)) }
			val labelAndPrediction = clusterwiseModel.predictClusterViaKNNLocal(testY, kPredict, g)
			val yPredTrainSort = bestFittedOut.flatten.sortBy(_._1)

			/********************************************************************************************************/
			/*										Measure quality of prediction 									*/
			/********************************************************************************************************/
			val trainY = broadcastedTrainData.value(idxCV).sortBy{ case (id, _) => id }.map{ case (_, (_, y)) => y }
		 	val testSize = testY.size

		 	val (meanX, meanY, sdX, sdY) = standardizationParameters.get

		 	val meanTrain = trainY.reduce(SumArrays.sumArraysNumerics[Double](_, _)).map(_ / trainY.size)

		 	val sdYtrain = trainY.map(_.zipWithIndex.map{ case (y, meanIdx) => pow(y - meanTrain(meanIdx), 2) }).reduce(SumArrays.sumArraysNumerics[Double](_, _)).map( x => sqrt(x / (broadcastedTrainData.value(idxCV).size - 1)) )
		 	
		 	val meanTest = testY.map(_._2._2).reduce(SumArrays.sumArraysNumerics[Double](_, _)).map(_ / testSize)
		 	
		 	val sdYtest = testY.map{ case (_, (_, y)) => y }.map(_.zipWithIndex.map{ case(y, meanIdx) => pow(y - meanTest(meanIdx), 2) }).reduce(SumArrays.sumArraysNumerics[Double](_, _)).map( x => sqrt(x / (testSize - 1)))

		 	// Standardized RMSE of train data
			val sqRmseCalIn = if( q == 1 )
			{
			  	val sqrmse = trainY.zip(yPredTrainSort).map{ case ((trueY, (_, yPred))) => pow(trueY(0) - yPred.head, 2)}.sum / trainY.size / sdYtrain.head
				sqrmse
			}
			else
		 	{
			    val meanSqRmseYCal = trainY.zip(yPredTrainSort).map{ case ((trueY, (_, yPred))) => trueY.zip(yPred).map( x => pow(x._1 - x._2, 2) ) }
			    	.reduce(SumArrays.sumArraysNumerics[Double](_, _))
			    	.map( _ / trainY.size )
			    	.zip(sdYtrain)
			    	.map{ case (rmseTrain, sdy) => rmseTrain / sdy }
			    	.sum / q
				meanSqRmseYCal
			}						

			val testAndPredData = testY.zip(labelAndPrediction)
		 	// Standardized RMSE of test data
			val sqRmseValIn = if( q == 1 )
			{
				testAndPredData.map{ case ((idx, (x, y)), (idx2, (label, yPred))) => pow(y.head - yPred(0), 2) }.sum / testSize / sdYtest.head
			}
			else
			{
				val sqRmseYValMean = testAndPredData.map{ case ((idx, (x, y)), (idx2, (label, yPred))) => y.zip(yPred.toArray).map{ case (yTest, yPred) => pow(yTest - yPred, 2) } }
					.reduce(SumArrays.sumArraysNumerics[Double](_, _))
					.zip(sdYtest)
					.map{ case (rmseTest, sdTest) => rmseTest / sdTest }
					.sum / q
				sqRmseYValMean
			}
			(sqRmseCalIn, sqRmseValIn)
		}

		val rmseCalAndVal = for( (idxCV, (bestClassifiedDataOut, _, bestCoInterceptOut, bestCoXYcoefOut, bestFittedOut)) <- bestModelPerCV.toVector ) yield computeRmseCalAndVal(idxCV, bestClassifiedDataOut, bestCoInterceptOut, bestCoXYcoefOut, bestFittedOut)

		(
			rmseCalAndVal,
			clusterwiseModels.toVector
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
		dataXY: immutable.Seq[(ID, (Xvector, Yvector))],
		g: Int,
		h: Int,
		nbCV: Int,
		init: Int,
		k: Int,
		standardized: Boolean = true,
		sizeBloc: Int = 1,
		nbMaxAttemps: Int = 30,
		epsilonKmeans: Double = 0.00001,
		iterMaxKmeans: Int = 100,
		logOn: Boolean = false
	): (immutable.Seq[(Double, Double)], immutable.Seq[ClusterwiseModel]) = 
	{
		val clusterwise = new Clusterwise(sc, dataXY, g, h, nbCV, init, k, standardized, sizeBloc, nbMaxAttemps, epsilonKmeans, iterMaxKmeans, logOn)
		clusterwise.run	
	}
}