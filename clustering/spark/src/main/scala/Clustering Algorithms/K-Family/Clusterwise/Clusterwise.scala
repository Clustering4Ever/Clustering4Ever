package clustering4ever.spark.clustering.clusterwise

import scala.util.Random
import scala.reflect.ClassTag
import scala.collection.{mutable, immutable, GenSeq}
import scala.math.{pow, sqrt}
import scala.annotation.meta.param
import org.apache.spark.{SparkContext, SparkConf, HashPartitioner}
import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.clustering._
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.broadcast.Broadcast
import clustering4ever.scala.clustering.kmeans.KMeans
import clustering4ever.util.SumVectors
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.util.GenerateClusterizable
import clustering4ever.scala.clusterizables.RealClusterizable

class Clusterwise[S <: Seq[Double] : ClassTag](
	@(transient @param) sc: SparkContext,
	val dataXY: GenSeq[(Int, (S, S))],
	var g: Int,
	var h: Int,
	var nbCV: Int,
	var init: Int,
	var kPredict: Int,
	var withY: Boolean,
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

	def run: (Array[(SqRmseCal, SqRmseVal)], Array[ClusterwiseModel[S]]) =
	{
		val n = dataXY.size
		val first = dataXY.head
		val p = first._2._1.size  // dimX
		val q = first._2._2.size  // dimY
		val kmeansKValue = (n / sizeBloc).toInt
		val clusterwiseModels = mutable.ArrayBuffer.empty[ClusterwiseModel[S]]

		def reduceXY(a: (S, S), b: (S, S)): (S, S) = (SumVectors.sumVectors[Double, S](a._1, b._1).asInstanceOf[S], SumVectors.sumVectors[Double, S](a._2, b._2).asInstanceOf[S])

  		val standardizationParameters = if( standardized )
  		{
	  		val (preMeanX, preMeanY) = dataXY.map{ case (_, (x, y)) => (x, y) }.reduce(reduceXY)
	  		val meanX = preMeanX.map(_ / n)
	  		val meanY = preMeanY.map(_ / n)

	  		val (preSDX, preSDY) = dataXY.map{ case (_, (x, y)) => (x.zipWithIndex, y.zipWithIndex) }
	  			.map{ case (x, y) => (x.map{ case (v, idx) =>  pow(v - meanX(idx), 2) }.asInstanceOf[S], y.map{ case (v, idx) => pow(v - meanX(idx), 2) }.asInstanceOf[S]) }
	  			.reduce(reduceXY)
	  		val sdX = preSDX.map( v => sqrt(v / (n - 1)))
	  		val sdY = preSDY.map( v => sqrt(v / (n - 1)))
  			Some((mutable.ArrayBuffer(meanX:_*), mutable.ArrayBuffer(meanY:_*), mutable.ArrayBuffer(sdX:_*), mutable.ArrayBuffer(sdY:_*)))
  		}
  		else None

  		// Center Reduct
  		val centerReductRDD = if( standardized )
  		{
  			val (meanX, meanY, sdX, sdY) = standardizationParameters.get

  			dataXY.map{ case (id, (x, y)) =>
  			(
  				id,
  				(
  					x.zipWithIndex.map{ case (v, idx) => (v - meanX(idx)) / sdX(idx) }.asInstanceOf[S],
  					y.zipWithIndex.map{ case (v, idx) => (v - meanY(idx)) / sdY(idx) }.asInstanceOf[S]
  				)
  			)}
  		}
  		else dataXY

  	  	val microClusterByIdAndNumbers = if( sizeBloc != 1 )
		{
	  	  	val kmData = centerReductRDD.map{ case (id, (x, y)) => GenerateClusterizable.obtainSimpleRealClusterizable[Int, S](id, (x ++ y).asInstanceOf[S]) }
	  	  	val kmeansModel = KMeans.run[Int, S, S, RealClusterizable[Int, S, S], Euclidean[S]](kmData, kmeansKValue, epsilonKmeans, iterMaxKmeans, new Euclidean[S](squareRoot = true))
	  	  	val unregularClusterIdsByStandardClusterIDs = kmeansModel.centers.keys.zipWithIndex.toMap
	  	  	val microClusterNumbers = kmeansModel.centers.size
	  	  	val clusterizedData = centerReductRDD.map{ case (id, (x, y)) => (id, unregularClusterIdsByStandardClusterIDs(kmeansModel.centerPredict((x ++ y).asInstanceOf[S]))) }.seq
  	  		val microClusterByIdIn = immutable.HashMap(clusterizedData:_*)
  	  		Some((microClusterByIdIn, microClusterNumbers))
		}
		else None

		val splits = scala.util.Random.shuffle(centerReductRDD.seq).grouped((centerReductRDD.size / nbCV) + 1).map(_.par).toArray
		val rangeCV = (0 until nbCV).par
		val trainDS = rangeCV.map( j => rangeCV.collect{ case u if( u != j ) => splits(u) }.flatten.seq.sortBy{ case (id, _) => id }.par )
		val broadcastedTrainData = sc.broadcast(trainDS)
		val broadcastedMicroClusterByIdAndNumbers = sc.broadcast(microClusterByIdAndNumbers)
		// Launch Meta Reg on each partition
		val resRegOut = sc.parallelize( 1 to 8888, init * nbCV).mapPartitionsWithIndex( (idx, it) =>
		{
			val idxCV = idx % nbCV
			val predFittedBuff = mutable.ArrayBuffer.empty[Array[immutable.Vector[(Int, mutable.ArrayBuffer[Double])]]]
			val critRegBuff = mutable.ArrayBuffer.empty[Array[Double]]
			val mapsRegCritBuff = mutable.ArrayBuffer.empty[mutable.HashMap[Int, Double]]
			val classedRegBuff = mutable.ArrayBuffer.empty[Array[(Int, Int)]]
			val coInterceptBuff = mutable.ArrayBuffer.empty[Array[Array[Double]]]
			val coXYcoefBuff = mutable.ArrayBuffer.empty[Array[Array[Double]]]
		  	// Clusterwise
		  	if( sizeBloc == 1 )
		  	{
		  		val (_, predFitted, coIntercept, coXYcoef, critReg, mapsRegCrit, classedReg) = ClusterwiseCore.plsPerDot[S](broadcastedTrainData.value(idxCV), h, g, nbMaxAttemps, logOn)
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
		  		val (_, predFitted, coIntercept, coXYcoef, critReg, mapsRegCrit, classedReg) = ClusterwiseCore.plsPerMicroClusters[S](broadcastedTrainData.value(idxCV), broadcastedMicroClusterByIdAndNumbers.value.get._1, h, g, broadcastedMicroClusterByIdAndNumbers.value.get._2, nbMaxAttemps, logOn)
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

		def computeRmseTrainAndTest(idxCV: Int, bestClassifiedDataOut: Seq[(Int, Int)], bestCoInterceptOut: Seq[Array[Double]], bestCoXYcoefOut: Array[Array[Double]], bestFittedOut: Array[immutable.Vector[(Int, mutable.ArrayBuffer[Double])]]) =
		{
			val mapBestClassifiedDataOut = immutable.HashMap(bestClassifiedDataOut:_*)
		
			/***********************************************************************************/
			/*   Compute the final G separate multiblock analyses (with the complete dataset)  */
			/***********************************************************************************/
			val labeledRDD = broadcastedTrainData.value(idxCV).map{ case (id, (x, y)) => (mapBestClassifiedDataOut(id), (id, x, y)) }

			val modelByCluster = labeledRDD.groupBy{ case (clusterID, (idx, x, y)) => clusterID }.map{ case (clusterID, aggregate) =>
			{
				val tmpBuffer = mutable.ArrayBuffer(aggregate.seq:_*)
				val xDs = tmpBuffer.map{ case (_, (idx, x, _)) => (idx, x) }
				val yDs = tmpBuffer.map{ case (_, (_, _, y)) => y }
				val (_, xyCoef, intercept, prediction) = PLS.runPLS(xDs, yDs, h)
				(clusterID, (intercept, xyCoef, prediction))
			}}

			/********************************************************************************************************/
			/* 										Test the model on testing set 									*/
			/********************************************************************************************************/
			val trainedData = labeledRDD.map{ case (label, (idx, x, y)) => (idx, (x, y, label)) }
			val metric = new Euclidean[S]
			val clusterwiseModel = new ClusterwiseModel[S](trainedData, modelByCluster, standardizationParameters, metric)
			clusterwiseModels += clusterwiseModel

			val testY = splits(idxCV)
			val labelAndPrediction = clusterwiseModel.predictClusterViaKNNLocal(testY, kPredict, g, withY)
			val yPredTrainSort = bestFittedOut.flatten.sortBy(_._1)

			/********************************************************************************************************/
			/*										Measure quality of prediction 									*/
			/********************************************************************************************************/
			val trainY = broadcastedTrainData.value(idxCV).seq.sortBy{ case (id, _) => id }.map{ case (_, (_, y)) => y }
		 	val testSize = testY.size

		 	val (meanX, meanY, sdX, sdY) = standardizationParameters.get

		 	val meanTrain = trainY.reduce(SumVectors.sumVectors[Double, S](_, _).asInstanceOf[S]).map(_ / trainY.size)

		 	val sdYtrain = trainY.map(_.zipWithIndex.map{ case (y, meanIdx) => pow(y - meanTrain(meanIdx), 2) }.asInstanceOf[S]).reduce(SumVectors.sumVectors[Double, S](_, _).asInstanceOf[S]).map( x => sqrt(x / (broadcastedTrainData.value(idxCV).size - 1)) )
		 	
		 	val meanTest = testY.map(_._2._2).reduce(SumVectors.sumVectors[Double, S](_, _).asInstanceOf[S]).map(_ / testSize)
		 	
		 	val sdYtest = testY.map{ case (_, (_, y)) => y }.map(_.zipWithIndex.map{ case(y, meanIdx) => pow(y - meanTest(meanIdx), 2) }.asInstanceOf[S]).reduce(SumVectors.sumVectors[Double, S](_, _).asInstanceOf[S]).map( x => sqrt(x / (testSize - 1)))

		 	// Standardized RMSE of train data
			val sqRmseTrainIn = if( q == 1 ) trainY.zip(yPredTrainSort).map{ case ((trueY, (_, yPred))) => pow(trueY.head - yPred.head, 2)}.sum / trainY.size / sdYtrain.head
				else trainY.zip(yPredTrainSort).map{ case ((trueY, (_, yPred))) => trueY.zip(yPred).map( x => pow(x._1 - x._2, 2) ).asInstanceOf[S] }
			    	.reduce(SumVectors.sumVectors[Double, S](_, _).asInstanceOf[S])
			    	.map( _ / trainY.size )
			    	.zip(sdYtrain)
			    	.map{ case (rmseTrain, sdy) => rmseTrain / sdy }
			    	.sum / q

			val testAndPredData = testY.zip(labelAndPrediction)
		 	// Standardized RMSE of test data
			val sqRmseTestIn = if( q == 1 ) testAndPredData.map{ case ((idx, (x, y)), (idx2, (label, yPred))) => pow(y.head - yPred(0), 2) }.sum / testSize / sdYtest.head
				else testAndPredData.map{ case ((idx, (x, y)), (idx2, (label, yPred))) => y.zip(yPred.toArray).map{ case (yTest, yPred) => pow(yTest - yPred, 2) }.asInstanceOf[S] }
					.reduce(SumVectors.sumVectors[Double, S](_, _).asInstanceOf[S])
					.zip(sdYtest)
					.map{ case (rmseTest, sdTest) => rmseTest / sdTest }
					.sum / q

			(sqRmseTrainIn, sqRmseTestIn)
		}

		val rmseTrainAndTest = bestModelPerCV.toArray.map{ case (idxCV, (bestClassifiedDataOut, _, bestCoInterceptOut, bestCoXYcoefOut, bestFittedOut)) => computeRmseTrainAndTest(idxCV, bestClassifiedDataOut, bestCoInterceptOut, bestCoXYcoefOut, bestFittedOut) }

		(rmseTrainAndTest, clusterwiseModels.toArray)
	}
}

object Clusterwise extends Serializable
{
	/**
	 *
	 *
	 */
	def run[S <: Seq[Double] : ClassTag](
		@(transient @param) sc: SparkContext,
		dataXY: GenSeq[(Int, (S, S))],
		g: Int,
		h: Int,
		nbCV: Int,
		init: Int,
		k: Int,
		withY: Boolean = true,
		standardized: Boolean = true,
		sizeBloc: Int = 1,
		nbMaxAttemps: Int = 30,
		epsilonKmeans: Double = 0.00001,
		iterMaxKmeans: Int = 100,
		logOn: Boolean = false
	): (Array[(Double, Double)], Array[ClusterwiseModel[S]]) = 
	{
		val clusterwise = new Clusterwise[S](sc, dataXY, g, h, nbCV, init, k, withY, standardized, sizeBloc, nbMaxAttemps, epsilonKmeans, iterMaxKmeans, logOn)
		clusterwise.run	
	}
}