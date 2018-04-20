package clustering4ever.spark.clustering.clusterwise

import org.apache.spark.mllib.linalg.{Vector, Vectors}
import org.apache.spark.rdd.RDD
import math.{floor, pow}
import org.apache.spark.SparkContext
import breeze.linalg.{DenseMatrix, DenseVector}
import org.apache.spark.broadcast.Broadcast
import _root_.scala.collection.mutable.{ArrayBuffer, HashMap}
import _root_.scala.util.Random
import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.clustering4ever.math.distances.scalar.Euclidean
import _root_.clustering4ever.clustering.ClusteringModel

class ClusterwiseModel(
	val xyTrain: Array[(Int, (Array[Double], Array[Double], Int))],
	val interceptXYcoefPredByClass: scala.collection.Map[Int, (Array[Double], breeze.linalg.DenseMatrix[Double], Array[(Int, Array[Double])])],
	standardizationParameters: Option[(Array[Double], Array[Double], Array[Double], Array[Double])] = None,
	metric: ContinuousDistances = new Euclidean(true)
) extends ClusteringModel
{
	type Xvector = Array[Double]
	type Yvector = Array[Double]
	type IDXtest = Array[(Long, Xvector)]
	type IDXYtest = Seq[(Int, (Xvector, Yvector))]

	val (meanX, meanY, sdX, sdY) = if( standardizationParameters.isDefined )
	{
		standardizationParameters.get
	}
	else
	{
		(Array.empty[Double], Array.empty[Double], Array.empty[Double], Array.empty[Double])
	}

	private[this] def knn(v: Array[Double], neighbors: Array[(Array[Double], Int)], k:Int) =
	{
		neighbors.map{ case (v2, clusterID) => (metric.d(v, v2), (v2, clusterID)) }
			.sortBy{ case (dist, _) => dist }
			.take(k)
			.map{ case (_, (vector, clusterID)) => (vector, clusterID) }
	}

	private[this] def knnMajorityVote(xyTest: IDXtest, k: Int, g: Int): Array[(Long, Int, Array[Double])] =
	{
		xyTest.map{ case (idx, x) => 
		{
			val neighbours = xyTrain.map{ case (_, (x2, _, clusterID)) => (x2, clusterID) }
			val majVote = knn(x, neighbours, k)
			val cptVote = Array.fill(g)(0)
			majVote.foreach{ case (_, clusterID) => cptVote(clusterID) += 1 }
			val classElem = cptVote.indexOf(cptVote.max)
			(idx, classElem, x)
		}}
	}


	private[this] def knnMajorityVoteWithY(xyTest: IDXYtest, k: Int, g: Int): Seq[(Int, Int, Array[Double])] =
	{
		xyTest.map{ case (idx, (x, y)) => 
		{
			val neighbours = xyTrain.map{ case (_, (x2, y2, label2)) => (x2, label2) }
			val majVote = knn(x, neighbours, k)
			val cptVote = Array.fill(g)(0)
			majVote.foreach{ case (_, clusterID) => cptVote(clusterID % g) += 1 }
			val classElem = cptVote.indexOf(cptVote.max)
			(idx, classElem, x)
		}}
	}

	def predictClusterViaKNNLocal(
		xyTest: Seq[(Int, (Xvector, Yvector))],
		k: Int,
		g: Int
	) =
	{
		val labelisedData = knnMajorityVoteWithY(xyTest, k, g)

		val yPred = labelisedData.map{ case(id, clusterID, x) =>
		{
			val (intercept, xyCoef, _) = interceptXYcoefPredByClass(clusterID)
			val prediction = (DenseVector(intercept).t + DenseVector(x).t * xyCoef).t
			(id, (clusterID, prediction))
		}}

		yPred
	}

	def cwPredictionKNNdistributed(
		xyTest: RDD[(Int, (Xvector, Yvector))],
		k: Int,
		g: Int
	) =
	{
		val labelisedData = xyTest.mapPartitions( it => knnMajorityVoteWithY(it.toSeq, k, g).toIterator )

		val yPred = labelisedData.map{ case(id, clusterID, x) =>
		{
			val (intercept, xyCoef, _) = interceptXYcoefPredByClass(clusterID)
			(id, (clusterID, (DenseVector(intercept).t + DenseVector(x).t * xyCoef).t))
		}}

		yPred
	}

	def predictKNN(
		toPredict: RDD[(Long, Xvector)],
		k: Int,
		g: Int
	)(implicit d: DummyImplicit) =
	{
		val labelisedData = toPredict.mapPartitions( it => knnMajorityVote(it.toArray, k, g).toIterator )
		val yPred = labelisedData.map{ case(id, clusterID, x) =>
		{
			(
				id,
				(
					clusterID,
					{
						val (intercept, xyCoef, _) = interceptXYcoefPredByClass(clusterID)
						val standardizedOrNotPrediction = ((DenseVector(intercept).t + DenseVector(x).t * xyCoef).t).toArray
						if( standardizationParameters.isDefined )
						{
							val unStandardizedPrediction = standardizedOrNotPrediction.zipWithIndex.map{ case (y, idx) => y * sdY(idx) + meanY(idx) }
							unStandardizedPrediction
						}
						else
						{
							standardizedOrNotPrediction
						}
					}
				)
			)
		}}
		yPred
	}

	def standardizeData(toStandardize: RDD[(Long, Xvector)]) =
	{
		toStandardize.map{ case (id, x) =>
		(
			id,
			x.zipWithIndex.map{ case (value, idx) => (value - meanX(idx)) / sdX(idx) }
		)
		}
	}

}