package clustering4ever.spark.clustering.clusterwise

import org.apache.spark.rdd.RDD
import breeze.linalg.{DenseMatrix, DenseVector}
import scala.collection.immutable
import scala.collection.parallel.ParSeq
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.math.distances.scalar.ClassicEuclidean
import clustering4ever.clustering.ClusteringModel

class ClusterwiseModel(
	val xyTrain: Seq[(Int, (immutable.Seq[Double], immutable.Seq[Double], Int))],
	val interceptXYcoefPredByClass: scala.collection.Map[Int, (Array[Double], breeze.linalg.DenseMatrix[Double], immutable.Vector[(Int, immutable.Vector[Double])])],
	standardizationParameters: Option[(immutable.Seq[Double], immutable.Seq[Double], immutable.Seq[Double], immutable.Seq[Double])] = None,
	metric: ContinuousDistance[immutable.Seq[Double]] = new ClassicEuclidean
) extends ClusteringModel
{
	type Xvector = immutable.Seq[Double]
	type Yvector = immutable.Seq[Double]
	type IDXtest = Seq[(Long, Xvector)]
	type IDXYtest = ParSeq[(Int, (Xvector, Yvector))]

	val (meanX, meanY, sdX, sdY) = if( standardizationParameters.isDefined ) standardizationParameters.get else (immutable.Vector.empty[Double], immutable.Vector.empty[Double], immutable.Vector.empty[Double], immutable.Vector.empty[Double])

	private[this] def knn(v: immutable.Seq[Double], neighbors: Seq[(immutable.Seq[Double], Int)], k:Int) = neighbors.sortBy{ case (v2, clusterID) => metric.d(v, v2) }.take(k)

	private[this] def obtainNearestClass(x: immutable.Seq[Double], k: Int, g: Int, withY: Boolean) =
	{
		val neighbours = if( withY ) xyTrain.map{ case (_, (x2, y2, label2)) => (x2 ++ y2, label2) } else xyTrain.map{ case (_, (x2, _, label2)) => (x2, label2) }
		val majVote = knn(x, neighbours, k)
		val cptVote = Array.fill(g)(0)
		majVote.foreach{ case (_, clusterID) => cptVote(clusterID % g) += 1 }
		val classElem = cptVote.indexOf(cptVote.max)
		classElem
	}

	private[this] def knnMajorityVote(xyTest: Iterator[(Long, Xvector)], k: Int, g: Int): Iterator[(Long, Int, immutable.Seq[Double])] = xyTest.map{ case (id, x) => (id, obtainNearestClass(x, k, g, false), x) }
	
	private[this] def knnMajorityVoteWithY(xyTest: IDXYtest, k: Int, g: Int): ParSeq[(Int, Int, immutable.Seq[Double])] = xyTest.map{ case (id, (x, y)) => (id, obtainNearestClass(x ++ y, k, g, true), x) }

	private[this] def knnMajorityVote2(xyTest: ParSeq[(Int, (Xvector, Yvector))], k: Int, g: Int): ParSeq[(Int, Int, immutable.Seq[Double])] = xyTest.map{ case (id, (x, y)) => (id, obtainNearestClass(x, k, g, false), x) }

	private[this] def knnMajorityVoteWithY2(xyTest: Iterator[(Int, (Xvector, Yvector))], k: Int, g: Int): Iterator[(Int, Int, immutable.Seq[Double])] = xyTest.map{ case (id, (x, y)) => (id, obtainNearestClass(x ++ y, k, g, true), x) }

	def predictClusterViaKNNLocal(
		xyTest: ParSeq[(Int, (Xvector, Yvector))],
		k: Int,
		g: Int,
		withY: Boolean = true
	) =
	{
		val labelisedData = if( withY ) knnMajorityVoteWithY(xyTest, k, g) else knnMajorityVote2(xyTest, k, g)

		val yPred = labelisedData.map{ case(id, clusterID, x) =>
		{
			val (intercept, xyCoef, _) = interceptXYcoefPredByClass(clusterID)
			val prediction = (DenseVector(intercept).t + DenseVector(x.toArray).t * xyCoef).t
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
		val labelisedData = xyTest.mapPartitions( it => knnMajorityVoteWithY2(it, k, g) )

		val yPred = labelisedData.map{ case (id, clusterID, x) =>
		{
			val (intercept, xyCoef, _) = interceptXYcoefPredByClass(clusterID)
			(id, (clusterID, (DenseVector(intercept).t + DenseVector(x.toArray).t * xyCoef).t))
		}}

		yPred
	}

	def predictKNN(
		toPredict: RDD[(Long, Xvector)],
		k: Int,
		g: Int
	) =
	{
		val labelisedData = toPredict.mapPartitions( it => knnMajorityVote(it, k, g) )
		val yPred = labelisedData.map{ case (id, clusterID, x) =>
		{
			(
				id,
				(
					clusterID,
					{
						val (intercept, xyCoef, _) = interceptXYcoefPredByClass(clusterID)
						val standardizedOrNotPrediction = ((DenseVector(intercept).t + DenseVector(x.toArray).t * xyCoef).t).toArray
						if( standardizationParameters.isDefined ) standardizedOrNotPrediction.zipWithIndex.map{ case (y, idx) => y * sdY(idx) + meanY(idx) } else standardizedOrNotPrediction
					}
				)
			)
		}}
		yPred
	}

	def standardizeData(toStandardize: RDD[(Long, Xvector)]) = toStandardize.map{ case (id, x) => (id, x.zip(meanX).zip(sdX).map{ case ((value, mean), sd) => (value - mean) / sd }) }

}