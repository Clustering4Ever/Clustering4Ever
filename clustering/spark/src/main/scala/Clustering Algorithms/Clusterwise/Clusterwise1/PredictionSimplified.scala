package clustering4ever.spark.clustering.clusterwise

import org.apache.spark.mllib.linalg.{Vector, Vectors}
import org.apache.spark.rdd.RDD
import math.{floor, pow}
import _root_.scala.collection.mutable.{ArrayBuffer, HashMap}
import org.apache.spark.SparkContext
import breeze.linalg.{DenseMatrix, DenseVector}
import org.apache.spark.broadcast.Broadcast
import _root_.scala.util.Random

object PredictionSimplified
{
	type IDXtest = Array[(Long, Xvector)]
	type IDXYtest = Array[(Int, (Xvector, Yvector))]
	type Xvector = Array[Double]
	type Yvector = Array[Double]

	val knn = (v: Array[Double], neighbors: Array[(Array[Double], Int)], k:Int) =>
	{
		neighbors.map{ case (v2, label) => ((for( i <- v2.indices ) yield( pow(v(i) - v2(i), 2) )).reduce(_ + _), (v2, label)) }
			.sortBy{ case (dist, _) => dist }
			.take(k)
			.map{ case (_, (vector, label)) => (vector, label) }
	}

	def knnMajorityVote(xyTrain: Broadcast[Array[(Int, (Array[Double], Array[Double], Int))]], xyTest: IDXtest, k: Int, g: Int) : Array[(Long, Int, Array[Double])] =
	{
		xyTest.map{ case (idx, x) => 
		{
			val neighbours = xyTrain.value.map{ case (_, (x2, _, label2)) => (x2, label2) }
			val majVote = knn(x, neighbours, k)
			val cptVote = Array.fill(g)(0)
			majVote.foreach{ case (_, label3) => cptVote(label3 % g) += 1 }
			val classElem = cptVote.indexOf(cptVote.max)
			(idx, classElem, x)
		}}
	}


	def knnMajorityVoteWithY(xyTrain:Broadcast[Array[(Int, (Array[Double], Array[Double], Int))]], xyTest: IDXYtest, k: Int, g: Int) : Array[(Int, Int, Array[Double])] =
	{
		xyTest.map{ case (idx, (x, y)) => 
		{
			val neighbours = xyTrain.value.map{ case (_, (x2, y2, label2)) => (x2 ++ y2, label2) }
			val majVote = knn(x ++ y, neighbours, k)
			val cptVote = Array.fill(g)(0)
			majVote.foreach{ case (_, label3) => cptVote(label3 % g) += 1 }
			val classElem = cptVote.indexOf(cptVote.max)
			(idx, classElem, x)
		}}
	}

	def cwPredictionKNNdistributed(
		xyTrain:Broadcast[Array[(Int, (Array[Double], Array[Double], Int))]],
		xyTest: RDD[(Int, (Array[Double],Array[Double]))],
		k: Int,
		g: Int,
		interceptXYcoefPredByClass: _root_.scala.collection.Map[Int, (Array[Double], breeze.linalg.DenseMatrix[Double], Array[(Int, Array[Double])])]) =
	{
		val labelisedData = xyTest.mapPartitions( it => knnMajorityVoteWithY(xyTrain, it.toArray, k, g).toIterator ).sortBy{ case(id, label, y) => id }

		val yPred = labelisedData.map{ case(id, label, x) => (id, (label, (DenseVector(interceptXYcoefPredByClass(label)._1).t + DenseVector(x).t * interceptXYcoefPredByClass(label)._2).t)) }

		yPred
	}

	def cwPredictionKNNdistributed(
		xyTrain:Broadcast[Array[(Int, (Array[Double], Array[Double], Int))]],
		xyTest: RDD[(Long, Array[Double])],
		k: Int,
		g: Int,
		interceptXYcoefPredByClass: _root_.scala.collection.Map[Int, (Array[Double], breeze.linalg.DenseMatrix[Double], Array[(Int, Array[Double])])])(implicit d: DummyImplicit) =
	{
		val labelisedData = xyTest.mapPartitions( it => knnMajorityVote(xyTrain, it.toArray, k, g).toIterator ).sortBy{ case(id, label, y) => id }

		val yPred = labelisedData.map{ case(id, label, x) => (id, (label, (DenseVector(interceptXYcoefPredByClass(label)._1).t + DenseVector(x).t * interceptXYcoefPredByClass(label)._2).t)) }

		yPred
	}

}