package clustering4ever.spark.clustering.clusterwise

import org.apache.spark.mllib.linalg.{Vector, Vectors}
import scala.util.Sorting.quickSort
import org.apache.spark.rdd.RDD
import math.floor
import scala.collection.mutable.{ArrayBuffer, HashMap}
import org.apache.spark.SparkContext
import breeze.linalg.{DenseMatrix, DenseVector}
import org.apache.spark.broadcast.Broadcast
import scala.util.Random
import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.clustering4ever.math.distances.scalar.Euclidean


class Prediction(val xyTrain: Broadcast[Array[(Int, (Array[Double], Array[Double], Int))]], val xyTest: RDD[(Int, (Array[Double],Array[Double]))], var k:Int, var g:Int)(metric: ContinuousDistances = new Euclidean(true)) extends Serializable
{
	type IdXYTest = Array[(Int, (Array[Double], Array[Double]))]

	def cwPredictionKNNdistributed(classInterceptXYcoefPred:scala.collection.Map[Int, (Array[Double], breeze.linalg.DenseMatrix[Double], Array[(Int, Array[Double])])]) =
	{
		val labelisedData = xyTest.mapPartitions( it => kNNvote(it.toArray, k, g).toIterator).sortBy{ case (id, label, y) => id }
		val yPred = labelisedData.map{ case (id, label, x) => (id, (label, (DenseVector(classInterceptXYcoefPred(label)._1).t + DenseVector(x).t * classInterceptXYcoefPred(label)._2).t)) }
		yPred
	}

	def knn(v1: Array[Double], l: Array[(Array[Double], Int)], k: Int) =
	{	
		l.map{ case(v2, label) => (metric.d(v1, v2), (v2, label)) }
			.sortBy{ case (dist, _) => dist }
			.take(k)
			.map{ case (_, (vector, label)) => (vector, label) }
	}

	def kNNvote(xyTest: IdXYTest, k: Int, g: Int) =
	{
		xyTest.map{ case (idx, (x, y)) => 
			{
				val neighbours = xyTrain.value.map{ case (_, (x2, _, label2)) => (x2, label2) }
				val majVote = knn(x, neighbours, k)
				val cptVote = Array.fill(g)(0)
				majVote.foreach{ case (_, label3) => cptVote(label3 % g) += 1 }
				val classElem = cptVote.indexOf(cptVote.max)
				println(cptVote.toList)
				(idx, classElem, x)
			}}
	}

	def kNNvoteWithY(xyTest: IdXYTest, k: Int, g: Int) =
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
}