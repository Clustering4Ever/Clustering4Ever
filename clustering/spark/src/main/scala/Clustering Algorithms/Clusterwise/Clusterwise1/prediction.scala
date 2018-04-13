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


class Prediction(val xyTrain: Broadcast[Array[(Int, (Array[Double], Array[Double], Int))]], val xyTest: RDD[(Int, (Array[Double],Array[Double]))], var k:Int, var g:Int) extends Serializable
{
	type IdXYTest = Array[(Int, (Array[Double], Array[Double]))]

	def cwPredictionKNNdistributed(classInterceptXYcoefPred:scala.collection.Map[Int, (Array[Double], breeze.linalg.DenseMatrix[Double], Array[(Int, Array[Double])])]) =
	{
		val labelisedData = xyTest.mapPartitions( it => kNNvote(it.toArray, k, g).toIterator).sortBy{ case (id, label, y) => id }

		val yPred = labelisedData.map{ case (id, label, x) => (id, (label, (DenseVector(classInterceptXYcoefPred(label)._1).t + DenseVector(x).t * classInterceptXYcoefPred(label)._2).t)) }

		yPred

	}

	def kNNvote(xyTest:IdXYTest, k:Int, g:Int) =
	{
		val bufferLabeled = ArrayBuffer.empty[(Int,Int, Array[Double])]
		for( (idx, (x, y)) <- xyTest)
		{
			//val elem = xyTest(i)
			val elemX = Vectors.dense(x)
			val bufferDist = ArrayBuffer.empty[(Double, Int)]
			for( (idx2, (x2, y2, label2)) <- xyTrain.value ) {
				val elem2X = Vectors.dense(x2)
				val dist = Vectors.sqdist(elemX, elem2X)
				bufferDist += ((dist, label2))
			}
			bufferDist.sortBy{ case (dist, _) => dist }
			val cptVote = Array.fill(g)(0)
			val majVote = bufferDist.take(k)
			majVote.foreach{ case (_, label3) => cptVote(label3 % g) += 1 }
			val classElem = cptVote.indexOf(cptVote.max)
			bufferLabeled += ((idx, classElem, x))
		}
		bufferLabeled.toArray
	}

	def kNNvoteWithY(xyTest:IdXYTest, k:Int, g:Int) = {
		val bufferLabeled = ArrayBuffer.empty[(Int,Int)]
		for( (idx, (x, y)) <- xyTest)
		{
			val elemXY = Vectors.dense(x ++ y)
			val bufferDist = ArrayBuffer.empty[(Double,Int)]
			for( (idx2, (x2, y2, label2)) <- xyTrain.value ) {
				val elem2XY = Vectors.dense(x2 ++ y2)
				val dist = Vectors.sqdist(elemXY, elem2XY)
				bufferDist += ((dist, label2))
			}
			bufferDist.sortBy{ case (dist, _) => dist }
			val cptVote = Array.fill(g)(0)
			val majVote = bufferDist.take(k)
			majVote.foreach{ case (_, label3) => cptVote(label3 % g) += 1 }
			val classElem = majVote.indexOf(majVote.max)
			bufferLabeled += ((idx,classElem))
		}
		bufferLabeled
	}
}