package clusterwise

import org.apache.spark.mllib.linalg.{Vector, Vectors}
import scala.util.Sorting.quickSort
import org.apache.spark.rdd.RDD
import math.{floor, pow}
import scala.collection.mutable.{ArrayBuffer, HashMap}
import org.apache.spark.SparkContext
import breeze.linalg.{DenseMatrix, DenseVector}
import org.apache.spark.broadcast.Broadcast
import scala.util.Random


class Prediction(val xyTrain:Broadcast[Array[(Int, (Array[Double], Array[Double], Int))]], val xyTest:RDD[(Int, (Array[Double],Array[Double]))], var k:Int, var g:Int)(var w:Double=1.0) extends Serializable
{
	//this(_, _, 25, 2)(1.0)
	type id__x_y_Test = Array[(Int, (Array[Double], Array[Double]))]

	val b = Random.nextDouble * w

	def cwPredictionKNNdistributed(tabHash:Array[Array[Double]], sizeBloc:Int, lsh:Boolean, intercept_XYcoef_pred_ByClass:scala.collection.Map[Int, (Array[Double], breeze.linalg.DenseMatrix[Double], Array[(Int, Array[Double])])]) =
	{
		val labelisedData = xyTest.mapPartitions(it => (if(lsh) /*lshVote(resCW,x.toArray,tabHash,sizeBloc,k,g,b,w)*/Array.empty[(Int, Int, Array[Double])] 
			//else knnMajorityVote(it.toArray, k, g) ).toIterator).sortBy{ case(id, label, y) => id }
			else knnMajorityVoteWithY(it.toArray, k, g) ).toIterator).sortBy{ case(id, label, y) => id }

		val yPred = labelisedData.map{ case(id, label, x) => (id, (label, (DenseVector(intercept_XYcoef_pred_ByClass(label)._1).t + DenseVector(x).t * intercept_XYcoef_pred_ByClass(label)._2).t)) }//.sortBy{ case(id, (label, yPred)) => id }

		yPred

	}

	val knn = (v:Array[Double], l:Array[(Array[Double], Int)], k:Int) => 
		l.map{ case(v2, label) => ((for( i <- v2.indices) yield(pow(v(i) - v2(i), 2))).reduce(_ + _), (v2, label)) }
			.sortBy{ case (dist, _) => dist }
			.take(k)
			.map{ case (_, (vector, label)) => (vector, label) }

	val knnMajorityVote : (id__x_y_Test, Int, Int) => Array[(Int, Int, Array[Double])] = (xyTest, k, g) =>
	{
/*
		println("labels")
		println(xyTrain.value.map{ case (_, (x2, _, label2)) => (x2, label2) }.map(_._2).distinct.toList)
		println("labels")
*/
		xyTest.map{ case (idx, (x, y)) => 
			{
				//println(x.toList)
				val neighbours = xyTrain.value.map{ case (_, (x2, _, label2)) => (x2, label2) }
				val majVote = knn(x, neighbours, k)
				val cptVote = Array.fill(g)(0)
				majVote.foreach{ case (_, label3) => cptVote(label3 % g) += 1 }
				val classElem = cptVote.indexOf(cptVote.max)
				println(cptVote.toList)
				(idx, classElem, x)
			}}
	}


	val knnMajorityVoteWithY : (id__x_y_Test, Int, Int) => Array[(Int, Int, Array[Double])] = (xyTest, k, g) =>
	{

		xyTest.map{ case (idx, (x, y)) => 
			{
				//println(x.toList)
				val neighbours = xyTrain.value.map{ case (_, (x2, y2, label2)) => (x2 ++ y2, label2) }
				val majVote = knn(x ++ y, neighbours, k)
				val cptVote = Array.fill(g)(0)
				majVote.foreach{ case (_, label3) => cptVote(label3 % g) += 1 }
				val classElem = cptVote.indexOf(cptVote.max)
				println(cptVote.toList)
				(idx, classElem, x)
			}}
	}

	def kNNvoteSparkV(xyTest:id__x_y_Test, k:Int, g:Int) =
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

	def kNNvoteWithY(xyTest:id__x_y_Test, k:Int, g:Int) = {
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

	def lshVote(xyTrain:Array[(Int,(Array[Double],Array[Double],Int))],
				xyTest:Array[((Array[Double],Array[Double]),Int)],
				tabHash:Array[Array[Double]],
				sizeBloc:Int,
				k:Int,
				g:Int,
				b:Double,
				w:Double) = {
		val bufferLabeled = ArrayBuffer.empty[(Int,Int)]
		val preLSH = xyTest.map(x=>(x._1,x._2,0)) ++ xyTrain.map(x=>((x._2._1,x._2._2),x._2._3,1))
		val postLSH = preLSH.map(x=>(Lsh.hashfunc(x._1._1,w,b,tabHash),x._1._1,x._1._2,x._2,x._3))
		quickSort(postLSH)(Ordering[Double].on(_._1))
		val buckets = postLSH.grouped(sizeBloc).toArray
		val bucketsTest = buckets.map(_.filter(_._5==0)).map(_.map(x=>(x._2,x._3,x._4)))
		val bucketsTrain = buckets.map(_.filter(_._5==1)).map(_.map(x=>(x._2,x._3,x._4)))
		for(i<- buckets.indices) {
			for(j<-bucketsTest(i).indices) {
				val elem = bucketsTest(i)(j)
				val elemIdx = elem._3
				val elemX = Vectors.dense(elem._1)
				var bufferDist = ArrayBuffer.empty[(Double,Int)]
				for(j2<-bucketsTrain(i).indices) {
					val elem2 = bucketsTrain(i)(j2)
					val elem2X = Vectors.dense(elem2._1)
					val elem2Class = elem2._3
					val dist = Vectors.sqdist(elemX,elem2X)
					bufferDist += ((dist,elem2Class))
				}
				val arrayDist = bufferDist.toArray
				quickSort(arrayDist)(Ordering[Double].on(_._1))
				val cptVote = (1 to g).toArray.map(x=>0)
				val majVote = arrayDist.take(k)
				majVote.foreach(x=>cptVote(x._2%g)+=1)	
				val max = majVote.max
				val classElem = majVote.indexOf(max)
				bufferLabeled += ((elemIdx,classElem))
			}
		}
		bufferLabeled
	}
	
	def lshVoteWithY(xyTrain:Array[(Int,(Array[Double],Array[Double],Int))],
				xyTest:Array[((Array[Double],Array[Double]),Int)],
				tabHash:Array[Array[Double]],
				sizeBloc:Int,
				k:Int,
				g:Int,
				b:Double,
				w:Double) = {
		val bufferLabeled = ArrayBuffer.empty[(Int,Int)]
		val preLSH = xyTest.map(x=>(x._1,x._2,0)) ++ xyTrain.map(x=>((x._2._1,x._2._2),x._2._3,1))
		val postLSH = preLSH.map(x=>(Lsh.hashfunc(x._1._1,w,b,tabHash),x._1._1,x._1._2,x._2,x._3))
		quickSort(postLSH)(Ordering[Double].on(_._1))
		val buckets = postLSH.grouped(sizeBloc).toArray
		val bucketsTest = buckets.map(_.filter(_._5==0)).map(_.map(x=>(x._2,x._3,x._4)))
		val bucketsTrain = buckets.map(_.filter(_._5==1)).map(_.map(x=>(x._2,x._3,x._4)))
		for(i<- buckets.indices) {
			for(j<-bucketsTest(i).indices) {
				val elem = bucketsTest(i)(j)
				val elemIdx = elem._3
				val elemX = Vectors.dense(elem._1++elem._2)
				var bufferDist = ArrayBuffer.empty[(Double,Int)]
				for(j2<-bucketsTrain(i).indices) {
					val elem2 = bucketsTrain(i)(j2)
					val elem2X = Vectors.dense(elem2._1++elem2._2)
					val elem2Class = elem2._3
					val dist = Vectors.sqdist(elemX,elem2X)
					bufferDist += ((dist,elem2Class))
				}
				val arrayDist = bufferDist.toArray
				quickSort(arrayDist)(Ordering[Double].on(_._1))
				val cptVote = (1 to g).toArray.map(x=>0)
				val majVote = arrayDist.take(k)
				majVote.foreach(x=>cptVote(x._2%g)+=1)	
				val max = majVote.max
				val classElem = majVote.indexOf(max)
				bufferLabeled += ((elemIdx,classElem))
			}
		}
		bufferLabeled
	}
}