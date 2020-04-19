package org.clustering4ever.clustering.dcdpm

import org.apache.commons.math3.distribution.MultivariateNormalDistribution
import Jama.Matrix
import scala.collection.mutable

trait AbstractCluster extends Serializable {
  
  var id: Int

  var size: Double

  val yy: Array[Double]

  val phi: Array[Double]

  val dim = yy.length
  
  def isEmpty: Boolean = {
    size == 0
  }
    
  def updateId(newId: Int) = {  
      id = newId
  }
 
  private[clustering4ever] val dimRange = 0 until dim


}

final case class Cluster(
  var id: Int,
  var size: Double,
  val yy: Array[Double],
  val phi: Array[Double],
  var beta: Double,
  var isNewCluster: Boolean
) extends AbstractCluster {
    
  def addPoint() = {
    size += 1
  }
    
  def removePoint() = {
    size -= 1
  }
  
  def calculateMean(dataCluster: Array[Point]): Unit = {
    if (isEmpty) {
      dimRange.foreach(phi(_) = 0D)
    }      
    else {
      dimRange.foreach{ j =>
        val s = dataCluster.foldLeft(0D)(_ + _.vector(j))
        yy(j) = s / dataCluster.length
      }

    }
    
  }
    
}

final case class GlobalCluster(
  var subIds: mutable.ArrayBuffer[(Int, Int)],
  var id: Int,
  var size: Double,
  val yy: Array[Double],
  val phi: Array[Double],
  val mean: Array[Double],
  var σ2: Array[Array[Double]]
) extends AbstractCluster {
  
  
  def updateyy(means: mutable.ArrayBuffer[_ <: AbstractCluster]): Unit = {
    
    val sum = Array.fill(dim)(0D)

    val sumSize = means.foldLeft(0D){ (sum1, mean) =>
        dimRange.foreach( i => sum(i) += mean.size * mean.yy(i) )
        sum1 + mean.size
    }
    
    dimRange.foreach( i => yy(i) = sum(i) / sumSize )
    
  }
  
  def updatePhi(σ1: Array[Array[Double]]): Unit = {
   
    val sigma = postSigma(σ1)
    postMean(σ1, sigma)
     
    val normalDist = new MultivariateNormalDistribution(mean, sigma)
    
    normalDist.sample.zipWithIndex.foreach{ case (v, i) => phi(i) = v }
      
    σ2 = sigma

  }
  
  def postMean(σ1: Array[Array[Double]], sigma: Array[Array[Double]]): Unit = {

    val y = Array.ofDim[Double](dim, 1)
    dimRange.foreach( i => y(i)(0) = yy(i) )
    
    val s1 = matrixProduct(inverseMatrix(σ1), y)
    dimRange.foreach( i => s1(i)(0) *= size )
      
    val m = Array.ofDim[Double](dim, 1)
    dimRange.foreach( i => m(i)(0) = mean(i) )
    
    val s2 = matrixProduct(inverseMatrix(σ2), m)
    val s = matrixProduct(sigma, matrixSum(s1, s2))    
    
    dimRange.foreach( i => mean(i) = s(i)(0) )
  
  }
    
  def postSigma(σ1: Array[Array[Double]]): Array[Array[Double]] = {
    
    val sigma = inverseMatrix(σ1)

    dimRange.foreach{ i =>
      dimRange.foreach{ j =>
        sigma(i)(j) *= size
      }
    }
    
    inverseMatrix(matrixSum(sigma, inverseMatrix(σ2)))

  }
  
  def matrixProduct(a: Array[Array[Double]], b: Array[Array[Double]]): Array[Array[Double]] = {
    
    val m = a.size
    val n = b.size
    val p = b.head.size
    val output = Array.ofDim[Double](m, p)
    val rangeIn = (0 until p)
    val rangeIn2 = (0 until n)

    (0 until m).par.foreach{ i =>
      rangeIn.foreach{ j =>        
        output(i)(j) = rangeIn2.foldLeft(0D)( (agg, k) => agg + a(i)(k) * b(k)(j) )
      }
    }

    output

  }

  def matrixSum(a: Array[Array[Double]], b: Array[Array[Double]]): Array[Array[Double]] = {
    
    val n = a.length
    val m = a.head.length
    val output = Array.ofDim[Double](n, m)

    val rangeIn = (0 until m)

    (0 until n).foreach{ i =>
      rangeIn.foreach{ j =>
        output(i)(j) = a(i)(j) + b(i)(j)
      }
    }

    output

  }
  
  def inverseMatrix(a: Array[Array[Double]]) = {
    val matrix = new Matrix(a)
    matrix.inverse.getArray
  }
  
}

final case class ResumeCluster(val workerId: Int, var id: Int, var size: Double, val yy: Array[Double], val phi: Array[Double]) extends AbstractCluster