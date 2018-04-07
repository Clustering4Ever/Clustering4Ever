package clustering4ever.scala.clustering.meanshift

import _root_.scala.util.Random
import _root_.scala.collection.parallel.mutable.ParArray
import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes
import _root_.clustering4ever.clustering.ClusteringAlgorithms
import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.clustering4ever.math.distances.scalar.Euclidean
import _root_.clustering4ever.util.SumArrays
import _root_.scala.math.{min, max}
import _root_.scala.collection.{immutable, mutable}
import _root_.scala.util.Random
import _root_.clustering4ever.scala.kernels.Kernels
import _root_.clustering4ever.scala.kernels.KernelNature._
import _root_.clustering4ever.scala.kernels.KernelNature
/**
 * @author Beck Gaël
 *
 * Mean-Shift-LSH clustering 
 * This algorithm could be used to analyse complex multivariate multidimensional data.
 * It can also be apply in order to analyse image, to use this features it is recommanded to convert image from RGB space to L*u*v* space
 * The major class where MS-LSH algorithm and prediction fonction are implemented
 */
class KernelGradientAscent(
  var epsilon: Double,
  var maxIterations: Int,
  metric: ContinuousDistances,
  kernelType: KernelType,
  bandwitch: Double
)
{

  def gradientAscent(readyToGA: Seq[(Int, Array[Double])], maxIterations: Int) =
  {
    var ind = 0
    val haveNotConverged = false
    var everyPointsHaveConverged = false
    var gradientAscentData: ParArray[(Int, Array[Double], Array[Double], Boolean)] = readyToGA.toParArray.map{ case (id, vector) => (id, vector, vector, haveNotConverged) }
    val kernelLocality = readyToGA.map{ case (_, vector) => vector }
  
    def kernelGradientAscent(toExplore: ParArray[(Int, Array[Double], Array[Double], Boolean)]) =
    {
      var cptConvergedPoints = 0
      val convergingData = toExplore.map{ case (id, vector, mod, haveConverged) =>
      { 
        val newMod = if( haveConverged )
        {
          mod
        }
        else
        {
          kernelType match
          {
            case kernel if( kernel == Gaussian || kernel == Flat ) =>  Kernels.computeModesThroughKernels(mod, kernelLocality, bandwitch, kernelType, metric)
            //case KernelNature.Sigmoid =>  Kernels.computeSigmoidKernel(mod, kernelLocality, bandwitch)
          }
        }
        
        val modShift = metric.d(newMod, mod)
        val hasConverged = if( modShift <= epsilon )
        {
          cptConvergedPoints += 1
          true
        }
        else 
        {
          false
        }

        (id, vector, newMod, hasConverged)
      }}

      (convergingData, cptConvergedPoints)
    }

      while( ind < maxIterations && ! everyPointsHaveConverged )
      {
        val (gaDStmp, cptConvergedPoints) = kernelGradientAscent(gradientAscentData)
        gradientAscentData = gaDStmp
        ind += 1
        everyPointsHaveConverged = cptConvergedPoints == gradientAscentData.size
      }
      gradientAscentData
  }
}

object KernelGradientAscent
{

  /**
   * Trains a MS-LSH model using the given set of parameters.
   *
   * @param data : an RDD[(String,Vector)] where String is the ID and Vector the rest of data
   * @param epsilon : threshold under which we stop iteration in gradient ascent
   * @param maxIterations : Number of iteration for modes search
   *
   **/
   def run(
    data: Seq[(Int, Array[Double])],
    metric: ContinuousDistances,
    epsilon: Double,
    maxIterations: Int,
    kernelType: KernelType,
    bandwitch: Double
    ) =
  {
    val meanShift = new KernelGradientAscent(epsilon, maxIterations, metric, kernelType, bandwitch)
    meanShift.gradientAscent(data, maxIterations)
  }
}