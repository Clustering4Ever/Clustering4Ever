package org.clustering4ever.spark.clustering
/**
 * @author Beck Gaël
 */
import scala.util.Random
import scala.reflect.ClassTag
import scala.collection.{immutable, mutable}
import scala.collection.parallel
import scala.language.higherKinds
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkContext, HashPartitioner}
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.sparktools.UtilSpark
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.hashing.{HashingScalar, LDLSH}
import org.clustering4ever.util.ClusterBasicOperations
import org.clustering4ever.kernels.{Kernel, KernelArgs, KernelArgsGaussian, GaussianKernel}
import org.clustering4ever.vectors.{GVector, ScalarVector}
import org.clustering4ever.shapeless.VMapping
/**
 * @tparam O
 * @tparam V
 * @tparam Cz
 * @tparam Hasher
 * Gradient Ascent LSH clustering 
 * This algorithm could be used to analyse complex multivariate multidimensional data.
 * It can also be apply in order to analyse image, to use this features it is recommanded to convert image from RGB space to L*u*v* space
 */
final case class GradientAscent[
  O,
  V <: Seq[Double],
  Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz],
  Hasher <: HashingScalar[V]
](
  data: RDD[Cz[O, ScalarVector[V]]],
  k: Int,
  knnBucketShift: Int,
  epsilon: Double,
  maxIterations: Int,
  bucketLayers: Int,
  bucketNumber: Int,
  propConvStopIter: Double,
  memoryExpensive: Boolean,
  hasher: Hasher,
  persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY,
  fonctionalStyle: Boolean = true
)(implicit ev: ClassTag[Cz[O, ScalarVector[V]]]) extends Serializable {
  /**
   * Is the mode has converged
   */
  final type HasConverged = Boolean
  /**
   * Is this point is an original dot or a copy of it 
   */
  final type IsOriginalDot = Boolean
  /**
   * To distinguish original vector from its associate mode
   */
  final type Mode = ScalarVector[V]
  /**
   *
   */
  final type BucketID = Int
  /**
   *
   */
  final type AscendingData = parallel.mutable.ParArray[(BucketID, (Cz[O, ScalarVector[V]], Mode, IsOriginalDot, HasConverged))]

  require(bucketNumber > 0)
  require(bucketLayers >= 0)
  /**
   *
   */
  private[this] final implicit val vMapping = new VMapping[Int, ScalarVector[V]]
  /**
   * Valid exclusively with Euclidean distance due to computation of the mean
   */
  private[this] final val metric = Euclidean[V](false)
  /**
   *
   */
  private final val gaussianK = GaussianKernel(KernelArgsGaussian(bandwidth = 0.001, metric))
  /**
   *
   */
  private[this] final def obtainSimilarityMatrix(dataToExplore: AscendingData, approxKNN: AscendingData): parallel.immutable.ParMap[Long, mutable.ArraySeq[(Int, ScalarVector[V], Double)]] = {
    dataToExplore.map{ case (_, (cz, mode, isOriginalDot, _)) => (cz.id,  approxKNN.map{ case (bID, (cz, _, _, _)) => (bID, cz.v, metric.d(mode, cz.v)) }.seq.sortBy(_._3)) }.toMap
  }
  /**
   * Is the mode shifting enough 
   */
  private[this] final def doesItConverged(mode: ScalarVector[V], updatedMode: ScalarVector[V]): Boolean = {
      val modeShift = metric.d(updatedMode, mode)
      modeShift <= epsilon
  }
  /**
   * Gradient ascent work using LSH
   */
  final def run: RDD[Cz[O, ScalarVector[V]]] = {

    /**
    * Initialisation 
    */
    data.persist(persistanceLVL)
    val size =  data.count.toInt
    val maxK = (size / bucketNumber).toInt -1 
    val dim = data.first.v.vector.size
    
    /**
     *
     */
    val bucketRange = 1 to bucketLayers
    /**
     *
     */
    val isOriginalPoint = true
    /**
     *
     */
    val hasConverged = true
    /**
     *
     */
    val localityRDD = UtilSpark.generateDataLocalityLD(data, hasher, bucketNumber, bucketLayers)
    /**
     *
     */
    val readyToGA = localityRDD.map{ case (bID, (cz, isOriginalDot, hasConverged)) => (bID, (cz, cz.v, isOriginalDot, hasConverged)) }.persist(persistanceLVL)
    /**
     *
     */
    def knnGradientAscent(toExplore: AscendingData, approxKNN: AscendingData, toAdd: AscendingData, memoryExpensive: Boolean) = {

      if(memoryExpensive) {
        val simMat = obtainSimilarityMatrix(toExplore, approxKNN)
        val explored = toExplore.map{ case (_, (cz, mode, isOriginalDot, _)) =>
          val knn = simMat(cz.id).take(k).map{ case (_, vector, _) => vector }
          val updatedMode = ClusterBasicOperations.obtainMean(knn)
          val hasConverged = doesItConverged(mode, updatedMode)
          val newIdx = simMat(cz.id).take(knnBucketShift).groupBy(_._1).maxBy{ case (_, nbs) => nbs.size }._1
          (newIdx, (cz, updatedMode, isOriginalDot, hasConverged))
        }
        explored.toIterator ++ toAdd
      }
      else {
        val explored = toExplore.map{ case (_, (cz, mode, isOriginalDot, _)) =>              
          val updatedMode = {
            val distKnnFromCurrentPoint = approxKNN.seq.sortBy{ case (_, (cz, _, _, _)) => metric.d(mode, cz.v) }.map(_._2._1.v)
            val knn = distKnnFromCurrentPoint.take(k)
            ClusterBasicOperations.obtainMean(knn)
          }
          val hasConverged = doesItConverged(mode, updatedMode)
          val newIdx = approxKNN.seq.sortBy{ case (_, (cz, _, _, _)) => metric.d(updatedMode, cz.v) }.take(knnBucketShift).groupBy(_._1).maxBy{ case (_, nbs) => nbs.size }._1
          (newIdx, (cz, updatedMode, isOriginalDot, hasConverged))
        }
        explored.toIterator ++ toAdd
      }
    }
    /**
     *
     */
    def kernelGradientAscent(toExplore: AscendingData, approxKNN: AscendingData, toAdd: AscendingData) = {
      toExplore.map{ case (_, (cz, mode, isOriginalDot, _)) =>  
        val updatedMode = {
          val locality = approxKNN.map{ case (_, (cz, mode, _, _)) => mode }
          gaussianK.obtainMode(mode, locality)
        }
        val hasConverged = doesItConverged(mode, updatedMode)
        val newIdx = approxKNN.seq.sortBy{ case (bidx, (cz, _, _, _)) => metric.d(updatedMode, cz.v) }.take(knnBucketShift).groupBy(_._1).maxBy{ case (_, nbs) => nbs.size }._1
        (newIdx, (cz, updatedMode, isOriginalDot, hasConverged))
     }.toIterator ++ toAdd
    }
    /**
     *
     */
    def gradientAscentInt(readyToGA: RDD[(BucketID, (Cz[O, ScalarVector[V]], Mode, IsOriginalDot, HasConverged))], maxIterations: Int) = {
      /**
       * gradient ascent using KNN kernel
       */
      def knnGA(rdd: RDD[(BucketID, (Cz[O, ScalarVector[V]], Mode, IsOriginalDot, HasConverged))]) = {
        rdd.mapPartitions{ it =>
            val approxKNN = it.toParArray
            val (toExplore, toAdd) =  approxKNN.partition{ case (_, (_, _, isOriginalDot, _)) => isOriginalDot }
            // kernelGradientAscent(toExplore, approxKNN, toAdd)
            knnGradientAscent(toExplore, approxKNN, toAdd, memoryExpensive)
          }
      }
      /**
       * Recursive version faster than imperative one, some test have to be perform on clustert to bee positive
       */
      def functionalStyle = {
        @annotation.tailrec
        def go(i: Int, rdd: RDD[(BucketID, (Cz[O, ScalarVector[V]], Mode, IsOriginalDot, HasConverged))]): RDD[(BucketID, (Cz[O, ScalarVector[V]], Mode, IsOriginalDot, HasConverged))] = {
          if(propConvStopIter == 1D) {
            val orderedLSHRDD =  knnGA(rdd)
            if(i < maxIterations) go(i + 1, orderedLSHRDD)
            else orderedLSHRDD
          }
          else {
            val orderedLSHRDD =  knnGA(rdd).persist(persistanceLVL)
            val convergedDots = orderedLSHRDD.filter{ case (_, (_, _, isOriginalDot, hasConverged)) => isOriginalDot && hasConverged }.count.toDouble
            val continueIteration = convergedDots / size < propConvStopIter
            if(i < maxIterations && continueIteration) go(i + 1, orderedLSHRDD)
            else orderedLSHRDD
          }
        }
        go(0, readyToGA)
      }
      /**
       * Sloweerrr, have to test on cluster
       */
      def imperativeStyle = {
        val lineageRDDs = mutable.ArrayBuffer(readyToGA)
        var ind = 0
        var continueIteration = true
        while(ind < maxIterations && continueIteration) {
          if( propConvStopIter == 1D ) {
            val orderedLSHRDD =  knnGA(lineageRDDs(ind))
            ind += 1
            if(ind < maxIterations) lineageRDDs += orderedLSHRDD.partitionBy(new HashPartitioner(bucketNumber))
            else lineageRDDs += orderedLSHRDD
            orderedLSHRDD.unpersist(false)
          }
          else {
            val orderedLSHRDD =  knnGA(lineageRDDs(ind))
            ind += 1
            val convergedDots = orderedLSHRDD.filter{ case (_, (_, _, isOriginalDot, hasConverged)) => isOriginalDot && hasConverged }.count.toDouble
            continueIteration = convergedDots / size < propConvStopIter
            if(ind < maxIterations && continueIteration) lineageRDDs += orderedLSHRDD.partitionBy(new HashPartitioner(bucketNumber))
            else lineageRDDs += orderedLSHRDD
            orderedLSHRDD.unpersist(false)          
          }
        }
        lineageRDDs.last
      }
      if(fonctionalStyle) functionalStyle else imperativeStyle
    }

    val convergedRDD = gradientAscentInt(readyToGA, maxIterations).filter{ case (_, (_, _, isOriginalDot, _)) => isOriginalDot }

    val finalConvergedRDD = convergedRDD.map{ case (_, (cz, mode, _, _)) => cz.addAlternativeVector(Int.MaxValue, mode) }

    finalConvergedRDD
  }


}
/**
 *
 */
object GradientAscent {
  /**
   * Trains a MS-LSH model using the given set of parameters.
   *
   * @param data : an RDD[(String,Vector)] where String is the ID and Vector the rest of data
   * @param k : number of neighbours to look at during gradient ascent
   * @param knnBucketShift : number of neighbours in KNN search to know if a specific point should be shifted to its neighbor bucket
   * @param epsilon : threshold under which we stop iteration in gradient ascent
   * @param maxIterations : Number of iteration for modes search
   * @param bucketNumber : number of buckets used to compute modes
   * @param nbLabelIter : number of iteration for the labelisation step, it determines the number of final models
   *
   */
  final def train[
    O,
    V <: Seq[Double],
    Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz]
    // Hash <: HashingScalar[V]
  ](
    data: RDD[Cz[O, ScalarVector[V]]],
    k: Int,
    epsilon: Double,
    maxIterations: Int,
    bucketNumber: Int,
    bucketLayers: Int = 1,
    knnBucketShift: Int = 1,
    propConvStopIter: Double = 1D,
    memoryExpensive: Boolean = true,
    persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY,
    fonctionalStyle: Boolean = true
  )(implicit ev: ClassTag[Cz[O, ScalarVector[V]]]): RDD[Cz[O, ScalarVector[V]]] = {
    val lsh = LDLSH[V](data.first.v.vector.size)
    GradientAscent(data, k, knnBucketShift, epsilon, maxIterations, bucketLayers, bucketNumber, propConvStopIter, memoryExpensive, lsh, persistanceLVL, fonctionalStyle).run
  }
}