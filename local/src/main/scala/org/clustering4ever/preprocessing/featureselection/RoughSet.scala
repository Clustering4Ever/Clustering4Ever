package org.clustering4ever.preprocessing.featureselection

/**
 * @author Beck Gaël & Chelly Dagdia Zaineb
 */
import org.clustering4ever.roottraits.{GSimpleVector, GVector, Supervizable}

import scala.collection.{GenSeq, parallel}
import scala.language.higherKinds
import scala.util.Random
/**
 *
 */
trait RoughSet extends RoughSetCommons {
  /**
   * Generate every combinations of features
   */
  protected final def obtainEveryFeaturesCombinations(n: Int): parallel.mutable.ParArray[Array[Int]] = {
    val seqFeats = Array.range(0, n)
    seqFeats.flatMap(seqFeats.combinations).drop(1).par
  }
  /**
   *
   */
  private final def obtainReductSet[O, T, V[A] <: GSimpleVector[A, V[A]], Sz[B, C <: GVector[C]] <: Supervizable[B, C, Sz]](data: GenSeq[Sz[O, V[T]]], indDecisionClasses: Iterable[Array[Long]], everyCombinations: parallel.mutable.ParArray[Array[Int]]): parallel.mutable.ParArray[Array[Int]] = {
    
    val indEveryCombinations = everyCombinations.map( f => (f, obtainIndecability(f, data)) )
    val dependencyAll = indEveryCombinations.map{ case (features, indecability) => (features, dependency(indecability, indDecisionClasses)) }
    var dependencyMax = dependencyAll.maxBy{ case (_, dependency) => dependency }._2
    val allDependencyMax = dependencyAll.filter{ case (_, dependency) => dependency == dependencyMax }
    val maxDependencyMinFeatureNb = allDependencyMax.minBy { case (features, _) => features.size }._1.length
    val allReductSet = allDependencyMax.collect{ case (features, _) if features.length == maxDependencyMinFeatureNb => features }
    allReductSet
  }
  /**
   *
   */
  protected final def selectAmongReductSets1(reductSets: parallel.mutable.ParArray[Array[Int]], k: Int): Array[Int] = {
    reductSets.flatten.groupBy(identity).seq.toArray.sortBy(_._2.size).take(k).map(_._1)
  }
  /**
   *
   */
  protected final def selectAmongReductSets2(reductSets: parallel.mutable.ParArray[Array[Int]], p: Double) = {
    require(p >= 0D && p <= 1D)
    reductSets.flatten.groupBy(identity).toSeq.filter{ case (_, agg) => agg.size.toDouble / reductSets.size >= p }.map(_._1)
  }
  /**
   * Rought Set feature selection classical algorithm
   */
  protected final def roughSet[O, T, V[A] <: GSimpleVector[A, V[A]], Sz[B, C <: GVector[C]] <: Supervizable[B, C, Sz]](data: GenSeq[Sz[O, V[T]]]): GenSeq[Array[Int]] = {
    val indDecisionClasses: Iterable[Array[Long]] = generateIndecidabilityDecisionClasses(data)
    val everyCombinations: parallel.mutable.ParArray[Array[Int]] = obtainEveryFeaturesCombinations(data.head.v.vector.length)
    val allReductSet = obtainReductSet(data, indDecisionClasses, everyCombinations)
    allReductSet    
  }
  /**
   * Pure scala functions to apply on each node locally
   */
  protected final def roughSetPerBucketOfFeatures[O, T, V[A] <: GSimpleVector[A, V[A]], Sz[B, C <: GVector[C]] <: Supervizable[B, C, Sz]](data: GenSeq[Sz[O, V[T]]], columnsOfFeats: Seq[Seq[Int]], columnToCompute: Option[GenSeq[Int]] = None): GenSeq[Int] = {
    
    val computedColumns: GenSeq[Int] = if(columnToCompute.isDefined) columnToCompute.get else parallel.mutable.ParArray.range(0, columnsOfFeats.size)

    val randomReductSetPerFeaturesBucket = computedColumns.map{ columnIdx =>

      val dataPerFeat = data.map(_.obtainOneBucket(columnIdx))
      val originalFeatures = columnsOfFeats(columnIdx)
      val originalFeatIdByTmpFeatId = originalFeatures.zipWithIndex.map(_.swap).toMap
      val everyCombinations = obtainEveryFeaturesCombinations(originalFeatures.size)      
      val indDecisionClasses = generateIndecidabilityDecisionClasses(dataPerFeat)
      val allReductSet = obtainReductSet(dataPerFeat, indDecisionClasses, everyCombinations)

      if(true) {
        // We select randomly one reduct set and give it its original features value
        allReductSet(Random.nextInt(allReductSet.size)).map(originalFeatIdByTmpFeatId)
      }
      else {
        // Add an alternative step by selecting features that appears the most in the various reduct set
        selectAmongReductSets1(allReductSet, 4)
        // selectAmongReductSets2(allReductSet, 0.2)
      }
    }

    randomReductSetPerFeaturesBucket.flatten
  
  }

}
/**
 *
 */
object RoughSet extends RoughSet {

  import org.clustering4ever.util.ScalaCollectionImplicits._
  /**
   * Rought Set feature selection classical algorithm
   */
  final def fit(data: GenSeq[(Array[Int], Int)]): GenSeq[Array[Int]] = roughSet(rawDataToSupervizable(data))
  /**
   * Rought Set feature selection classical algorithm
   */
  final def fit[O, T, V[A] <: GSimpleVector[A, V[A]], Sz[B, C <: GVector[C]] <: Supervizable[B, C, Sz]](data: GenSeq[Sz[O, V[T]]])(implicit di: DummyImplicit): GenSeq[Array[Int]] = roughSet(data)
  /**
   *
   */
  final def runHeuristic[O, T, V[A] <: GSimpleVector[A, V[A]], Sz[B, C <: GVector[C]] <: Supervizable[B, C, Sz]](data: GenSeq[Sz[O, V[T]]], columnsOfFeats: Seq[Seq[Int]], columnToCompute: Option[GenSeq[Int]] = None): GenSeq[Int] = roughSetPerBucketOfFeatures(data, columnsOfFeats, columnToCompute)
  /**
   *
   */
  // def easyRunHeuristic[T, S[X] <: Seq[X]](data: GenSeq[(V[T], Int)], numberOfBucket: Int = 0): GenSeq[Int] = {
  //   val autoBucket = if( numberOfBucket <= 1 ) data.head._1.size / 8 + 1 else numberOfBucket
  //   val (readyToLearn, columnsOfFeats) = Util.prepareGsForRoughSetHeuristic(data, autoBucket)
  //   roughSetPerBucketOfFeatures(readyToLearn, columnsOfFeats)
  // }

}