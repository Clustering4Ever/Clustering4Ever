package org.clustering4ever.spark.preprocessing.rst
/**
 * @author Beck Gaël & Chelly Dagdia Zaineb
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.collection.{immutable, mutable, parallel}
import scala.util.Random
import scala.annotation.meta.param
import org.clustering4ever.preprocessing.{DFCL, HDFCL}
import org.clustering4ever.scala.preprocessing.rst.RoughSet
/**
 *
 */
class DistributedRoughSet(@(transient @param) sc:SparkContext) extends RoughSet with DistributedRoughSetCommons {
  /**
   * RoughSet distributed classic version
   * Don't forget complexity is in O(F!) with F the number of Features, distributed system won't help enough facing this kind of complexity
   */
  def run[ID: Numeric : ClassTag, T: ClassTag, V[T] <: Seq[T]](data: RDD[DFCL[ID, V[T]]], everyCombinationsO: Option[mutable.ArraySeq[mutable.ArrayBuffer[Int]]] = None)(implicit ct: ClassTag[V[T]]) = {
   
    val everyCombinations = if( everyCombinationsO.isDefined ) everyCombinationsO.get else obtainEveryFeaturesCombinations(data.first.originalVector.size).seq

    val indDecisionClasses = sc.broadcast(generateIndecidabilityDecisionClassesD(data))
    val indAllCombinations = sc.parallelize(everyCombinations).map( f => (f, obtainIndecabilityD(f, data)) )
    val dependencyAll = indAllCombinations.map{ case (comboFeatures, indecability) => (comboFeatures, dependency(indecability, indDecisionClasses.value)) }.cache
    var dependencyMax = dependencyAll.max()(Ordering[Int].on(_._2))._2
    val allDependencyMax = dependencyAll.filter{ case (_, dependency) => dependency == dependencyMax }.cache
    val maxDependencyMinFeatureNb = allDependencyMax.min()(Ordering[Int].on(_._1.size))._1.size
    val allReductSet = allDependencyMax.collect{ case (features, _) if features.size == maxDependencyMinFeatureNb => features }.collect
    dependencyAll.unpersist(true)
    allDependencyMax.unpersist(true)

    allReductSet    
  }
  /*
   *  RoughSet working by range of features
   */
  def runHeuristic[ID: Numeric : ClassTag, U: ClassTag, T[U] <: Seq[U], V[T] <: Seq[T]](data: RDD[HDFCL[ID, U, T, V]], columnsOfFeats: Seq[Seq[Int]])(implicit ct1: ClassTag[T[U]], ct2: ClassTag[V[T[U]]]): mutable.Buffer[Int] = {

    val nbColumns = columnsOfFeats.size
    val dataBC = sc.broadcast(data.collect.par)
    
    sc.parallelize(0 until 8888, nbColumns).mapPartitionsWithIndex{ (idxp, _) =>
      val dataPerFeat = dataBC.value.map(_.getOneFeaturesBucket(idxp))
      val originalFeatures = columnsOfFeats(idxp)
      val originalFeatIdByTmpFeatId = originalFeatures.zipWithIndex.map(_.swap).toMap      
      val allReductSet = roughSet(dataPerFeat)
      allReductSet(Random.nextInt(allReductSet.size)).map(originalFeatIdByTmpFeatId).toIterator
    }
    .collect
    .toBuffer
  }

}