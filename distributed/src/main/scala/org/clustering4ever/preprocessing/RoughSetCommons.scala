package org.clustering4ever.preprocessing

/**
 * @author Beck Gaël & Chelly Dagdia Zaineb
 */
import org.apache.spark.rdd.RDD
import org.clustering4ever.preprocessing.featureselection.RoughSetCommons
import org.clustering4ever.roottraits.{GSimpleVector, GVector, Supervizable}

import scala.collection.mutable
import scala.language.higherKinds
import scala.reflect.ClassTag
/**
 *
 */
trait DistributedRoughSetCommons extends RoughSetCommons {
  /**
   * Define a function to calculate the IND of each element in the list indDecisionClasses
   */
  protected final def obtainIndecabilityD[O, T : ClassTag, V[A] <: GSimpleVector[A, V[A]], Sz[B, C <: GVector[C]] <: Supervizable[B, C, Sz]](f: Array[Int], data: RDD[Sz[O, V[T]]]) = {

    val neutralElement = mutable.ArrayBuffer.empty[Long]
    def addToBuffer(buff: mutable.ArrayBuffer[Long], elem: Long) = buff += elem
    def aggregateBuff(buff1: mutable.ArrayBuffer[Long], buff2: mutable.ArrayBuffer[Long]) = buff1 ++= buff2
    
    data.map{ case sup => (keyValueExtract(f, sup.v.vector), sup.id) }.aggregateByKey(neutralElement)(addToBuffer, aggregateBuff).map{ case (listValues, objectsId) => objectsId.toArray }.collect
  }
  /**
   *
   */
  protected final def generateIndecidabilityDecisionClassesD[O, T : ClassTag, V[A] <: GSimpleVector[A, V[A]], Sz[B, C <: GVector[C]] <: Supervizable[B, C, Sz]](data: RDD[Sz[O, V[T]]]): Array[Array[Long]] = {

    val neutralElement = mutable.ArrayBuffer.empty[Long]
    def addToBuffer(buff: mutable.ArrayBuffer[Long], elem: Long) = buff += elem
    def aggregateBuff(buff1: mutable.ArrayBuffer[Long], buff2: mutable.ArrayBuffer[Long]) = buff1 ++= buff2
  
    val indDecisionClass = data.map{ case sup => (sup.label, sup.id) }.aggregateByKey(neutralElement)(addToBuffer, aggregateBuff).map{ case (label, objects) => objects.toArray }.collect
    indDecisionClass
  
  }
}