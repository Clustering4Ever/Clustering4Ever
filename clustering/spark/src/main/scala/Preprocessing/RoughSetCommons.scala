package org.clustering4ever.spark.preprocessing.rst
/**
 * @author Beck Gaël & Chelly Dagdia Zaineb
 */
import scala.language.higherKinds
import scala.collection.{immutable, mutable, GenSeq, GenIterable, Iterable, parallel}
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.rdd.RDD
import scala.reflect.ClassTag
import org.clustering4ever.scala.preprocessing.rst.RoughSetCommons
import org.clustering4ever.supervizables.Supervizable
import org.clustering4ever.vectors.{GVector, GSimpleVector}
/**
 *
 */
trait DistributedRoughSetCommons extends RoughSetCommons {
  /**
   * Define a function to calculate the IND of each element in the list indDecisionClasses
   */
  protected final def obtainIndecabilityD[O, T : ClassTag, S[X] <: Seq[X], V[A, B[X] <: Seq[X]] <: GSimpleVector[A, B[A], V[A, B]], Sz[B, C <: GVector[C]] <: Supervizable[B, C, Sz]](f: mutable.ArrayBuffer[Int], data: RDD[Sz[O, V[T, S]]])(implicit ct: ClassTag[S[T]]) = {

    val neutralElement = mutable.ArrayBuffer.empty[Long]
    def addToBuffer(buff: mutable.ArrayBuffer[Long], elem: Long) = buff += elem
    def aggregateBuff(buff1: mutable.ArrayBuffer[Long], buff2: mutable.ArrayBuffer[Long]) = buff1 ++= buff2
    
    data.map{ case sup => (keyValueExtract(f, sup.v.vector), sup.id) }.aggregateByKey(neutralElement)(addToBuffer, aggregateBuff).map{ case (listValues, objectsId) => objectsId }.collect
  }
  /**
   *
   */
  protected final def generateIndecidabilityDecisionClassesD[O, T : ClassTag, S[X] <: Seq[X], V[A, B[X] <: Seq[X]] <: GSimpleVector[A, B[A], V[A, B]], Sz[B, C <: GVector[C]] <: Supervizable[B, C, Sz]](data: RDD[Sz[O, V[T, S]]]): mutable.Buffer[mutable.ArrayBuffer[Long]] = {

    val neutralElement = mutable.ArrayBuffer.empty[Long]
    def addToBuffer(buff: mutable.ArrayBuffer[Long], elem: Long) = buff += elem
    def aggregateBuff(buff1: mutable.ArrayBuffer[Long], buff2: mutable.ArrayBuffer[Long]) = buff1 ++= buff2
  
    val indDecisionClass = data.map{ case sup => (sup.label, sup.id) }.aggregateByKey(neutralElement)(addToBuffer, aggregateBuff).map{ case (label, objects) => objects }.collect.toBuffer
    indDecisionClass
  
  }
}