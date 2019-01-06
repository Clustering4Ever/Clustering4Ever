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
import org.clustering4ever.vectors.{SupervizedVector, GVector}
/**
 *
 */
trait DistributedRoughSetCommons extends RoughSetCommons {
  /**
   * Define a function to calculate the IND of each element in the list indDecisionClasses
   */
  protected def obtainIndecabilityD[ID : ClassTag, O, T : ClassTag, V[X] <: Seq[X], Sz[A, B, C <: GVector[C]] <: Supervizable[A, B, C, Sz]](f: mutable.ArrayBuffer[Int], data: RDD[Sz[ID, O, SupervizedVector[T, V]]])(implicit ct: ClassTag[V[T]]) = {

    val neutralElement = mutable.ArrayBuffer.empty[ID]
    def addToBuffer(buff: mutable.ArrayBuffer[ID], elem: ID) = buff += elem
    def aggregateBuff(buff1: mutable.ArrayBuffer[ID], buff2: mutable.ArrayBuffer[ID]) = buff1 ++= buff2
    
    data.map{ case sup => (keyValueExtract(f, sup.v.vector), sup.id) }.aggregateByKey(neutralElement)(addToBuffer, aggregateBuff).map{ case (listValues, objectsId) => objectsId }.collect
  }
  /**
   *
   */
  protected def generateIndecidabilityDecisionClassesD[ID : ClassTag, O, T : ClassTag, V[X] <: Seq[X], Sz[A, B, C <: GVector[C]] <: Supervizable[A, B, C, Sz]](data: RDD[Sz[ID, O, SupervizedVector[T, V]]]): mutable.Buffer[mutable.ArrayBuffer[ID]] = {

    val neutralElement = mutable.ArrayBuffer.empty[ID]
    def addToBuffer(buff: mutable.ArrayBuffer[ID], elem: ID) = buff += elem
    def aggregateBuff(buff1: mutable.ArrayBuffer[ID], buff2: mutable.ArrayBuffer[ID]) = buff1 ++= buff2
  
    val indDecisionClass = data.map{ case sup => (sup.label, sup.id) }.aggregateByKey(neutralElement)(addToBuffer, aggregateBuff).map{ case (label, objects) => objects }.collect.toBuffer
    indDecisionClass
  
  }
}