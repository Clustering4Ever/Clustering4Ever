package org.clustering4ever.spark.preprocessing.rst
/**
 * @author Beck Gaël & Chelly Dagdia Zaineb
 */
import scala.language.higherKinds
import scala.collection.{immutable, mutable, GenSeq, GenIterable, Iterable, parallel}
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.rdd.RDD
import scala.reflect.ClassTag
import org.clustering4ever.preprocessing.{DFCL, HDFCL}
import org.clustering4ever.scala.preprocessing.rst.RoughSetCommons
/**
 *
 */
trait DistributedRoughSetCommons extends RoughSetCommons {
  /**
   * Define a function to calculate the IND of each element in the list indDecisionClasses
   */
  protected def obtainIndecabilityD[ID: ClassTag, T: ClassTag, V[X] <: Seq[X]](f: mutable.ArrayBuffer[Int], data: RDD[DFCL[ID, V[T]]])(implicit ct: ClassTag[V[T]]) = {

    val neutralElement = mutable.ArrayBuffer.empty[ID]
    def addToBuffer(buff: mutable.ArrayBuffer[ID], elem: ID) = buff += elem
    def aggregateBuff(buff1: mutable.ArrayBuffer[ID], buff2: mutable.ArrayBuffer[ID]) = buff1 ++= buff2
    
    data.map{ case dfcl => (keyValueExtract(f, dfcl.workingVector), dfcl.id) }.aggregateByKey(neutralElement)(addToBuffer, aggregateBuff).map{ case (listValues, objectsId) => objectsId }.collect
  }
  /**
   *
   */
  protected def generateIndecidabilityDecisionClassesD[ID: ClassTag, T: ClassTag, V[X] <: Seq[X]](data: RDD[DFCL[ID, V[T]]]): mutable.Buffer[mutable.ArrayBuffer[ID]] = {

    val neutralElement = mutable.ArrayBuffer.empty[ID]
    def addToBuffer(buff: mutable.ArrayBuffer[ID], elem: ID) = buff += elem
    def aggregateBuff(buff1: mutable.ArrayBuffer[ID], buff2: mutable.ArrayBuffer[ID]) = buff1 ++= buff2
  
    val indDecisionClass = data.map{ case dfcl => (dfcl.label, dfcl.id) }.aggregateByKey(neutralElement)(addToBuffer, aggregateBuff).map{ case (label, objects) => objects }.collect.toBuffer
    indDecisionClass
  
  }
}