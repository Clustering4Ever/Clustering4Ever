package org.clustering4ever.scala.preprocessing.rst
/**
 * @author Beck Gaël & Chelly Dagdia Zaineb
 */
import scala.language.higherKinds
import scala.collection.{immutable, mutable, GenSeq, GenIterable, Iterable}
import scala.reflect.ClassTag
import org.clustering4ever.preprocessing.{DFCL, HDFCL}
/**
 *
 */
trait RoughSetCommons extends Serializable {
  /**
   * Define KeyValueExtract function
   */
  protected def keyValueExtract[T, V[T] <: Seq[T]](f: mutable.ArrayBuffer[Int], vector: V[T]): immutable.IndexedSeq[T] = (0 until f.size).map( i => vector(f(i)) )
  /**
   * Compute dependency
   */
  protected def dependency[ID: Numeric](indecability: GenIterable[Seq[ID]], indDecisionClasses: Iterable[Seq[ID]]) = {
    var dependency = 0
    indDecisionClasses.foreach( indCl => indecability.foreach( i => if( i.intersect(indCl).size != i.size ) dependency += i.size ) )
    dependency
  }
  /**
   * Define a function to calculate the Indecability of each element in the list indDecisionClasses
   */
  protected def obtainIndecability[ID: Numeric, T, V[T] <: Seq[T]](f: mutable.ArrayBuffer[Int], data: GenSeq[DFCL[ID, V[T]]]): GenIterable[Seq[ID]] = {
    data.groupBy( l => keyValueExtract(f, l.originalVector) ).map{ case (listValues, aggregates) => aggregates.map(_.id).seq }
  }
  /**
   *
   */
  protected def generateIndecidabilityDecisionClasses[ID: Numeric, T, V[T] <: Seq[T]](data: GenSeq[DFCL[ID, V[T]]]): Iterable[Seq[ID]] = {
    data.groupBy(_.label).map{ case (label, aggregate) => aggregate.map(_.id).seq }.seq
  }

}