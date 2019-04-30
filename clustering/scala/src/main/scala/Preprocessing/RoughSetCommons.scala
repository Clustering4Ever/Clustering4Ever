package org.clustering4ever.scala.preprocessing.rst
/**
 * @author Beck Gaël & Chelly Dagdia Zaineb
 */
import scala.language.higherKinds
import scala.collection.{immutable, mutable, GenSeq, GenIterable, Iterable}
import scala.reflect.ClassTag
import org.clustering4ever.supervizables.Supervizable
import org.clustering4ever.vectors.{GVector, GSimpleVector}
/**
 *
 */
trait RoughSetCommons extends Serializable {
  /**
   * Define KeyValueExtract function
   */
  protected final def keyValueExtract[T](f: Array[Int], vector: Array[T]): immutable.IndexedSeq[T] = (0 until f.size).map( i => vector(f(i)) )
  /**
   * Compute dependency
   */
  protected final def dependency(indecability: GenIterable[Array[Long]], indDecisionClasses: Iterable[Array[Long]]) = {
    var dependency = 0
    indDecisionClasses.foreach( indCl => indecability.foreach( i => if(i.intersect(indCl).size != i.size) dependency += i.size ) )
    dependency
  }
  /**
   * Define a function to calculate the Indecability of each element in the list indDecisionClasses
   */
  protected final def obtainIndecability[O, T, V[A] <: GSimpleVector[A, V[A]], Sz[B, C <: GVector[C]] <: Supervizable[B, C, Sz]](f: Array[Int], data: GenSeq[Sz[O, V[T]]]): GenIterable[Array[Long]] = {
    data.groupBy( l => keyValueExtract(f, l.v.vector) ).map{ case (listValues, aggregates) => aggregates.map(_.id).seq.toArray }
  }
  /**
   *
   */
  protected final def generateIndecidabilityDecisionClasses[O, T, S[X] <: Seq[X], V[A] <: GSimpleVector[A, V[A]], Sz[B, C <: GVector[C]] <: Supervizable[B, C, Sz]](data: GenSeq[Sz[O, V[T]]]): Iterable[Array[Long]] = {
    data.groupBy(_.label).map{ case (label, aggregate) => aggregate.map(_.id).seq.toArray }.seq
  }

}