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
  protected final def keyValueExtract[T, S[X] <: Seq[X]](f: mutable.ArrayBuffer[Int], vector: S[T]): immutable.IndexedSeq[T] = (0 until f.size).map( i => vector(f(i)) )
  /**
   * Compute dependency
   */
  protected final def dependency(indecability: GenIterable[Seq[Long]], indDecisionClasses: Iterable[Seq[Long]]) = {
    var dependency = 0
    indDecisionClasses.foreach( indCl => indecability.foreach( i => if(i.intersect(indCl).size != i.size) dependency += i.size ) )
    dependency
  }
  /**
   * Define a function to calculate the Indecability of each element in the list indDecisionClasses
   */
  protected final def obtainIndecability[O, T, S[X] <: Seq[X], V[A, B[X] <: Seq[X]] <: GSimpleVector[A, B[A], V[A, B]], Sz[B, C <: GVector[C]] <: Supervizable[B, C, Sz]](f: mutable.ArrayBuffer[Int], data: GenSeq[Sz[O, V[T, S]]]): GenIterable[Seq[Long]] = {
    data.groupBy( l => keyValueExtract(f, l.v.vector) ).map{ case (listValues, aggregates) => aggregates.map(_.id).seq }
  }
  /**
   *
   */
  protected final def generateIndecidabilityDecisionClasses[O, T, S[X] <: Seq[X], V[A, B[X] <: Seq[X]] <: GSimpleVector[A, B[A], V[A, B]], Sz[B, C <: GVector[C]] <: Supervizable[B, C, Sz]](data: GenSeq[Sz[O, V[T, S]]]): Iterable[Seq[Long]] = {
    data.groupBy(_.label).map{ case (label, aggregate) => aggregate.map(_.id).seq }.seq
  }

}