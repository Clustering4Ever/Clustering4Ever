package org.clustering4ever.preprocessing.featureselection

/**
 * @author Beck Gaël & Chelly Dagdia Zaineb
 */
import org.clustering4ever.roottraits.{GSimpleVector, GVector, Supervizable}

import scala.collection.{GenIterable, GenSeq, Iterable, immutable}
import scala.language.higherKinds
/**
 *
 */
trait RoughSetCommons extends Serializable {
  /**
   * Define KeyValueExtract function
   */
  protected final def keyValueExtract[T](f: Array[Int], vector: Array[T]): immutable.IndexedSeq[T] = f.indices.map(i => vector(f(i)) )
  /**
   * Compute dependency
   */
  protected final def dependency(indecability: GenIterable[Array[Long]], indDecisionClasses: Iterable[Array[Long]]) = {
    var dependency = 0
    indDecisionClasses.foreach( indCl => indecability.foreach( i => if(i.intersect(indCl).length != i.length) dependency += i.length ) )
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