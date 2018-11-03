package clustering4ever.spark.indexes

import org.apache.spark.util.AccumulatorV2
import scala.collection.mutable
/**
 *
 */
case class NmiAccumulator(initialValue: mutable.ArrayBuffer[mutable.ArrayBuffer[Double]], x: Int, y: Int) extends AccumulatorV2[mutable.ArrayBuffer[mutable.ArrayBuffer[Double]], mutable.ArrayBuffer[mutable.ArrayBuffer[Double]]]
{
  /**
   *
   */
  type NmiAccumulatorType = mutable.ArrayBuffer[mutable.ArrayBuffer[Double]]
  /**
   *
   */
  private var nmiAccumulator: NmiAccumulatorType = initialValue
  /**
   *
   */
  def value = nmiAccumulator
  /**
   *
   */
  def isZero = value.forall{ centroid => centroid.forall(_ == 0D) }
  /**
   *
   */
  def reset: Unit = nmiAccumulator = mutable.ArrayBuffer.fill(x)(mutable.ArrayBuffer.fill(y)(0D))
  /**
   *
   */
  def add(m1: NmiAccumulatorType): Unit = nmiAccumulator = m1.zip(nmiAccumulator).map{ case (v1, altVectors) => v1.zip(altVectors).map( x => x._1 + x._2 ) }
  /**
   *
   */
  def copy: AccumulatorV2[NmiAccumulatorType, NmiAccumulatorType] = NmiAccumulator(value, x, y)
  /**
   *
   */
  def merge(otherAccum: AccumulatorV2[NmiAccumulatorType, NmiAccumulatorType]): Unit = add(otherAccum.value)
  /**
   *
   */ 
  def addOne(x: Int, y: Int): Unit = nmiAccumulator(x)(y) += 1D
  /**
   *
   */
  def set(newInitialValue: NmiAccumulatorType) = nmiAccumulator = newInitialValue
}