package clustering4ever.spark.indexes

import org.apache.spark.util.AccumulatorV2
import scala.collection.mutable

case class NmiAccumulator(initialValue: Array[Array[Double]], x: Int, y: Int) extends AccumulatorV2[Array[Array[Double]], Array[Array[Double]]]
{
  type NmiAccumulatorType = Array[Array[Double]]

  private var nmiAccumulator: NmiAccumulatorType = initialValue

  def value = nmiAccumulator

  def isZero = value.forall{ centroid => centroid.forall(_ == 0D) }

  def reset: Unit = nmiAccumulator = Array.fill(x)(Array.fill(y)(0D))

  def add(m1: NmiAccumulatorType): Unit = nmiAccumulator = m1.zip(nmiAccumulator).map{ case (v1, v2) => v1.indices.map( i => v1(i) + v2(i) ).toArray }

  def copy: AccumulatorV2[NmiAccumulatorType, NmiAccumulatorType] = NmiAccumulator(value, x, y)

  def merge(otherAccum: AccumulatorV2[NmiAccumulatorType, NmiAccumulatorType]): Unit = add(otherAccum.value)
 
  def addOne(x: Int, y: Int): Unit = nmiAccumulator(x)(y) += 1D

  def set(newInitialValue: NmiAccumulatorType) = nmiAccumulator = newInitialValue
}