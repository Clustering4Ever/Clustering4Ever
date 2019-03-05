package org.clustering4ever.hashing
/**
 * @author Beck Gaël
 */
import scala.collection.{mutable, immutable}
import scala.util.Random
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixedVector}
/**
 *
 */
trait HashingArgs extends Serializable
/**
 *
 */
trait GenericHashing[O] extends Serializable {
  /**
   * The hashing function (hf)
   * @return a hashing value as a Double
   */
  def hf(o: O): Double
}
/**
 * @tparam V
 */
trait Hashing[V <: GVector[V]] extends GenericHashing[V]
/**
 * @tparam V
 * Trait for continuous data hashing
 */
trait HashingScalar[V <: Seq[Double]] extends Hashing[ScalarVector[V]] {
  /**
   *
   */
  def hf(v: V): Double
}
/**
 * @tparam V
 * Trait for binary data hashing
 */
trait HashingBinary[V <: Seq[Int]] extends Hashing[BinaryVector[V]]
/**
 * @tparam V
 * Trait for mixt data hashing
 */
trait HashingMixed[Vb <: Seq[Int], Vs <: Seq[Double]] extends Hashing[MixedVector[Vb, Vs]]
/**
 * @tparam V
 * A basic implementation of Locality Sensitive Hashing for low dimensions vectors
 */
final case class LDLSH[V <: Seq[Double]](final val dim: Int, final val w: Double = 1D) extends HashingScalar[V] {
  require(dim <= 3, println("This hashfunction only works well on law dimentional space"))
  /**
   *
   */
  final val b = Random.nextDouble * w
  /**
   * Create a random vector where component are taken on normal law N(0,1) for LSH
   */
  final val hashVector = mutable.ArrayBuffer.fill(dim)(Random.nextGaussian)
  /**
   *  Generate the hash value for a given vector v depending on w, b, hashVector
   */
  final def hf(v: V): Double = {
    @annotation.tailrec
    def go(s: Double, i: Int): Double = {
      if(i < v.size) go(s + v(i) * hashVector(i), i + 1)
      else s
    }
    (go(0D, 0) + b) / w
  }
  /**
   *  Generate the hash value for a given vector v depending on w, b, hashVector
   */
  final def hf(v: ScalarVector[V]): Double = hf(v.vector)
}
/**
 *
 */
trait RealSpacePartionner[V <: Seq[Double]] extends Serializable {
  def obtainBucketPerLevel(v: V): immutable.IndexedSeq[Int]
}
/**
 *
 */
final case class HDLSH[V <: Seq[Double]](final val l: Int, final val dim: Int, buckets: Int, w: Double = 1D) extends RealSpacePartionner[V] {

  final val hvs = (0 until l).map( hfid => (mutable.ArrayBuffer.fill(dim)(Random.nextGaussian), Random.nextDouble * w, hfid) )

  private final val quasiExtremum = dim.toDouble
  final val bucketRange = (2D * quasiExtremum) / buckets
  final val bucketsLimits = (0 until buckets).map( l => - quasiExtremum + bucketRange * l ).zipWithIndex
  /**
   *  Generate the hash value for a given vector x depending on w, b, hashVector
   */
  final def hf[V <: Seq[Double]](v: V, j: Int): Double = {
    @annotation.tailrec
    def go(s: Double, i: Int): Double = {
      if(i < v.size) go(s + v(i) * hvs(j)._1(i), i + 1)
      else s
    }
    (go(0D, 0) + hvs(j)._2) / w
  }

  final def obtainBucketPerLevel(v: V): immutable.IndexedSeq[Int] = {
    hvs.map{ case (rv, _, hfid) => 
      val bucketID = bucketsLimits.find{ case (th, _) => hf(v, hfid) <= th }
      if(bucketID.isDefined) bucketID.get._2 else buckets
    }
  }
}