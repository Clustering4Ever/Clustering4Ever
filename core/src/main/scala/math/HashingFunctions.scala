package org.clustering4ever.hashing
/**
 * @author Beck Gaël
 */
import scala.collection.{mutable, immutable}
import scala.util.Random
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixtVector}
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
 *
 */
trait Hashing[V <: GVector[V]] extends GenericHashing[V]
/**
 * Trait for continuous data hashing
 */
trait HashingScalar[V <: Seq[Double]] extends Hashing[ScalarVector[V]]
/**
 * Trait for binary data hashing
 */
trait HashingBinary[V <: Seq[Int]] extends Hashing[BinaryVector[V]]
/**
 * Trait for mixt data hashing
 */
trait HashingMixt[Vb <: Seq[Int], Vs <: Seq[Double]] extends Hashing[MixtVector[Vb, Vs]]
/**
 * A basic implementation of Locality Sensitive Hashing for low dimensions vectors
 */
case class LDLSH[V <: Seq[Double]](val dim: Int, val w: Double = 1D) extends HashingScalar[V] {
  /**
   *
   */
  val b = Random.nextDouble * w
  /**
   * Create a random vector where component are taken on normal law N(0,1) for LSH
   */
  val hashVector = mutable.ArrayBuffer.fill(dim)(Random.nextGaussian)
  /**
   *  Generate the hash value for a given vector v depending on w, b, hashVector
   */
  def hf(v: V): Double = {
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
  def hf(v: ScalarVector[V]): Double = hf(v.vector)
}
/**
 * Trait regrouping various hashing methods
 */
trait RealHasher extends Serializable {
  def hf[V <: Seq[Double]](v: V): Double
}
/**
 *
 */
trait RealSpacePartionner extends Serializable {
  def obtainBucketPerLevel[V <: Seq[Double]](v: V): immutable.IndexedSeq[Int]
}
/**
 *
 */
class HDLSH(val l: Int, val dim: Int, buckets: Int, w: Double = 1D) extends RealSpacePartionner {

  val hvs = (0 until l).map( hfid => (mutable.ArrayBuffer.fill(dim)(Random.nextGaussian), Random.nextDouble * w, hfid) )

  val quasiExtremum = dim.toDouble
  val bucketRange = (2D * quasiExtremum) / buckets
  val bucketsLimits = (0 until buckets).map( l => - quasiExtremum + bucketRange * l ).zipWithIndex
  /**
   *  Generate the hash value for a given vector x depending on w, b, hashVector
   */
  def hf[V <: Seq[Double]](v: V, j: Int): Double = {
    @annotation.tailrec
    def go(s: Double, i: Int): Double = {
      if(i < v.size) go(s + v(i) * hvs(j)._1(i), i + 1)
      else s
    }
    (go(0D, 0) + hvs(j)._2) / w
  }

  def obtainBucketPerLevel[V <: Seq[Double]](v: V): immutable.IndexedSeq[Int] = {
    hvs.map{ case (rv, _, hfid) => 
      val bucketID = bucketsLimits.find{ case (th, _) => hf(v, hfid) <= th }
      if(bucketID.isDefined) bucketID.get._2 else buckets
    }
  }
}