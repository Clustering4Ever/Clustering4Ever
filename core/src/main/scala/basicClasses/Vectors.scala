package org.clustering4ever.scala.vectors
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import spire.math.{Numeric => SNumeric}
import scala.collection.mutable
import org.clustering4ever.util.SumVectors

sealed trait GVector extends Serializable

trait GSimpleVector[N, V <: Seq[N]] extends GVector {
	val vector: V
}

trait GBinaryVector[Vb <: Seq[Int]] extends GSimpleVector[Int, Vb]

trait GScalarVector[Vs <: Seq[Double]] extends GSimpleVector[Double, Vs]

trait GMixtVector[Vb <: Seq[Int], Vs <: Seq[Double]] extends GVector {
	val binary: Vb
	val scalar: Vs
}

case class BinaryVector[Vb <: Seq[Int]](val vector: Vb) extends GBinaryVector[Vb]

case class ScalarVector[Vs <: Seq[Double]](val vector: Vs) extends GScalarVector[Vs]

case class MixtVector[Vb <: Seq[Int], Vs <: Seq[Double]](val binary: Vb, val scalar: Vs) extends GMixtVector[Vb, Vs]