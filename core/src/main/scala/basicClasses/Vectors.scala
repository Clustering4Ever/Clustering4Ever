package org.clustering4ever.vectors
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.mutable
import org.clustering4ever.util.SumVectors
/**
 * The Generic Vector trait
 * If you want to apply algorithms on other vector nature as time series, you've just need to write a case class which inherit GVector which describe your vectorization withou forget the corresponding distance which inherit Distance
 * @tparam Self the concrete implementation of GVector
 */
trait GVector[Self <: GVector[Self]] extends Serializable {
	/**
	 *
	 */
	this: Self =>
	/**
	 * @return the same Vector nature with selected features
	 */
	def pickFeatures(indices: Int*): Self
}
/**
 * @tparam N the type object composing the vector
 * @tparam V the vector type
 */
trait GSimpleVector[N, V <: Seq[N], Self <: GSimpleVector[N, V, Self]] extends GVector[Self] {
	this: Self =>
	/**
	 * A vector taking the form of Seq[N] for any N
	 */
	val vector: V
}
/**
 *
 */
trait GBinaryVector[Vb <: Seq[Int], Self <: GBinaryVector[Vb, Self]] extends GSimpleVector[Int, Vb, Self] {
	this: Self =>
}
/**
 *
 */
trait GScalarVector[Vs <: Seq[Double], Self <: GScalarVector[Vs, Self]] extends GSimpleVector[Double, Vs, Self] {
	this: Self =>
}
/**
 *
 */
trait GMixedVector[Vb <: Seq[Int], Vs <: Seq[Double], Self <: GMixedVector[Vb, Vs, Self]] extends GVector[Self] {
	this: Self =>
	/**
	 * The binary part of this mixt vector as <: Seq[Int] 
	 */
	val binary: Vb
	/**
	 * The scalar part of this mixt vector as <: Seq[Double] 
	 */
	val scalar: Vs
}
/**
 * Vector for binary data taking represented as a vector on {0, 1}<sup>d</sup>
 * @tparam Vb the type of this vector
 */
final case class BinaryVector[Vb <: Seq[Int]](final val vector: Vb) extends GBinaryVector[Vb, BinaryVector[Vb]] {

	final def pickFeatures(indices: Int*): BinaryVector[Vb] = {
		BinaryVector{
			val builder = vector.genericBuilder[Int].asInstanceOf[mutable.Builder[Int, Vb]]
			builder.sizeHint(indices.size)
			indices.foreach( i => builder += vector(i) )
			builder.result
		}
	}
}
/**
 * Vector for continuous data represented as a vector on R<sup>d</sup>
 * @tparam Vs the type of this vector
 */
final case class ScalarVector[Vs <: Seq[Double]](final val vector: Vs) extends GScalarVector[Vs, ScalarVector[Vs]] {

	final def pickFeatures(indices: Int*): ScalarVector[Vs] = {
		ScalarVector{
			val builder = vector.genericBuilder[Double].asInstanceOf[mutable.Builder[Double, Vs]]
			builder.sizeHint(indices.size)
			indices.foreach( i => builder += vector(i) )
			builder.result
		}
	}
}
/**
 * Vector for binary and continuous data represented as 2 vectors, one on R<sup>d1</sup> Vs, the other on {0, 1}<sup>d2</sup> Vb
 * @tparam Vb binary part type of this mixt vector
 * @tparam Vs scalar part type of this mixt vector
 */
final case class MixedVector[Vb <: Seq[Int], Vs <: Seq[Double]](final val binary: Vb, final val scalar: Vs) extends GMixedVector[Vb, Vs, MixedVector[Vb, Vs]] {
	/**
	 * Features are indexed as follow, first one are the binary features, rest are scalar features
	 */
	final def pickFeatures(indices: Int*): MixedVector[Vb, Vs] = {
			val (binaries, scalars) = indices.partition(_ < binary.size)
			val binBuilder = binary.genericBuilder[Int].asInstanceOf[mutable.Builder[Int, Vb]]
			binBuilder.sizeHint(binaries.size)
			binaries.foreach( i => binBuilder += binary(i) )
			val scaBuilder = scalar.genericBuilder[Double].asInstanceOf[mutable.Builder[Double, Vs]]
			scaBuilder.sizeHint(scalars.size)
			scalars.foreach( i => scaBuilder += scalar(i - binary.size - 1) )
			MixedVector(binBuilder.result, scaBuilder.result)
	}
	/**
	 * @return ScalarVector[Vs] with binary features first then scalar ones
	 */
	final def toScalarVector: ScalarVector[Vs] = {
		val builder = scalar.genericBuilder[Double].asInstanceOf[mutable.Builder[Double, Vs]]
		builder.sizeHint(scalar.size + binary.size)
		builder ++= binary.map(_.toDouble)
		builder ++= scalar
		ScalarVector(builder.result)
	}
}
/**
 *
 */
final case class SupervizedVector[N, V[X] <: Seq[X]](final val vector: V[N]) extends GSimpleVector[N, V[N], SupervizedVector[N, V]] {

	final def pickFeatures(indices: Int*): SupervizedVector[N, V] = {
		SupervizedVector{
			val builder = vector.genericBuilder[N].asInstanceOf[mutable.Builder[N, V[N]]]
			builder.sizeHint(indices.size)
			indices.foreach( i => builder += vector(i) )
			builder.result
		}
	}
}
/**
 *
 */
final case class GenericObjectVector[O](final val rawObject: O) extends GVector[GenericObjectVector[O]] {

	final def pickFeatures(indices: Int*): GenericObjectVector[O] = this.copy()
}