package org.clustering4ever.vectors
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.mutable
import scala.math.sqrt
import org.clustering4ever.util.SumVectors
import org.clustering4ever.util.VectorsAddOperationsImplicits
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
 * GVector sub family on which basic numerical operation can be apply
 */
trait NumericalVector[Self <: NumericalVector[Self]] extends GVector[Self] {
	this: Self =>
	/**
	 * @return the sum of this vector plus the given one
	 */
	def +(v: Self): Self
	/**
	 *
	 */
	// def -(v: Self): Self
	/**
	 *
	 */
	// def *(v: Self): Self
}
/**
 * @tparam N the type object composing the vector
 * @tparam V the vector type
 */
trait GSimpleVector[@specialized(Int, Double) N, Self <: GSimpleVector[N, Self]] extends GVector[Self] {
	this: Self =>
	/**
	 * A vector taking the form of Seq[N] for any N
	 */
	val vector: Array[N]
	/**
	 *
	 */
	// lazy val norm: Double = SumVectors.dotProduct
}
/**
 *
 */
trait GBinaryVector[Self <: GBinaryVector[Self]] extends GSimpleVector[Int, Self] with NumericalVector[Self] {
	this: Self =>
}
/**
 *
 */
trait GScalarVector[Self <: GScalarVector[Self]] extends GSimpleVector[Double, Self] with NumericalVector[Self] {
	this: Self =>
}
/**
 *
 */
trait GMixedVector[Self <: GMixedVector[Self]] extends GVector[Self] with NumericalVector[Self] {
	this: Self =>
	/**
	 * The binary part of this mixt vector 
	 */
	val binary: Array[Int]
	/**
	 * The scalar part of this mixt vector 
	 */
	val scalar: Array[Double]
}
/**
 * Vector for binary data taking represented as a vector on {0, 1}<sup>d</sup>
 * @tparam Vb the type of this vector
 */
final case class BinaryVector(final val vector: Array[Int]) extends GBinaryVector[BinaryVector] {

	final def pickFeatures(indices: Int*): BinaryVector = {

		BinaryVector{
			val builder = vector.genericBuilder[Int].asInstanceOf[mutable.Builder[Int, Array[Int]]]
			builder.sizeHint(indices.size)
			indices.foreach( i => builder += vector(i) )
			builder.result
		}
	}
	/**
	 *
	 */
	final def +(v: BinaryVector): BinaryVector = VectorsAddOperationsImplicits.addBinaryVectors(this, v)
	/**
	 *
	 */
	final def dot(v: BinaryVector): Double = SumVectors.dotProduct(this.vector, v.vector)
}
/**
 * Vector for continuous data represented as a vector on R<sup>d</sup>
 * @tparam Vs the type of this vector
 */
final case class ScalarVector(final val vector: Array[Double]) extends GScalarVector[ScalarVector] {

	final def pickFeatures(indices: Int*): ScalarVector = {
		ScalarVector{
			val builder = vector.genericBuilder[Double].asInstanceOf[mutable.Builder[Double, Array[Double]]]
			builder.sizeHint(indices.size)
			indices.foreach( i => builder += vector(i) )
			builder.result
		}
	}

	final def +(v: ScalarVector): ScalarVector = VectorsAddOperationsImplicits.addScalarVectors(this, v)
	/**
	 * Dot product of this ScalarVector with another
	 */
	final def dot(v: ScalarVector): Double = SumVectors.dotProduct(this.vector, v.vector)
	/**
	 * Norm of this ScalarVector
	 */
	final lazy val norm: Double = sqrt(norm2)
	/**
	 * Norm squared of this ScalarVector
	 */
	final lazy val norm2: Double = SumVectors.dotProduct(this.vector, this.vector)
}
/**
 * Vector for binary and continuous data represented as 2 vectors, one on R<sup>d1</sup> Vs, the other on {0, 1}<sup>d2</sup> Vb
 * @tparam Vb binary part type of this mixt vector
 * @tparam Vs scalar part type of this mixt vector
 */
final case class MixedVector(final val binary: Array[Int], final val scalar: Array[Double]) extends GMixedVector[MixedVector] {
	/**
	 * Features are indexed as follow, first one are the binary features, rest are scalar features
	 */
	final def pickFeatures(indices: Int*): MixedVector = {
			val (binaries, scalars) = indices.partition(_ < binary.size)
			val binBuilder = binary.genericBuilder[Int].asInstanceOf[mutable.Builder[Int, Array[Int]]]
			binBuilder.sizeHint(binaries.size)
			binaries.foreach( i => binBuilder += binary(i) )
			val scaBuilder = scalar.genericBuilder[Double].asInstanceOf[mutable.Builder[Double, Array[Double]]]
			scaBuilder.sizeHint(scalars.size)
			scalars.foreach( i => scaBuilder += scalar(i - binary.size - 1) )
			MixedVector(binBuilder.result, scaBuilder.result)
	}
	/**
	 * @return ScalarVector[Vs] with binary features first then scalar ones
	 */
	final def toScalarVector: ScalarVector = {
		val builder = scalar.genericBuilder[Double].asInstanceOf[mutable.Builder[Double, Array[Double]]]
		builder.sizeHint(scalar.size + binary.size)
		binary.foreach(builder += _.toDouble)
		scalar.foreach(builder += _)
		ScalarVector(builder.result)
	}

	final def +(v: MixedVector): MixedVector = VectorsAddOperationsImplicits.addMixedVectors(this, v)
}
/**
 *
 */
final case class SupervizedVector[N](final val vector: Array[N]) extends GSimpleVector[N, SupervizedVector[N]] {

	final def pickFeatures(indices: Int*): SupervizedVector[N] = {
		SupervizedVector{
			val builder = vector.genericBuilder[N].asInstanceOf[mutable.Builder[N, Array[N]]]
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