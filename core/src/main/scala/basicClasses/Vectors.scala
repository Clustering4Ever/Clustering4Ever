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
 *
 */
trait GSimpleVector[N, V <: Seq[N], Self <: GSimpleVector[N, V, Self]] extends GVector[Self] {
	/**
	 *
	 */
	this: Self =>
	/**
	 *
	 */
	val vector: V
}
/**
 *
 */
trait GBinaryVector[Vb <: Seq[Int], Self <: GBinaryVector[Vb, Self]] extends GSimpleVector[Int, Vb, Self] {
	/**
	 *
	 */
	this: Self =>
}
/**
 *
 */
trait GScalarVector[Vs <: Seq[Double], Self <: GScalarVector[Vs, Self]] extends GSimpleVector[Double, Vs, Self] {
	/**
	 *
	 */
	this: Self =>
}
/**
 *
 */
trait GMixtVector[Vb <: Seq[Int], Vs <: Seq[Double], Self <: GMixtVector[Vb, Vs, Self]] extends GVector[Self] {
	/**
	 *
	 */
	this: Self =>
	/**
	 *
	 */
	val binary: Vb
	/**
	 *
	 */
	val scalar: Vs
}
/**
 * Vector for binary data taking represented as a vector on {0, 1}<sup>d</sup>
 */
case class BinaryVector[Vb <: Seq[Int]](val vector: Vb) extends GBinaryVector[Vb, BinaryVector[Vb]] {
	/**
	 *
	 */
	def pickFeatures(indices: Int*): BinaryVector[Vb] = {
		BinaryVector{
			val builder = vector.genericBuilder.asInstanceOf[mutable.Builder[Int, Vb]]
			builder.sizeHint(indices.size)
			indices.foreach( i => builder += vector(i) )
			builder.result
		}
	}
}
/**
 * Vector for continuous data represented as a vector on R<sup>d</sup>
 */
case class ScalarVector[Vs <: Seq[Double]](val vector: Vs) extends GScalarVector[Vs, ScalarVector[Vs]] {
	/**
	 *
	 */
	def pickFeatures(indices: Int*): ScalarVector[Vs] = {
		ScalarVector{
			val builder = vector.genericBuilder.asInstanceOf[mutable.Builder[Double, Vs]]
			builder.sizeHint(indices.size)
			indices.foreach( i => builder += vector(i) )
			builder.result
		}
	}
}
/**
 * Vector for binary and continuous data represented as 2 vectors, one on R<sup>d1</sup> Vs, the other on {0, 1}<sup>d2</sup> Vb
 */
case class MixtVector[Vb <: Seq[Int], Vs <: Seq[Double]](val binary: Vb, val scalar: Vs) extends GMixtVector[Vb, Vs, MixtVector[Vb, Vs]] {
	/**
	 * Features are indexed as follow, first one are the binary features, rest are scalar features
	 */
	def pickFeatures(indices: Int*): MixtVector[Vb, Vs] = {
			val (binaries, scalars) = indices.partition(_ < binary.size)
			val binBuilder = binary.genericBuilder.asInstanceOf[mutable.Builder[Int, Vb]]
			binBuilder.sizeHint(binaries.size)
			binaries.foreach( i => binBuilder += binary(i) )
			val scaBuilder = scalar.genericBuilder.asInstanceOf[mutable.Builder[Double, Vs]]
			scaBuilder.sizeHint(scalars.size)
			scalars.foreach( i => scaBuilder += scalar(i - binary.size - 1) )
			MixtVector(binBuilder.result, scaBuilder.result)
	}
}
/**
 *
 */
case class SupervizedVector[N, V[X] <: Seq[X]](val vector: V[N]) extends GSimpleVector[N, V[N], SupervizedVector[N, V]] {
	/**
	 *
	 */
	def pickFeatures(indices: Int*): SupervizedVector[N, V] = {
		SupervizedVector{
			val builder = vector.genericBuilder.asInstanceOf[mutable.Builder[N, V[N]]]
			builder.sizeHint(indices.size)
			indices.foreach( i => builder += vector(i) )
			builder.result
		}
	}
}
/**
 *
 */
case class GenericObjectVector[O](val rawObject: O) extends GVector[GenericObjectVector[O]] {
	/**
	 *
	 */
	def pickFeatures(indices: Int*): GenericObjectVector[O] = this.copy()
}