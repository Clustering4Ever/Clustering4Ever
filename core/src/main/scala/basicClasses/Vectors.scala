package org.clustering4ever.vectors
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.mutable
import org.clustering4ever.util.SumVectors
/**
 *
 */
trait GVector[Self <: GVector[Self]] extends Serializable {
	/**
	 *
	 */
	def pickFeatures(i: Int*): Self
}
/**
 *
 */
trait GSimpleVector[N, V <: Seq[N], Self <: GSimpleVector[N, V, Self]] extends GVector[Self] {
	val vector: V
}
/**
 *
 */
case class SupervizedVector[N, V[X] <: Seq[X]](val vector: V[N]) extends GSimpleVector[N, V[N], SupervizedVector[N, V]] {
	/**
	 *
	 */
	def pickFeatures(idxs: Int*): SupervizedVector[N, V] = {
		new SupervizedVector({
			val builder = vector.genericBuilder.asInstanceOf[mutable.Builder[N, V[N]]]
			builder.sizeHint(idxs.size)
			idxs.foreach( i => builder += vector(i) )
			builder.result
		})
	}
}
/**
 *
 */
trait AnyVectorNature[O, Self <: AnyVectorNature[O, Self]] extends GVector[Self]
/**
 *
 */
trait GBinaryVector[Vb <: Seq[Int], Self <: GBinaryVector[Vb, Self]] extends GSimpleVector[Int, Vb, Self]
/**
 *
 */
trait GScalarVector[Vs <: Seq[Double], Self <: GScalarVector[Vs, Self]] extends GSimpleVector[Double, Vs, Self]
/**
 *
 */
trait GMixtVector[Vb <: Seq[Int], Vs <: Seq[Double], Self <: GMixtVector[Vb, Vs, Self]] extends GVector[Self] {
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
 *
 */
case class BinaryVector[Vb <: Seq[Int]](val vector: Vb) extends GBinaryVector[Vb, BinaryVector[Vb]] {
	/**
	 *
	 */
	def pickFeatures(idxs: Int*): BinaryVector[Vb] = {
		new BinaryVector({
			val builder = vector.genericBuilder.asInstanceOf[mutable.Builder[Int, Vb]]
			builder.sizeHint(idxs.size)
			idxs.foreach( i => builder += vector(i) )
			builder.result
		})
	}
}
/**
 *
 */
case class ScalarVector[Vs <: Seq[Double]](val vector: Vs) extends GScalarVector[Vs, ScalarVector[Vs]] {
	/**
	 *
	 */
	def pickFeatures(idxs: Int*): ScalarVector[Vs] = {
		new ScalarVector({
			val builder = vector.genericBuilder.asInstanceOf[mutable.Builder[Double, Vs]]
			builder.sizeHint(idxs.size)
			idxs.foreach( i => builder += vector(i) )
			builder.result
		})
	}
}
/**
 *
 */
case class MixtVector[Vb <: Seq[Int], Vs <: Seq[Double]](val binary: Vb, val scalar: Vs) extends GMixtVector[Vb, Vs, MixtVector[Vb, Vs]] {
	/**
	 * Features are indexed as follow, first one are the binary features, rest are scalar features
	 */
	def pickFeatures(idxs: Int*): MixtVector[Vb, Vs] = {

			val (binaries, scalars) = idxs.partition(_ <= binary.size)

			val binBuilder = binary.genericBuilder.asInstanceOf[mutable.Builder[Int, Vb]]
			binBuilder.sizeHint(binaries.size)
			binaries.foreach( i => binBuilder += binary(i) )
			val scaBuilder = scalar.genericBuilder.asInstanceOf[mutable.Builder[Double, Vs]]
			scaBuilder.sizeHint(scalars.size)
			scalars.foreach( i => scaBuilder += scalar(i) )

			new MixtVector(binBuilder.result, scaBuilder.result)
	}
}