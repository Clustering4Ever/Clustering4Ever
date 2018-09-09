package clustering4ever.util

import scala.collection.parallel.mutable.ParArray
import clustering4ever.scala.vectorizables.{RealVector, BinaryVector, MixtVector}
import clustering4ever.scala.clusterizables.{RealClusterizable, BinaryClusterizable, MixtClusterizable}
import clustering4ever.scala.measurableclass.BinaryScalarVector

object GenerateClusterizable {

	def obtainSimpleRealClusterizable[ID: Numeric, V <: Seq[Double]](id: ID, vector: V): RealClusterizable[ID, V, V] = new RealClusterizable[ID, V, V](id, new RealVector[V](vector))

	def obtainSimpleBinaryClusterizable[ID: Numeric, V <: Seq[Int]](id: ID, vector: V): BinaryClusterizable[ID, V, V] = new BinaryClusterizable[ID, V, V](id, new BinaryVector[V](vector))

	def obtainSimpleMixtClusterizable[ID: Numeric, Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs]](id: ID, vectors: V): MixtClusterizable[ID, V, Vb, Vs, V] = new MixtClusterizable[ID, V, Vb, Vs, V](id, new MixtVector[Vb, Vs, V](vectors))
}

object ScalaImplicits {

	implicit def seqOfRealSeqWithIndexedSequenceToSeqOfRealClusterizable[ID: Numeric, V <: Seq[Double]](parArr: ParArray[(V, ID)]): ParArray[RealClusterizable[ID, V, V]] =
		parArr.map{ case (vector, id) => GenerateClusterizable.obtainSimpleRealClusterizable(id, vector) }

	implicit def seqOfBinarySeqWithIndexedSequenceToSeqOfBinaryClusterizable[ID: Numeric, V <: Seq[Int]](parArr: ParArray[(V, ID)]): ParArray[BinaryClusterizable[ID, V, V]] =
		parArr.map{ case (vector, id) => GenerateClusterizable.obtainSimpleBinaryClusterizable(id, vector) }

	implicit def realVectorSequenceToRealClusterizable[V <: Seq[Double]](parArr: ParArray[V]): ParArray[RealClusterizable[Int, V, V]] =
		seqOfRealSeqWithIndexedSequenceToSeqOfRealClusterizable(parArr.zipWithIndex)

	implicit def binaryVectorSequenceToBinaryClusterizable[V <: Seq[Int]](parArr: ParArray[V]): ParArray[BinaryClusterizable[Int, V, V]] =
		seqOfBinarySeqWithIndexedSequenceToSeqOfBinaryClusterizable(parArr.zipWithIndex)
}