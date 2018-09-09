package clustering4ever.util

import scala.collection.GenSeq
import clustering4ever.scala.vectorizables.{RealVector, BinaryVector, MixtVector}
import clustering4ever.scala.clusterizables.{RealClusterizable, BinaryClusterizable, MixtClusterizable}
import clustering4ever.scala.measurableclass.BinaryScalarVector

object GenerateClusterizable {

	def obtainSimpleRealClusterizable[ID: Numeric, V <: Seq[Double]](id: ID, vector: V): RealClusterizable[ID, V, V] = new RealClusterizable[ID, V, V](id, new RealVector[V](vector))

	def obtainSimpleBinaryClusterizable[ID: Numeric, V <: Seq[Int]](id: ID, vector: V): BinaryClusterizable[ID, V, V] = new BinaryClusterizable[ID, V, V](id, new BinaryVector[V](vector))

	def obtainSimpleMixtClusterizable[ID: Numeric, Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs]](id: ID, vectors: V): MixtClusterizable[ID, V, Vb, Vs, V] = new MixtClusterizable[ID, V, Vb, Vs, V](id, new MixtVector[Vb, Vs, V](vectors))
}

object ScalaImplicits {

	implicit def seqOfRealSeqWithIndexedSequenceToSeqOfRealClusterizable[ID: Numeric, V <: Seq[Double]](genSeq: GenSeq[(V, ID)]): GenSeq[RealClusterizable[ID, V, V]] =
		genSeq.map{ case (vector, id) => GenerateClusterizable.obtainSimpleRealClusterizable(id, vector) }

	implicit def seqOfBinarySeqWithIndexedSequenceToSeqOfBinaryClusterizable[ID: Numeric, V <: Seq[Int]](genSeq: GenSeq[(V, ID)]): GenSeq[BinaryClusterizable[ID, V, V]] =
		genSeq.map{ case (vector, id) => GenerateClusterizable.obtainSimpleBinaryClusterizable(id, vector) }

	implicit def realVectorSequenceToRealClusterizable[V <: Seq[Double]](genSeq: GenSeq[V]): GenSeq[RealClusterizable[Int, V, V]] =
		seqOfRealSeqWithIndexedSequenceToSeqOfRealClusterizable(genSeq.zipWithIndex)

	implicit def binaryVectorSequenceToBinaryClusterizable[V <: Seq[Int]](genSeq: GenSeq[V]): GenSeq[BinaryClusterizable[Int, V, V]] =
		seqOfBinarySeqWithIndexedSequenceToSeqOfBinaryClusterizable(genSeq.zipWithIndex)
}