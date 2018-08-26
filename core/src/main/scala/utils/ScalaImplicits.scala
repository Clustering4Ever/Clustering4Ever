package clustering4ever.util

import clustering4ever.scala.vectorizables.{RealVector, BinaryVector}
import clustering4ever.scala.clusterizables.{RealClusterizable, BinaryClusterizable}
import scala.collection.{immutable, GenSeq}

object GenerateClusterizable
{
	def obtainSimpleRealClusterizable[ID: Numeric](id: ID, vector: Seq[Double]) = new RealClusterizable(id, new RealVector(vector))

	def obtainSimpleBinaryClusterizable[ID: Numeric](id: ID, vector: Seq[Int]) = new BinaryClusterizable(id, new BinaryVector(vector))
}

object ScalaImplicits
{
	implicit def seqOfRealSeqWithIndexedSequenceToSeqOfRealClusterizable[ID: Numeric](seq: GenSeq[(Seq[Double], ID)]): GenSeq[RealClusterizable[ID, Seq[Double], Seq[Double]]] =
		seq.map{ case (vector, id) => GenerateClusterizable.obtainSimpleRealClusterizable(id, vector) }

	implicit def seqOfBinarySeqWithIndexedSequenceToSeqOfBinaryClusterizable[ID: Numeric](seq: GenSeq[(Seq[Int], ID)]): GenSeq[BinaryClusterizable[ID, Seq[Int], Seq[Int]]] =
		seq.map{ case (vector, id) => GenerateClusterizable.obtainSimpleBinaryClusterizable(id, vector) }

	implicit def realVectorSequenceToRealClusterizable(seq: GenSeq[Seq[Double]]): GenSeq[RealClusterizable[Int, Seq[Double], Seq[Double]]] =
		seqOfRealSeqWithIndexedSequenceToSeqOfRealClusterizable(seq.zipWithIndex)

	implicit def binaryVectorSequenceToBinaryClusterizable(seq: GenSeq[Seq[Int]]): GenSeq[BinaryClusterizable[Int, Seq[Int], Seq[Int]]] =
		seqOfBinarySeqWithIndexedSequenceToSeqOfBinaryClusterizable(seq.zipWithIndex)
}