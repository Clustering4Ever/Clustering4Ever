package clustering4ever.util

import clustering4ever.scala.clusterizables.{RealClusterizable, BinaryClusterizable}
import scala.collection.{immutable, GenSeq}

object ScalaImplicits
{
	implicit def seqOfRealSeqWithIndexedSequenceToSeqOfRealClusterizable[ID: Numeric](seq: GenSeq[(Seq[Double], ID)]): GenSeq[RealClusterizable[ID, Seq[Double]]] =
		seq.map{ case (vector, id) => GenerateDatasets.obtainSimpleRealClusterizable(id, vector) }

	implicit def seqOfBinarySeqWithIndexedSequenceToSeqOfBinaryClusterizable[ID: Numeric](seq: GenSeq[(Seq[Int], ID)]): GenSeq[BinaryClusterizable[ID, Seq[Int]]] =
		seq.map{ case (vector, id) => GenerateDatasets.obtainSimpleBinaryClusterizable(id, vector) }

	implicit def realVectorSequenceToRealClusterizable(seq: GenSeq[Seq[Double]]): GenSeq[RealClusterizable[Int, Seq[Double]]] =
		seqOfRealSeqWithIndexedSequenceToSeqOfRealClusterizable(seq.zipWithIndex)

	implicit def binaryVectorSequenceToBinaryClusterizable(seq: GenSeq[Seq[Int]]): GenSeq[BinaryClusterizable[Int, Seq[Int]]] =
		seqOfBinarySeqWithIndexedSequenceToSeqOfBinaryClusterizable(seq.zipWithIndex)
}