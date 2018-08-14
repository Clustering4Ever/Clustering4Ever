package clustering4ever.util

import clustering4ever.scala.clusterizables.{RealClusterizable, BinaryClusterizable}
import scala.collection.immutable

object ScalaImplicits
{
	implicit def seqOfRealSeqWithIndexedSequenceToSeqOfRealClusterizable[ID: Numeric](seq: immutable.Seq[(immutable.Seq[Double], ID)]): immutable.Seq[RealClusterizable[ID, immutable.Seq[Double]]] =
	{
		seq.map{ case (vector, id) => GenerateDatasets.obtainSimpleRealClusterizable(id, vector) }
	}

	implicit def seqOfBinarySeqWithIndexedSequenceToSeqOfBinaryClusterizable[ID: Numeric](seq: immutable.Seq[(immutable.Seq[Int], ID)]): immutable.Seq[BinaryClusterizable[ID, immutable.Seq[Int]]] =
	{
		seq.map{ case (vector, id) => GenerateDatasets.obtainSimpleBinaryClusterizable(id, vector) }
	}

	implicit def realVectorSequenceToRealClusterizable(seq: immutable.Seq[immutable.Seq[Double]]): immutable.Seq[RealClusterizable[Int, immutable.Seq[Double]]] =
	{
		seqOfRealSeqWithIndexedSequenceToSeqOfRealClusterizable(seq.zipWithIndex)
	}

	implicit def binaryVectorSequenceToBinaryClusterizable(seq: immutable.Seq[immutable.Seq[Int]]): immutable.Seq[BinaryClusterizable[Int, immutable.Seq[Int]]] =
	{
		seqOfBinarySeqWithIndexedSequenceToSeqOfBinaryClusterizable(seq.zipWithIndex)
	}
}