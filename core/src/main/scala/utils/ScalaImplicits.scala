package clustering4ever.util

import _root_.clustering4ever.scala.clusterizables.{RealClusterizable, BinaryClusterizable}
import scala.collection.immutable

object ScalaImplicits
{
	implicit def realVectorWithIndexSequenceToRealClusterizable[ID: Numeric](seq: Seq[(immutable.Seq[Double], ID)]): Seq[RealClusterizable[ID, immutable.Seq[Double]]] =
	{
		seq.map{ case (vector, id) => GenerateDatasets.obtainSimpleRealClusterizable(id, vector) }
	}

	implicit def binaryVectorWithIndexSequenceToBinaryClusterizable[ID: Numeric](seq: Seq[(immutable.Seq[Int], ID)]): Seq[BinaryClusterizable[ID, immutable.Seq[Int]]] =
	{
		seq.map{ case (vector, id) => GenerateDatasets.obtainSimpleBinaryClusterizable(id, vector) }
	}

	implicit def realVectorSequenceToRealClusterizable(seq: Seq[immutable.Seq[Double]]): Seq[RealClusterizable[Int, immutable.Seq[Double]]] =
	{
		realVectorWithIndexSequenceToRealClusterizable(seq.zipWithIndex)
	}

	implicit def binaryVectorSequenceToBinaryClusterizable(seq: Seq[immutable.Seq[Int]]): Seq[BinaryClusterizable[Int, immutable.Seq[Int]]] =
	{
		binaryVectorWithIndexSequenceToBinaryClusterizable(seq.zipWithIndex)
	}
}