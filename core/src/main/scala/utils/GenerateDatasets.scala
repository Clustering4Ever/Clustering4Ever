package clustering4ever.util

import _root_.clustering4ever.scala.vectorizables.{RealVector, BinaryVector}
import _root_.clustering4ever.scala.clusterizables.{RealClusterizable, BinaryClusterizable}
import scala.collection.immutable

object GenerateDatasets
{
	def obtainSimpleRealClusterizable[ID: Numeric](id: ID, vector: immutable.Seq[Double]) =
	{
		new RealClusterizable(id, new RealVector(vector))
	}

	def obtainSimpleBinaryClusterizable[ID: Numeric](id: ID, vector: immutable.Seq[Int]) =
	{
		new BinaryClusterizable(id, new BinaryVector(vector))
	}
}