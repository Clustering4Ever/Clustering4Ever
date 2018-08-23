package clustering4ever.util

import clustering4ever.scala.vectorizables.{RealVector, BinaryVector}
import clustering4ever.scala.clusterizables.{RealClusterizable, BinaryClusterizable}

object GenerateDatasets
{
	def obtainSimpleRealClusterizable[ID: Numeric](id: ID, vector: Seq[Double]) = new RealClusterizable(id, new RealVector(vector))

	def obtainSimpleBinaryClusterizable[ID: Numeric](id: ID, vector: Seq[Int]) = new BinaryClusterizable(id, new BinaryVector(vector))
}