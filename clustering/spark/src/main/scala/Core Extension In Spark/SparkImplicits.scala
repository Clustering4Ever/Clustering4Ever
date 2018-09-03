package clustering4ever.util

import org.apache.spark.rdd.RDD
import clustering4ever.scala.clusterizables.{RealClusterizable, BinaryClusterizable}

object SparkImplicits
{
	implicit def realVectorWithIndexRDDToRealClusterizable[ID: Numeric, V <: Seq[Double]](rdd: RDD[(V, ID)]): RDD[RealClusterizable[ID, V, V]] =
		rdd.map{ case (vector, id) => GenerateClusterizable.obtainSimpleRealClusterizable(id, vector) }

	implicit def binaryVectorWithIndexRDDToBinaryClusterizable[ID: Numeric, V <: Seq[Int]](rdd: RDD[(V, ID)]): RDD[BinaryClusterizable[ID, V, V]] =
		rdd.map{ case (vector, id) => GenerateClusterizable.obtainSimpleBinaryClusterizable(id, vector) }

	implicit def realVectorRDDToRealClusterizable[V <: Seq[Double]](rdd: RDD[V]): RDD[RealClusterizable[Long, V, V]] =
		realVectorWithIndexRDDToRealClusterizable(rdd.zipWithIndex)

	implicit def binaryVectorRDDToBinaryClusterizable[V <: Seq[Int]](rdd: RDD[V]): RDD[BinaryClusterizable[Long, V, V]] =
		binaryVectorWithIndexRDDToBinaryClusterizable(rdd.zipWithIndex)
}