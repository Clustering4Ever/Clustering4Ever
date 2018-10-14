package clustering4ever.util

import org.apache.spark.rdd.RDD
import clustering4ever.scala.clusterizables.{SimpleRealClusterizable, SimpleBinaryClusterizable}
import scala.language.implicitConversions

object SparkImplicits {

	implicit def realVectorWithIndexRDDToRealClusterizable[ID: Numeric, V <: Seq[Double]](rdd: RDD[(V, ID)]): RDD[SimpleRealClusterizable[ID, V, V]] =
		rdd.map{ case (vector, id) => GenerateClusterizable.obtainSimpleRealClusterizable(id, vector) }

	implicit def binaryVectorWithIndexRDDToBinaryClusterizable[ID: Numeric, V <: Seq[Int]](rdd: RDD[(V, ID)]): RDD[SimpleBinaryClusterizable[ID, V, V]] =
		rdd.map{ case (vector, id) => GenerateClusterizable.obtainSimpleBinaryClusterizable(id, vector) }

	implicit def realVectorRDDToRealClusterizable[V <: Seq[Double]](rdd: RDD[V]): RDD[SimpleRealClusterizable[Long, V, V]] =
		realVectorWithIndexRDDToRealClusterizable(rdd.zipWithIndex)

	implicit def binaryVectorRDDToBinaryClusterizable[V <: Seq[Int]](rdd: RDD[V]): RDD[SimpleBinaryClusterizable[Long, V, V]] =
		binaryVectorWithIndexRDDToBinaryClusterizable(rdd.zipWithIndex)
}