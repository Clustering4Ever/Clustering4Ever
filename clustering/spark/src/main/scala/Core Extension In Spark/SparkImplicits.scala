package org.clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.language.implicitConversions
import org.apache.spark.rdd.RDD
import org.clustering4ever.scala.clusterizables.EasyClusterizable
/**
 *
 */
object SparkImplicits {
	/**
	 *
	 */
	implicit def realVectorWithIndexRDDToRealClusterizable[ID: Numeric, V[Double] <: Seq[Double]](rdd: RDD[(V[Double], ID)]): RDD[EasyClusterizable[ID, V[Double], V[Double]]] = {
		rdd.map{ case (vector, id) => ClusterizableGenerator.obtainEasyClusterizable(id, vector) }
	}
	/**
	 *
	 */
	implicit def binaryVectorWithIndexRDDToBinaryClusterizable[ID: Numeric, V[Int] <: Seq[Int]](rdd: RDD[(V[Int], ID)]): RDD[EasyClusterizable[ID, V[Int], V[Int]]] = {
		rdd.map{ case (vector, id) => ClusterizableGenerator.obtainEasyClusterizable(id, vector) }
	}
	/**
	 *
	 */
	implicit def realVectorRDDToRealClusterizable[V[Double] <: Seq[Double]](rdd: RDD[V[Double]]): RDD[EasyClusterizable[Long, V[Double], V[Double]]] = rdd.zipWithIndex
	/**
	 *
	 */
	implicit def binaryVectorRDDToBinaryClusterizable[V[Int] <: Seq[Int]](rdd: RDD[V[Int]]): RDD[EasyClusterizable[Long, V[Int], V[Int]]] = rdd.zipWithIndex
}