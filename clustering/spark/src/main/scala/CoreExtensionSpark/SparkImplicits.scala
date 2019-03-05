package org.clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.language.implicitConversions
import org.apache.spark.rdd.RDD
import org.clustering4ever.clusterizables.EasyClusterizable
import org.clustering4ever.vectors.{BinaryVector, ScalarVector}
/**
 *
 */
object SparkImplicits {
	/**
	 *
	 */
	final implicit def scalarDataWithIDToClusterizable[V <: Seq[Double]](rdd: RDD[(V, Long)]): RDD[EasyClusterizable[None.type, ScalarVector[V]]] = {
		rdd.map{ case (vector, id) => EasyClusterizable(id, ScalarVector(vector)) }
	}
	/**
	 *
	 */
	final implicit def binaryDataWithIDToClusterizable[V <: Seq[Int]](rdd: RDD[(V, Long)]): RDD[EasyClusterizable[None.type, BinaryVector[V]]] = {
		rdd.map{ case (vector, id) => EasyClusterizable(id, BinaryVector(vector)) }
	}
	/**
	 *
	 */
	final implicit def scalarDataToClusterizable[V <: Seq[Double]](rdd: RDD[V]): RDD[EasyClusterizable[None.type, ScalarVector[V]]] = {
		rdd.zipWithIndex
	}
	/**
	 *
	 */
	final implicit def binaryDataToClusterizable[V <: Seq[Int]](rdd: RDD[V]): RDD[EasyClusterizable[None.type, BinaryVector[V]]] = {
		rdd.zipWithIndex
	}
}