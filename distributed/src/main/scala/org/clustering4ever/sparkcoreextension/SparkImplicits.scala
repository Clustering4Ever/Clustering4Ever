package org.clustering4ever.sparkcoreextension

/**
 * @author Beck Gaël
 */
import org.apache.spark.rdd.RDD
import org.clustering4ever.roottraits.{BinaryVector, EasyClusterizable, ScalarVector}

import scala.language.{higherKinds, implicitConversions}
/**
 *
 */
object SparkImplicits {
	/**
	 *
	 */
	final implicit def scalarDataWithIDToClusterizable(rdd: RDD[(Array[Double], Long)]): RDD[EasyClusterizable[None.type, ScalarVector]] = {
		rdd.map{ case (vector, id) => EasyClusterizable(id, ScalarVector(vector)) }
	}
	/**
	 *
	 */
	final implicit def binaryDataWithIDToClusterizable(rdd: RDD[(Array[Int], Long)]): RDD[EasyClusterizable[None.type, BinaryVector]] = {
		rdd.map{ case (vector, id) => EasyClusterizable(id, BinaryVector(vector)) }
	}
	/**
	 *
	 */
	final implicit def scalarDataToClusterizable(rdd: RDD[Array[Double]]): RDD[EasyClusterizable[None.type, ScalarVector]] = {
		rdd.zipWithIndex
	}
	/**
	 *
	 */
	final implicit def binaryDataToClusterizable(rdd: RDD[Array[Int]]): RDD[EasyClusterizable[None.type, BinaryVector]] = {
		rdd.zipWithIndex
	}
}