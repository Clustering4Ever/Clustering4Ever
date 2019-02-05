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
	implicit def scalarDataWithIDToClusterizable[V <: Seq[Double]](rdd: RDD[(V, Long)]): RDD[EasyClusterizable[ScalarVector[V], ScalarVector[V]]] = {
		rdd.map{ case (vector, id) => EasyClusterizable(id, ScalarVector(vector)) }
	}
	/**
	 *
	 */
	implicit def binaryDataWithIDToClusterizable[V <: Seq[Int]](rdd: RDD[(V, Long)]): RDD[EasyClusterizable[BinaryVector[V], BinaryVector[V]]] = {
		rdd.map{ case (vector, id) => EasyClusterizable(id, BinaryVector(vector)) }
	}
	/**
	 *
	 */
	implicit def scalarDataToClusterizable[V <: Seq[Double]](rdd: RDD[V]): RDD[EasyClusterizable[ScalarVector[V], ScalarVector[V]]] = {
		rdd.zipWithIndex
	}

	/**
	 *
	 */
	implicit def binaryDataToClusterizable[V <: Seq[Int]](rdd: RDD[V]): RDD[EasyClusterizable[BinaryVector[V], BinaryVector[V]]] = {
		rdd.zipWithIndex
	}
}