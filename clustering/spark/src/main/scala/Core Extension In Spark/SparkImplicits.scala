package org.clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.language.implicitConversions
import org.apache.spark.rdd.RDD
import org.clustering4ever.clusterizables.EasyClusterizable
import org.clustering4ever.vectors.{BinaryVector, ScalarVector}
import org.clustering4ever.vectors.GSimpleVector
/**
 *
 */
object SparkImplicits {
	/**
	 *
	 */
	implicit def scalarDataWithIDToClusterizable[ID, V <: Seq[Double]](rdd: RDD[(V, ID)]): RDD[EasyClusterizable[ID, ScalarVector[V], ScalarVector[V]]] = {
		rdd.map{ case (vector, id) => ClusterizableGenerator.obtainEasyClusterizable(id, new ScalarVector(vector)) }
	}

	/**
	 *
	 */
	implicit def binaryDataWithIDToClusterizable[ID, V <: Seq[Int]](rdd: RDD[(V, ID)]): RDD[EasyClusterizable[ID, BinaryVector[V], BinaryVector[V]]] = {
		rdd.map{ case (vector, id) => ClusterizableGenerator.obtainEasyClusterizable(id, new BinaryVector(vector)) }
	}
	/**
	 *
	 */
	implicit def scalarDataToClusterizable[V <: Seq[Double]](rdd: RDD[V]): RDD[EasyClusterizable[Long, ScalarVector[V], ScalarVector[V]]] = {
		rdd.zipWithIndex
	}

	/**
	 *
	 */
	implicit def binaryDataToClusterizable[V <: Seq[Int]](rdd: RDD[V]): RDD[EasyClusterizable[Long, BinaryVector[V], BinaryVector[V]]] = {
		rdd.zipWithIndex
	}
}