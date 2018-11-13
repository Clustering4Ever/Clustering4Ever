package clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.language.implicitConversions
import org.apache.spark.rdd.RDD
import clustering4ever.scala.clusterizables.EasyClusterizable
/**
 *
 */
object SparkImplicits {
	/**
	 *
	 */
	implicit def realVectorWithIndexRDDToRealClusterizable[ID: Numeric, V <: Seq[Double]](rdd: RDD[(V, ID)]): RDD[EasyClusterizable[ID, V, V]] = {
		rdd.map{ case (vector, id) => GenerateClusterizable.obtainEasyRealClusterizable(id, vector) }
	}
	/**
	 *
	 */
	implicit def binaryVectorWithIndexRDDToBinaryClusterizable[ID: Numeric, V <: Seq[Int]](rdd: RDD[(V, ID)]): RDD[EasyClusterizable[ID, V, V]] = {
		rdd.map{ case (vector, id) => GenerateClusterizable.obtainEasyBinaryClusterizable(id, vector) }
	}
	/**
	 *
	 */
	implicit def realVectorRDDToRealClusterizable[V <: Seq[Double]](rdd: RDD[V]): RDD[EasyClusterizable[Long, V, V]] = {
		realVectorWithIndexRDDToRealClusterizable(rdd.zipWithIndex)
	}
	/**
	 *
	 */
	implicit def binaryVectorRDDToBinaryClusterizable[V <: Seq[Int]](rdd: RDD[V]): RDD[EasyClusterizable[Long, V, V]] = {
		binaryVectorWithIndexRDDToBinaryClusterizable(rdd.zipWithIndex)
	}
}