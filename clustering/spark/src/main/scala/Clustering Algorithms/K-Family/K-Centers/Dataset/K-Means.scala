package org.clustering4ever.clustering.kcenters.dataset
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.immutable
import scala.util.Random
import org.apache.spark.sql.Dataset
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.util.SparkImplicits._
import org.clustering4ever.vectors.{GVector, ScalarVector}
/**
 *
 */
case class KMeans[ID, O, V <: Seq[Double], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Double]] <: ContinuousDistance[X]](val args: KMeansArgs[V, D])(protected implicit val ct: ClassTag[Cz[ID, O, ScalarVector[V]]], protected val ctV: ClassTag[ScalarVector[V]]) extends KCentersAncestor[ID, O, ScalarVector[V], Cz, D[V], KMeansArgs[V, D], KMeansModel[ID, O, V, Cz, D]] {
	/**
	 *
	 */
	val kryoSerialization = args.kryoSerialization	
	/**
	 *
	 */
	def run(data: Dataset[Cz[ID, O, ScalarVector[V]]]): KMeansModel[ID, O, V, Cz, D] = KMeansModel[ID, O, V, Cz, D](obtainCenters(data), args.metric, args)
}