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
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixedDistance}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.util.SparkImplicits._
import org.clustering4ever.vectors.{GVector, ScalarVector}
/**
 *
 */
final case class KMeans[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](final val k: Int, final val metric: D[V], final val epsilon: Double, final val maxIterations: Int, final val persistanceLVL: StorageLevel, final val kryoSerialization: Boolean = false, final val customCenters: immutable.HashMap[Int, ScalarVector[V]] = immutable.HashMap.empty[Int, ScalarVector[V]])(implicit protected final val ctV: ClassTag[ScalarVector[V]]) extends KCentersAncestor[ScalarVector[V], D[V], KMeansModel[V, D]] {

	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KMeans

	final def run[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: Dataset[Cz[O, ScalarVector[V]]])(implicit ct: ClassTag[Cz[O, ScalarVector[V]]]): KMeansModel[V, D] = KMeansModel[V, D](k, metric, epsilon, maxIterations, persistanceLVL, kryoSerialization, obtainCenters(data))
}