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
final case class KMeans[D <: ContinuousDistance](final val k: Int, final val metric: D, final val minShift: Double, final val maxIterations: Int, final val persistanceLVL: StorageLevel, final val kryoSerialization: Boolean = false, final val customCenters: immutable.HashMap[Int, ScalarVector] = immutable.HashMap.empty[Int, ScalarVector])(implicit protected final val ctV: ClassTag[ScalarVector]) extends KCentersAncestor[ScalarVector, D, KMeansModel[D]] {

	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KMeans

	final def fit[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: Dataset[Cz[O, ScalarVector]])(implicit ct: ClassTag[Cz[O, ScalarVector]]): KMeansModel[D] = KMeansModel[D](k, metric, minShift, maxIterations, persistanceLVL, kryoSerialization, obtainCenters(data))
}