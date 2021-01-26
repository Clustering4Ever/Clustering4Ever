package org.clustering4ever.clustering.kfamily.kcenters.dataset

/**
 * @author Beck Gaël
 */
import org.apache.spark.sql.Dataset
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.distances.ContinuousDistance
import org.clustering4ever.roottraits.{Clusterizable, GVector, ScalarVector}

import scala.collection.immutable
import scala.language.higherKinds
import scala.reflect.ClassTag
/**
 *
 */
final case class KMeans[D <: ContinuousDistance](final val k: Int, final val metric: D, final val minShift: Double, final val maxIterations: Int, final val persistanceLVL: StorageLevel, final val kryoSerialization: Boolean = false, final val customCenters: immutable.HashMap[Int, ScalarVector] = immutable.HashMap.empty[Int, ScalarVector])(implicit protected final val ctV: ClassTag[ScalarVector]) extends KCentersAncestor[ScalarVector, D, KMeansModel[D]] {

	val algorithmID = org.clustering4ever.roottraits.KMeans

	def fit[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: Dataset[Cz[O, ScalarVector]])(implicit ct: ClassTag[Cz[O, ScalarVector]]): KMeansModel[D] = KMeansModel[D](k, metric, minShift, maxIterations, persistanceLVL, kryoSerialization, obtainCenters(data))
}