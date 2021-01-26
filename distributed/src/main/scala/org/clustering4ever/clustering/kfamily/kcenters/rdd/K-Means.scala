package org.clustering4ever.clustering.kfamily.kcenters.rdd

/**
 * @author Beck Gaël
 */
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.distances.ContinuousDistance
import org.clustering4ever.roottraits
import org.clustering4ever.roottraits.{Clusterizable, GVector, ScalarVector}
import org.clustering4ever.sparkcoreextension.SparkImplicits._
import org.clustering4ever.util.{ClusterBasicOperations, VectorsAddOperationsImplicits}

import scala.collection.immutable
import scala.language.higherKinds
import scala.reflect.ClassTag
/**
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : preferably and ArrayBuffer or ParArray of Clusterizable descendant, the EasyClusterizable is the basic reference
 * @param k : number of clusters
 * @param minShift : minimal threshold under which we consider a centroid has converged
 * @param maxIterations : maximal number of iteration
 * @param metric : a defined dissimilarity measure
 */
final case class KMeans[D <: ContinuousDistance](final val k: Int, final val metric: D, final val minShift: Double, final val maxIterations: Int, final val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, final val customCenters: immutable.HashMap[Int, ScalarVector] = immutable.HashMap.empty[Int, ScalarVector])(protected implicit final val ctV: ClassTag[ScalarVector]) extends KCentersAncestor[ScalarVector, D, KMeansModel[D]] {

	implicit val sumVector: (ScalarVector, ScalarVector) => ScalarVector = VectorsAddOperationsImplicits.addScalarVectors

	implicit val getCenter: (ScalarVector, Long) => ScalarVector = (v, d) => ClusterBasicOperations.transformPreMeanAndCastIt(v, d)

	final val algorithmID = roottraits.KMeans

	final def fit[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, ScalarVector]])(implicit ct: ClassTag[Cz[O, ScalarVector]]): KMeansModel[D] = KMeansModel[D](k, metric, minShift, maxIterations, persistanceLVL, obtainCenters(data))
}
/**
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : preferably and ArrayBuffer or ParArray of Clusterizable descendant, the EasyClusterizable is the basic reference
 * @param k : number of clusters
 * @param minShift : minimal threshold under which we consider a centroid has converged
 * @param maxIterations : maximal number of iteration
 * @param metric : a defined dissimilarity measure
 */
object KMeans {
	/**
	 * Run the K-Means with any continuous distance
	 */
	final def fit[D <: ContinuousDistance](
		data: RDD[Array[Double]],
		k: Int,
		metric: D,
		minShift: Double,
		maxIterations: Int,
		persistanceLVL: StorageLevel
		): KMeansModel[D] = {
		KMeans(k, metric, minShift, maxIterations, persistanceLVL).fit(scalarDataWithIDToClusterizable(data.zipWithIndex))
	}
}