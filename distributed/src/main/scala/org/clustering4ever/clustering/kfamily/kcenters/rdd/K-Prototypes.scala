package org.clustering4ever.clustering.kfamily.kcenters.rdd

/**
 * @author Beck Gaël
 */
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.distances.MixedDistance
import org.clustering4ever.roottraits.{Clusterizable, GVector, MixedVector}
import org.clustering4ever.util.{ClusterBasicOperations, VectorsAddOperationsImplicits}

import scala.collection.immutable
import scala.language.higherKinds
import scala.reflect.ClassTag
/**
 * The famous K-Prototypes using a user-defined dissmilarity measure.
 * @param data :
 * @param k : number of clusters
 * @param minShift : minimal threshold under which we consider a centroid has converged
 * @param maxIterations : maximal number of iteration
 * @param metric : a defined dissimilarity measure
 */
final case class KPrototypes[D <: MixedDistance](val k: Int, val metric: D, val minShift: Double, val maxIterations: Int, val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, val customCenters: immutable.HashMap[Int, MixedVector] = immutable.HashMap.empty[Int, MixedVector])(protected implicit val ctV: ClassTag[MixedVector]) extends KCentersAncestor[MixedVector, D, KPrototypesModels[D]] {

	implicit val sumVector: (MixedVector, MixedVector) => MixedVector = VectorsAddOperationsImplicits.addMixedVectors

	implicit val getCenter: (MixedVector, Long) => MixedVector = (v, d) => {
	 	val binaryPart = ClusterBasicOperations.transformPreModeAndCastIt(v.binary, d)
	 	val realPart = ClusterBasicOperations.transformPreMeanAndCastItRaw(v.scalar, d)
		MixedVector(binaryPart, realPart)
	}

	val algorithmID = org.clustering4ever.roottraits.KPrototypes

	def fit[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, MixedVector]])(implicit ct: ClassTag[Cz[O, MixedVector]]): KPrototypesModels[D] = KPrototypesModels[D](k, metric, minShift, maxIterations, persistanceLVL, obtainCenters(data))
}