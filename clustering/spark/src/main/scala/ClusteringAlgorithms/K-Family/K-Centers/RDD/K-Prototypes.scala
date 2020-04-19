package org.clustering4ever.clustering.kcenters.rdd
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.immutable
import scala.util.Random
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.math.distances.MixedDistance
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.math.distances.binary.Hamming
import org.clustering4ever.math.distances.mixt.HammingAndEuclidean
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.util.SparkImplicits._
import org.clustering4ever.vectors.{GVector, MixedVector}
import org.clustering4ever.util.ClusterBasicOperations
/**
 * The famous K-Prototypes using a user-defined dissmilarity measure.
 * @param data :
 * @param k : number of clusters
 * @param minShift : minimal threshold under which we consider a centroid has converged
 * @param maxIterations : maximal number of iteration
 * @param metric : a defined dissimilarity measure
 */
final case class KPrototypes[D <: MixedDistance](final val k: Int, final val metric: D, final val minShift: Double, final val maxIterations: Int, final val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, final val customCenters: immutable.HashMap[Int, MixedVector] = immutable.HashMap.empty[Int, MixedVector])(protected implicit final val ctV: ClassTag[MixedVector]) extends KCentersAncestor[MixedVector, D, KPrototypesModels[D]] {

	implicit val sumVector: (MixedVector, MixedVector) => MixedVector = org.clustering4ever.util.VectorsAddOperationsImplicits.addMixedVectors

	implicit val getCenter: (MixedVector, Long) => MixedVector = (v, d) => {
	 	val binaryPart = ClusterBasicOperations.transformPreModeAndCastIt(v.binary, d)
	 	val realPart = ClusterBasicOperations.transformPreMeanAndCastItRaw(v.scalar, d)
		MixedVector(binaryPart, realPart)
	}

	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KPrototypes

	final def fit[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, MixedVector]])(implicit ct: ClassTag[Cz[O, MixedVector]]): KPrototypesModels[D] = KPrototypesModels[D](k, metric, minShift, maxIterations, persistanceLVL, obtainCenters(data))
}