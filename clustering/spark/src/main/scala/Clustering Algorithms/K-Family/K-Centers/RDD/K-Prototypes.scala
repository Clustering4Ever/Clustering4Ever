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
/**
 * The famous K-Prototypes using a user-defined dissmilarity measure.
 * @param data :
 * @param k : number of clusters
 * @param minShift : minimal threshold under which we consider a centroid has converged
 * @param maxIterations : maximal number of iteration
 * @param metric : a defined dissimilarity measure
 */
final case class KPrototypes[Vb <: Seq[Int], Vs <: Seq[Double], D[X <: Seq[Int], Y <: Seq[Double]] <: MixedDistance[X, Y]](final val k: Int, final val metric: D[Vb, Vs], final val minShift: Double, final val maxIterations: Int, final val persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, final val customCenters: immutable.HashMap[Int, MixedVector[Vb, Vs]] = immutable.HashMap.empty[Int, MixedVector[Vb, Vs]])(protected implicit final val ctV: ClassTag[MixedVector[Vb, Vs]]) extends KCentersAncestor[MixedVector[Vb, Vs], D[Vb, Vs], KPrototypesModels[Vb, Vs, D]] {

	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KPrototypes

	final def fit[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, MixedVector[Vb, Vs]]])(implicit ct: ClassTag[Cz[O, MixedVector[Vb, Vs]]]): KPrototypesModels[Vb, Vs, D] = KPrototypesModels[Vb, Vs, D](k, metric, minShift, maxIterations, persistanceLVL, obtainCenters(data))
}