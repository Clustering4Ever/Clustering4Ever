package org.clustering4ever.clustering.kcenters.scala
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{immutable, GenSeq}
import scala.util.Random
import org.clustering4ever.math.distances.{MixedDistance, Distance}
import org.clustering4ever.math.distances.mixt.HammingAndEuclidean
import org.clustering4ever.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.util.ScalaCollectionImplicits._
import org.clustering4ever.vectors.{GVector, MixedVector}
/**
 * The famous K-Prototypes using a user-defined dissmilarity measure.
 * @param data GenSeq of Clusterizable descendant, the EasyClusterizable is the basic reference
 * @param k number of clusters seeked
 * @param minShift The stopping criteria, ie the distance under which centers are mooving from their previous position
 * @param maxIterations maximal number of iteration
 * @param metric a defined dissimilarity measure
 */
final case class KPrototypes[Vb <: Seq[Int], Vs <: Seq[Double], D[X <: Seq[Int], Y <: Seq[Double]] <: MixedDistance[X, Y]](final val k: Int, final val metric: D[Vb, Vs], final val minShift: Double, final val maxIterations: Int, final val customCenters: immutable.HashMap[Int, MixedVector[Vb, Vs]] = immutable.HashMap.empty[Int, MixedVector[Vb, Vs]]) extends KCentersAncestor[MixedVector[Vb, Vs], D[Vb, Vs], KPrototypesModels[Vb, Vs, D]] {

	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.KPrototypes

	final def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, MixedVector[Vb, Vs]]]): KPrototypesModels[Vb, Vs, D] = KPrototypesModels(k, metric, minShift, maxIterations, obtainCenters(data))

}