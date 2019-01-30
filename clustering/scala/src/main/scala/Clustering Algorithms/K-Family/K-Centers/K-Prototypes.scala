package org.clustering4ever.clustering.kcenters.scala
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{immutable, GenSeq}
import scala.util.Random
import org.clustering4ever.math.distances.{MixtDistance, Distance}
import org.clustering4ever.math.distances.mixt.HammingAndEuclidean
import org.clustering4ever.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.util.ScalaCollectionImplicits._
import org.clustering4ever.vectors.{GVector, MixtVector}
/**
 * The famous K-Prototypes using a user-defined dissmilarity measure.
 * @param data : GenSeq of Clusterizable descendant, the EasyClusterizable is the basic reference
 * @param k : number of clusters seeked
 * @param epsilon : The stopping criteria, ie the distance under which centers are mooving from their previous position
 * @param maxIterations : maximal number of iteration
 * @param metric : a defined dissimilarity measure
 */
case class KPrototypes[Vb <: Seq[Int], Vs <: Seq[Double], D[X <: Seq[Int], Y <: Seq[Double]] <: MixtDistance[X, Y]](val k: Int, val metric: D[Vb, Vs], val epsilon: Double, val maxIterations: Int, val customCenters: immutable.HashMap[Int, MixtVector[Vb, Vs]] = immutable.HashMap.empty[Int, MixtVector[Vb, Vs]]) extends KCentersAncestor[MixtVector[Vb, Vs], D[Vb, Vs], KPrototypesModels[Vb, Vs, D]] {

	def run[ID, O, Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, MixtVector[Vb, Vs]]]): KPrototypesModels[Vb, Vs, D] = KPrototypesModels(k, metric, epsilon, maxIterations, obtainCenters(data))

}