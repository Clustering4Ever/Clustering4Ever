package org.clustering4ever.scala.clustering.kcenters
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{mutable, GenSeq}
import org.clustering4ever.math.distances.{Distance, BinaryDistance}
import org.clustering4ever.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.util.ScalaImplicits._
import org.clustering4ever.vectors.{GVector, BinaryVector}
import org.clustering4ever.vectorizables.NotVectorizable
/**
 *
 */
case class KModesArgs[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X]](val k: Int, val metric: D[V], val epsilon: Double, val maxIterations: Int, val initializedCenters: mutable.HashMap[Int, BinaryVector[V]] = mutable.HashMap.empty[Int, BinaryVector[V]]) extends KCentersArgsTrait[BinaryVector[V], D[V]] {
	override val algorithm = org.clustering4ever.extensibleAlgorithmNature.KModes
}
/**
 *
 */
object KModes {
	/**
	 * Run the K-Modes with any binary distance
	 */
	def run[ID, O, V <: Seq[Int], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Int]] <: BinaryDistance[X], GS[Y] <: GenSeq[Y]](
		data: GS[Cz[ID, O, BinaryVector[V]]],
		k: Int,
		metric: D[V],
		maxIterations: Int,
		epsilon: Double,
		initializedCenters: mutable.HashMap[Int, BinaryVector[V]] = mutable.HashMap.empty[Int, BinaryVector[V]]
	)(implicit ct: ClassTag[Cz[ID, O, BinaryVector[V]]]): KCentersModel[ID, O, BinaryVector[V], Cz, D[V], GS] = {
		
		val kmodesAlgorithm = new KCenters[ID, O, BinaryVector[V], Cz, D[V], GS, KModesArgs[V, D]](KModesArgs(k, metric, epsilon, maxIterations, initializedCenters))
		kmodesAlgorithm.run(data)
	
	}
	/**
	 * Run the K-Modes with any binary distance
	 */
	def run[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X], GS[Y] <: GenSeq[Y]](
		data: GS[V],
		k: Int,
		metric: D[V],
		maxIterations: Int,
		epsilon: Double
	): KCentersModel[Int, NotVectorizable.type, BinaryVector[V], EasyClusterizable, D[V], GS] = {
		val kModesModel = run(binaryToClusterizable(data), k, metric, maxIterations, epsilon)
		kModesModel
	}
}