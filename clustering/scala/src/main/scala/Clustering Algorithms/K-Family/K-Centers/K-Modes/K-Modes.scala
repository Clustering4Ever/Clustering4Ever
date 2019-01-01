package org.clustering4ever.scala.clustering.kmodes
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{mutable, GenSeq}
import org.clustering4ever.math.distances.{Distance, BinaryDistance}
import org.clustering4ever.scala.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.scala.clustering.kcenters.{KCentersModel, KCenters}
import org.clustering4ever.util.ScalaImplicits._
import org.clustering4ever.scala.vectors.{GVector, BinaryVector}
/**
 *
 */
object KModes {
	/**
	 * Run the K-Modes with any binary distance
	 */
	def run[
		ID,
		O,
		V <: Seq[Int],
		Cz[X, Y, Z <: GVector] <: Clusterizable[X, Y, Z, Cz],
		D <: Distance[BinaryVector[V]]
	](
		data: GenSeq[Cz[ID, O, BinaryVector[V]]],
		k: Int,
		metric: D,
		maxIterations: Int,
		epsilon: Double,
		initializedCenters: mutable.HashMap[Int, BinaryVector[V]] = mutable.HashMap.empty[Int, BinaryVector[V]]
	)(implicit ct: ClassTag[Cz[ID, O, BinaryVector[V]]]): KCentersModel[BinaryVector[V], D] = {
		val kModesModel = KCenters.run(data, k, metric, epsilon, maxIterations, initializedCenters)
		kModesModel
	}
	/**
	 * Run the K-Modes with any binary distance
	 */
	def runRawData[V <: Seq[Int], D <: BinaryDistance[V], GS[Y] <: GenSeq[Y]](
		data: GS[V],
		k: Int,
		metric: D,
		maxIterations: Int,
		epsilon: Double = 0.01
	): KCentersModel[BinaryVector[V], D] = {
		val kModesModel = run(binaryToClusterizable(data), k, metric, maxIterations, epsilon)
		kModesModel
	}
}