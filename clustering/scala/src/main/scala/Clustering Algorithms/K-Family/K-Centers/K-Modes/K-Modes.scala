package org.clustering4ever.scala.clustering.kmodes
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{mutable, GenSeq}
import scala.util.Random
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.math.distances.binary.Hamming
import org.clustering4ever.scala.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.scala.clustering.kcenters.{KCentersModel, KCenters}
import org.clustering4ever.util.ScalaImplicits._
/**
 *
 */
object KModes {
	/**
	 * Run the K-Modes with any binary distance
	 */
	def run[
		ID: Numeric,
		O,
		V <: Seq[Int],
		Cz[ID, O, V] <: Clusterizable[ID, O, V, Cz[ID, O, V]],
		D <: BinaryDistance[V]
	](
		data: GenSeq[Cz[ID, O, V]],
		k: Int,
		epsilon: Double,
		maxIterations: Int,
		metric: D,
		initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V],
		workingVector: Int = 0
	)(implicit ct: ClassTag[V]): KCentersModel[ID, O, V, Cz[ID, O, V], D] = {
		val kmodes = new KCenters(k, epsilon, maxIterations, metric, initializedCenters)
		val kModesModel = kmodes.run(data)(workingVector)
		kModesModel
	}
	/**
	 * Run the K-Modes with any binary distance
	 */
	def runRawData[V[X] <: Seq[X], D <: BinaryDistance[V[Int]], GS[Y] <: GenSeq[Y]](
		data: GS[V[Int]],
		k: Int,
		metric: D,
		maxIterations: Int,
		epsilon: Double = 0.01,
		initializedCenters: mutable.HashMap[Int, V[Int]] = mutable.HashMap.empty[Int, V[Int]]
	)(implicit ct: ClassTag[V[Int]]): KCentersModel[Long, V[Int], V[Int], EasyClusterizable[Long, V[Int], V[Int]], D] = {
		val kmodes = new KCenters(k, epsilon, maxIterations, metric, initializedCenters)
		val kModesModel = kmodes.run(prepareToVectorsClustering(data))(0)
		kModesModel
	}
}