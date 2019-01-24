package org.clustering4ever.clustering.kcenters.scala
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{immutable, GenSeq}
import org.clustering4ever.math.distances.{Distance, BinaryDistance}
import org.clustering4ever.math.distances.binary.Hamming
import org.clustering4ever.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.util.ScalaCollectionImplicits._
import org.clustering4ever.vectors.{GVector, BinaryVector}
/**
 *
 */
case class KModes[ID, O, V <: Seq[Int], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Int]] <: BinaryDistance[X], GS[X] <: GenSeq[X]](val args: KModesArgs[V, D])(protected implicit val ct: ClassTag[Cz[ID, O, BinaryVector[V]]]) extends KCentersAncestor[ID, O, BinaryVector[V], Cz, D[V], GS, KModesArgs[V, D], KModesModel[ID, O, V, Cz, D, GS]] {

	def run(data: GS[Cz[ID, O, BinaryVector[V]]]): KModesModel[ID, O, V, Cz, D, GS] = new KModesModel(obtainCenters(data), args.metric, args)
}
/**
 *
 */
object KModes {
	/**
	 *
	 */
	def generateAnyArgumentsCombination[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X]](
		kValues: Seq[Int] = Seq(4, 6, 8),
		metricValues: Seq[D[V]] = Seq(Hamming[V] _),
		epsilonValues: Seq[Double] = Seq(0.0001),
		maxIterationsValues: Seq[Int] = Seq(100),
		initializedCentersValues: Seq[immutable.HashMap[Int, BinaryVector[V]]] = Seq(immutable.HashMap.empty[Int, BinaryVector[V]])): Seq[KModesArgs[V, D]] = {
		for(
			k <- kValues;
			metric <- metricValues;
			epsilon <- epsilonValues;
			maxIterations <- maxIterationsValues;
			initializedCenters <- initializedCentersValues
		) yield	KModesArgs(k, metric, epsilon, maxIterations, initializedCenters)
	}
	/**
	 * Run the K-Modes with any binary distance
	 */
	def run[ID, O, V <: Seq[Int], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Int]] <: BinaryDistance[X], GS[Y] <: GenSeq[Y]](
		data: GS[Cz[ID, O, BinaryVector[V]]],
		k: Int,
		metric: D[V],
		maxIterations: Int,
		epsilon: Double,
		initializedCenters: immutable.HashMap[Int, BinaryVector[V]] = immutable.HashMap.empty[Int, BinaryVector[V]]
	)(implicit ct: ClassTag[Cz[ID, O, BinaryVector[V]]]): KModesModel[ID, O, V, Cz, D, GS] = {
		
		val kmodesAlgorithm = new KModes[ID, O, V, Cz, D, GS](KModesArgs(k, metric, epsilon, maxIterations, initializedCenters))
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
	): KModesModel[Int, BinaryVector[V], V, EasyClusterizable, D, GS] = {
		val kModesModel = run(binaryToClusterizable(data), k, metric, maxIterations, epsilon)
		kModesModel
	}
}