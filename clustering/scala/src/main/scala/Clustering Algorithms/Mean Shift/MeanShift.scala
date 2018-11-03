package clustering4ever.scala.clustering.meanshift
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import clustering4ever.clustering.{LocalClusteringAlgorithm, MeanShiftable, ClusteringModel}
import scala.math.{min, max}
import scala.collection.GenSeq
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.scala.clusterizables.{RealClusterizable, SimpleRealClusterizable}
import clustering4ever.scala.kernels.{Kernel, KernelArgs}
import clustering4ever.scala.clustering.rla.RLA
import clustering4ever.clustering.ClusteringModel
import clustering4ever.scala.basicenum.AlternativeVectorNature._
/**
 *
 */
class MeanShift[
	@specialized(Int, Long) ID: Numeric,
	O,
	V <: Seq[Double],
	Cz[ID, O, V <: Seq[Double]] <: RealClusterizable[ID, O, V, Cz[ID, O, V]],
	D <: ContinuousDistance[V],
	KArgs <: KernelArgs,
	K[V, KArgs <: KernelArgs] <: Kernel[V, KArgs],
	CA <: MeanShiftable[GenSeq[SimpleRealClusterizable[Long, V, V]]]
](
	epsilon: Double,
	maxIterations: Int,
	kernel: K[V, KArgs],
	metric: D,
	clusteringAlgorithm: CA
) extends LocalClusteringAlgorithm[GenSeq[Cz[ID, O, V]]] {

	def run(data: GenSeq[Cz[ID, O, V]]): MeanShiftModel[ID, O, V, Cz[ID, O, V], CA#ClusteringModelType] = {

		val ascendedData: GenSeq[Cz[ID, O, V]] = (new GradientAscent(data, epsilon, maxIterations, kernel, metric)).run()

		val learnableData = ascendedData.map( cz => (cz.altVectors.get(Gradient_Ascent), cz.id) )

		import clustering4ever.util.ScalaImplicits._
		val clusterModel = clusteringAlgorithm.castingModel(clusteringAlgorithm.run(learnableData))
	
		new MeanShiftModel[ID, O, V, Cz[ID, O, V], CA#ClusteringModelType](ascendedData, clusterModel)
	}
}
/**
 * Mean Shift model which contains the model from the clustering part and the dataset with the mode under Cz.altVectors.get(Gradient_Ascent)
 */
class MeanShiftModel[@specialized(Int, Long) ID: Numeric, O, V <: Seq[Double], Cz <: RealClusterizable[ID, O, V, Cz], CM <: ClusteringModel](val ascendedData: GenSeq[Cz], val clusterModel: CM) extends ClusteringModel
/**
 *
 */
object MeanShift {
	/**
	 *
	 */
	def run[
		ID: Numeric,
		O,
		V[Double] <: Seq[Double],
		Cz[ID, O, V <: Seq[Double]] <: RealClusterizable[ID, O, V, Cz[ID, O, V]],
		D <: ContinuousDistance[V[Double]],
		KArgs <: KernelArgs,
		K[V, KArgs <: KernelArgs] <: Kernel[V, KArgs],
		CA <: MeanShiftable[GenSeq[SimpleRealClusterizable[Long, V[Double], V[Double]]]]
	](
		data: GenSeq[Cz[ID, O, V[Double]]],
		epsilon: Double,
		maxIterations: Int,
		kernel: K[V[Double], KArgs],
		metric: D,
		clusteringAlgorithm: CA): MeanShiftModel[ID, O, V[Double], Cz[ID, O, V[Double]], CA#ClusteringModelType] = {
		(new MeanShift[ID, O, V[Double], Cz, D, KArgs, K, CA](epsilon, maxIterations, kernel, metric, clusteringAlgorithm)).run(data)
	}
}