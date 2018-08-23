package clustering4ever.scala.clustering.kmeans

import scala.collection.{mutable, immutable, GenSeq}
import clustering4ever.clustering.{CommonPredictClusteringModel, ClusteringModel}
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.clustering.datasetstype.DataSetsTypes
import clustering4ever.scala.clusterizables.RealClusterizable
import scala.reflect.ClassTag

/**
 * @author Beck Gaël
 */
sealed abstract class KMeansModel[S <: Seq[Double] : ClassTag] extends CommonPredictClusteringModel[S]
{
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input GenSeq with labels obtain via centerPredict method
	 */
	override def centerPredict(data: GenSeq[S]): GenSeq[(ClusterID, S)] = data.map( v => (centerPredict(v), v) )
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input GenSeq with labels obtain via centerPredict method
	 **/
	def centerPredict[ID: Numeric, Obj](data: GenSeq[RealClusterizable[ID, Obj, S]]): GenSeq[RealClusterizable[ID, Obj, S]] = data.map( rc => rc.setClusterID(centerPredict(rc.vector)) )
	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input GenSeq with labels obtain via knnPredict method
	 */
	override def knnPredict(data: GenSeq[S], k: Int, trainDS: Seq[(ClusterID, S)]): GenSeq[(ClusterID, S)] = data.map( v => (knnPredict(v, k, trainDS), v) )
	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input GenSeq with labels obtain via knnPredict method
	 */
	def knnPredict[ID: Numeric, Obj](data: GenSeq[RealClusterizable[ID, Obj, S]], k: Int, trainDS: Seq[(ClusterID, S)]): GenSeq[RealClusterizable[ID, Obj, S]] =
		data.map( rc => rc.setClusterID(knnPredict(rc.vector, k, trainDS)) )
	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input GenSeq with labels obtain via knnPredict method
	 */
	def knnPredict[ID: Numeric, Obj](data: GenSeq[RealClusterizable[ID, Obj, S]], k: Int, trainDS: Seq[RealClusterizable[ID, Obj, S]])(implicit i: DummyImplicit): GenSeq[RealClusterizable[ID, Obj, S]] =
		knnPredict(data, k, trainDS.map( rc => (rc.clusterID, rc.vector) ))
}

final class KMeansModelSeq(val centers: mutable.HashMap[Int, Seq[Double]], val metric: ContinuousDistance) extends KMeansModel[Seq[Double]]

final class KMeansModelCustom[S <: Seq[Double] : ClassTag](val centers: mutable.HashMap[Int, S], val metric: ContinuousDistance) extends KMeansModel[S]