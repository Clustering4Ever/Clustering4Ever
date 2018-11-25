package org.clustering4ever.scala.clustering.kcenters
/**
 * @author Beck Gaël
 */
import scala.collection.{mutable, GenSeq}
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.scala.clusterizables.Clusterizable
import org.clustering4ever.clustering.CenterOrientedModel
/**
 *
 */
class KCentersModel[
	ID: Numeric,
	O,
	V,
	Cz <: Clusterizable[ID, O, V, Cz],
	D <: Distance[V]
	](
	val centers: mutable.HashMap[Int, V],
	val metric: D,
	var workingVector: Int
) extends CenterOrientedModel[V, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	def centerPredict(data: GenSeq[Cz])(implicit i: DummyImplicit): GenSeq[Cz] = data.map( rc => rc.addClusterID(centerPredict(rc.vector(workingVector))) )
	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input Seq with labels obtain via knnPredict method
	 */
	def knnPredict(data: GenSeq[Cz], k: Int, trainDS: Seq[Cz], clusteringNumber: Int = 0)(implicit i: DummyImplicit): GenSeq[Cz] = knnPredict(data, k, trainDS.map( rc => (rc.clusterID(clusteringNumber), rc.vector(workingVector)) ))
	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input Seq with labels obtain via knnPredict method
	 */
	def knnPredict(data: GenSeq[Cz], k: Int, trainDS: Seq[(ClusterID, V)]): GenSeq[Cz] = data.map( rc => rc.addClusterID(knnPredict(rc.vector(workingVector), k, trainDS)) )
}