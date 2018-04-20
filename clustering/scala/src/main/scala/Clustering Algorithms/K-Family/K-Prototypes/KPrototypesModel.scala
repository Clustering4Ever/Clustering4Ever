package clustering4ever.scala.clustering.kprotoypes

import _root_.clustering4ever.clustering.ClusteringModel
import _root_.scala.collection.mutable
import _root_.clustering4ever.math.distances.MixtDistance
import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes
import _root_.clustering4ever.scala.measurableclass.BinaryScalarVector

/**
 * @author Beck Gaël
 **/
class KPrototypesModel(val centers: mutable.HashMap[Int, BinaryScalarVector], val metric: MixtDistance) extends ClusteringModel with DataSetsTypes[Int, BinaryScalarVector]
{
	val centersAsArray = centers.toArray
	/**
	 * Return the nearest mode for a specific point
	 **/
	def predict(v: BinaryScalarVector): ClusterID =
	{
		centersAsArray.map{ case(clusterID, centroid) => (clusterID, metric.d(centroid, v)) }.minBy(_._2)._1
	}
	/**
	 * Return the nearest mode for a dataset
	 **/
	def predict(data: Seq[BinaryScalarVector]): Seq[(ClusterID, Vector)] =
	{
		data.map( v => (predict(v), v) )
	}
}