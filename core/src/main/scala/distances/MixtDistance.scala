package clustering4ever.math.distances

import clustering4ever.scala.measurableclass.BinaryScalarVector
import _root_.clustering4ever.scala.clusterizables.ClusterizableM

/**
 * @author Beck Gaël
 **/
trait MixtDistance extends Distance[BinaryScalarVector]
{
	def d(vector1: BinaryScalarVector, vector2: BinaryScalarVector): Double
}

trait MixtDistanceClusterizable[ID, Obj] extends Distance[ClusterizableM[ID, Obj]]
{
	def d(vector1: ClusterizableM[ID, Obj], vector2: ClusterizableM[ID, Obj]): Double
}