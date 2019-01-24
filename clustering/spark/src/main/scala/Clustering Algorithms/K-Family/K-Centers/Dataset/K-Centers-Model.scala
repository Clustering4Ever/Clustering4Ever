package org.clustering4ever.clustering.kcenters.dataset
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.immutable
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.Encoders
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.clustering.models.CenterModelDistributedCz
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixtVector}
import org.clustering4ever.clustering.dataset.ClusteringModelDistributedDS
/**
 *
 */
trait KCentersModelAncestor[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D <: Distance[V], +Args <: KCentersArgsAncestor[V, D]] extends CenterModelDistributedCz[ID, O, V, Cz, D] with ClusteringModelDistributedDS[ID, O, V, Cz, Args] {
	/**
	 * kryo Serialization if true, java one else
	 */
	private val encoder = if(args.kryoSerialization) Encoders.kryo[Cz[ID, O, V]] else Encoders.javaSerialization[Cz[ID, O, V]]
	/**
	 *
	 */
	def obtainClustering(data: Dataset[Cz[ID, O, V]]): Dataset[Cz[ID, O, V]] = data.map( cz => cz.addClusterIDs(centerPredict(cz.v)) )(encoder)
}
/**
 *
 */
case class KMeansModel[ID, O, V <: Seq[Double], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Double]] <: ContinuousDistance[X]](val centers: immutable.HashMap[Int, ScalarVector[V]], val metric: D[V], val args: KMeansArgs[V, D])(protected implicit val ct: ClassTag[Cz[ID, O, ScalarVector[V]]]) extends KCentersModelAncestor[ID, O, ScalarVector[V], Cz, D[V], KMeansArgs[V, D]]