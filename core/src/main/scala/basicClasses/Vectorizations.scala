package org.clustering4ever.vectorizations
/**
 * @author Beck Gaël
 */
import org.clustering4ever.vectors.GVector
import scala.collection.immutable
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping}
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.clustering.ClusteringCommons
/**
 *
 */
sealed trait VectorizationNature extends Serializable
/**
 *
 */
object Scalar extends VectorizationNature
/**
 *
 */
object Binary extends VectorizationNature
/**
 *
 */
object Mixt extends VectorizationNature
/**
 *
 */
object Other extends VectorizationNature
/**
 *
 */
object Default extends VectorizationNature
/**
 *
 */
trait Vectorization[O, V <: GVector[V]]  extends ClusteringCommons {
	
	val vectorizationID: VectorizationID

	val vectorizationFct: Option[O => V]
	
	val outputFeaturesNames: immutable.Vector[String]

	val clusteringNumbers: immutable.HashSet[ClusterID]

	def updateClustering(clusteringIDs: Int*): Vectorization[O, V]

	val vMapping = new VMapping[VectorizationID, V]

	val vectoMapping: VectorizationMapping[VectorizationID, Vectorization[O, V]]
}
/**
 *
 */
case class EasyVectorization[O, V <: GVector[V]](
	val vectorizationID: VectorizationID,
	val vectorizationFct: Option[O => V] = None,
	val clusteringNumbers: immutable.HashSet[Int] = immutable.HashSet.empty[Int],
	val outputFeaturesNames: immutable.Vector[String] = immutable.Vector.empty[String]
) extends Vectorization[O, V] {
	/**
	 *
	 */
	val vectoMapping = new VectorizationMapping[VectorizationID, EasyVectorization[O, V]]

	def updateClustering(clusteringIDs: Int*): EasyVectorization[O, V] = copy(clusteringNumbers = clusteringNumbers ++ clusteringIDs)

}
