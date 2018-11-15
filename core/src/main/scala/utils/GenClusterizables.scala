package org.clustering4ever.util
/**
 * @author Beck Gaël
 */
import org.clustering4ever.scala.vectorizables.{RealVector, BinaryVector, MixtVector}
import org.clustering4ever.scala.clusterizables.EasyClusterizable
import org.clustering4ever.scala.measurableclass.BinaryScalarVector
/**
 *
 */
object GenerateClusterizable {
	/**
	 *
	 */
	def obtainEasyRealClusterizable[@specialized(Int, Long) ID: Numeric, V <: Seq[Double]](id: ID, vector: V): EasyClusterizable[ID, V, V] = new EasyClusterizable[ID, V, V](id, new RealVector[V](vector))
	/**
	 *
	 */
	def obtainEasyBinaryClusterizable[@specialized(Int, Long) ID: Numeric, V <: Seq[Int]](id: ID, vector: V): EasyClusterizable[ID, V, V] = new EasyClusterizable[ID, V, V](id, new BinaryVector[V](vector))
	/**
	 *
	 */
	def obtainSimpleMixtClusterizable[@specialized(Int, Long) ID: Numeric, Vb <: Seq[Int], Vs <: Seq[Double]](id: ID, vectors: BinaryScalarVector[Vb, Vs]): EasyClusterizable[ID, BinaryScalarVector[Vb, Vs], BinaryScalarVector[Vb, Vs]] = new EasyClusterizable(id, new MixtVector(vectors))
}