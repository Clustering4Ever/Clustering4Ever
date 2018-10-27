package clustering4ever.util
/**
 * @author Beck Gaël
 */
import clustering4ever.scala.vectorizables.{RealVector, BinaryVector, MixtVector}
import clustering4ever.scala.clusterizables.{SimpleRealClusterizable, SimpleBinaryClusterizable, SimpleMixtClusterizable}
import clustering4ever.scala.measurableclass.BinaryScalarVector
/**
 *
 */
object GenerateClusterizable {

	def obtainSimpleRealClusterizable[ID: Numeric, V <: Seq[Double]](id: ID, vector: V): SimpleRealClusterizable[ID, V, V] = new SimpleRealClusterizable[ID, V, V](id, new RealVector[V](vector))

	def obtainSimpleBinaryClusterizable[ID: Numeric, V <: Seq[Int]](id: ID, vector: V): SimpleBinaryClusterizable[ID, V, V] = new SimpleBinaryClusterizable[ID, V, V](id, new BinaryVector[V](vector))

	def obtainSimpleMixtClusterizable[ID: Numeric, Vb <: Seq[Int], Vs <: Seq[Double]](id: ID, vectors: BinaryScalarVector[Vb, Vs]): SimpleMixtClusterizable[ID, BinaryScalarVector[Vb, Vs], Vb, Vs] = new SimpleMixtClusterizable(id, new MixtVector(vectors))
}