package clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.collection.{mutable, GenSeq, parallel}
import clustering4ever.scala.clusterizables.{SimpleRealClusterizable, SimpleBinaryClusterizable, SimpleMixtClusterizable}
import clustering4ever.scala.measurableclass.BinaryScalarVector
import scala.language.implicitConversions
/**
 *
 */
object ScalaImplicits {
	/**
	 *
	 */
	private type MAB[N] = mutable.ArrayBuffer[N]
	/**
	 *
	 */
	implicit def prepareDataWithIDToRealClustering[ID, V <: Seq[Double]](genSeq: GenSeq[(V, ID)])(implicit num: Numeric[ID]): GenSeq[SimpleRealClusterizable[Long, V, V]] = {
		genSeq.map{ case (vector, id) => GenerateClusterizable.obtainSimpleRealClusterizable(num.toLong(id), vector) }
	}
	/**
	 *
	 */
	implicit def prepareDataWithIDToBinaryClustering[ID, V <: Seq[Int]](genSeq: GenSeq[(V, ID)])(implicit num: Numeric[ID]): GenSeq[SimpleBinaryClusterizable[Long, V, V]] = {
		genSeq.map{ case (vector, id) => GenerateClusterizable.obtainSimpleBinaryClusterizable(num.toLong(id), vector) }
	}
	/**
	 *
	 */
	implicit def prepareDataWithIDToMixtClustering[ID, Vb <: Seq[Int], Vs <: Seq[Double]](genSeq: GenSeq[(BinaryScalarVector[Vb, Vs], ID)])(implicit num: Numeric[ID]): GenSeq[SimpleMixtClusterizable[Long, BinaryScalarVector[Vb, Vs], Vb, Vs]] = {
		genSeq.map{ case (vectors, id) => GenerateClusterizable.obtainSimpleMixtClusterizable(num.toLong(id), new BinaryScalarVector(vectors.binary, vectors.scalar)) }
	}
	/**
	 *
	 */
	implicit def prepareToRealClustering[V <: Seq[Double]](genSeq: GenSeq[V]): GenSeq[SimpleRealClusterizable[Long, V, V]] = {
		prepareDataWithIDToRealClustering(genSeq.zipWithIndex)
	}
	/**
	 *
	 */
	implicit def prepareToBinaryClustering[V <: Seq[Int]](genSeq: GenSeq[V]): GenSeq[SimpleBinaryClusterizable[Long, V, V]] = {
		prepareDataWithIDToBinaryClustering(genSeq.zipWithIndex)
	}
	/**
	 *
	 */
	implicit def prepareToMixtClustering[Vb <: Seq[Int], Vs <: Seq[Double]](genSeq: GenSeq[BinaryScalarVector[Vb, Vs]]): GenSeq[SimpleMixtClusterizable[Long, BinaryScalarVector[Vb, Vs], Vb, Vs]] = {
		prepareDataWithIDToMixtClustering(genSeq.zipWithIndex)
	}
}