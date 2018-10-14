package clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.collection.{mutable, GenSeq, parallel}
import clustering4ever.scala.vectorizables.{RealVector, BinaryVector, MixtVector}
import clustering4ever.scala.clusterizables.{SimpleRealClusterizable, SimpleBinaryClusterizable, SimpleMixtClusterizable}
import clustering4ever.scala.measurableclass.BinaryScalarVector
import scala.language.implicitConversions

object ScalaImplicits {

	type MAB[N] = mutable.ArrayBuffer[N]
	type MixtVector = BinaryScalarVector[MAB[Int], MAB[Double]]

	implicit def prepareDataWithIDToRealClustering[ID, V <: Seq[Double]](genSeq: GenSeq[(V, ID)])(implicit num: Numeric[ID]): GenSeq[SimpleRealClusterizable[Long, MAB[Double], MAB[Double]]] =
		genSeq.map{ case (vector, id) => GenerateClusterizable.obtainSimpleRealClusterizable(num.toLong(id), mutable.ArrayBuffer(vector:_*)) }

	implicit def prepareDataWithIDToBinaryClustering[ID, V <: Seq[Int]](genSeq: GenSeq[(V, ID)])(implicit num: Numeric[ID]): GenSeq[SimpleBinaryClusterizable[Long, MAB[Int], MAB[Int]]] =
		genSeq.map{ case (vector, id) => GenerateClusterizable.obtainSimpleBinaryClusterizable(num.toLong(id), mutable.ArrayBuffer(vector:_*)) }

	implicit def prepareDataWithIDToMixtClustering[
		ID,
		Vb <: Seq[Int],
		Vs <: Seq[Double],
		V <: BinaryScalarVector[Vb, Vs]
		]
		(genSeq: GenSeq[(V, ID)])(implicit num: Numeric[ID]): GenSeq[
			SimpleMixtClusterizable[
				Long,
				MixtVector,
				MAB[Int],
				MAB[Double],
				MixtVector
			]
		] =
		genSeq.map{ case (vectors, id) => GenerateClusterizable.obtainSimpleMixtClusterizable[Long, MAB[Int], MAB[Double], MixtVector](num.toLong(id), new BinaryScalarVector(mutable.ArrayBuffer(vectors.binary:_*), mutable.ArrayBuffer(vectors.scalar:_*))) }

	implicit def prepareToRealClustering[V <: Seq[Double]](genSeq: GenSeq[V]): GenSeq[SimpleRealClusterizable[Long, MAB[Double], MAB[Double]]] =
		prepareDataWithIDToRealClustering(genSeq.zipWithIndex)

	implicit def prepareToBinaryClustering[V <: Seq[Int]](genSeq: GenSeq[V]): GenSeq[SimpleBinaryClusterizable[Long, MAB[Int], MAB[Int]]] =
		prepareDataWithIDToBinaryClustering(genSeq.zipWithIndex)

	implicit def prepareToMixtClustering[Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs]](genSeq: GenSeq[V]): GenSeq[SimpleMixtClusterizable[Long, MixtVector, MAB[Int], MAB[Double], MixtVector]] =
		prepareDataWithIDToMixtClustering[Int, Vb, Vs, V](genSeq.zipWithIndex)
}