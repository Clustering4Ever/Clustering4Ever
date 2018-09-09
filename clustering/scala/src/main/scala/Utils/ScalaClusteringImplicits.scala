package clustering4ever.util

import scala.collection.{mutable, GenSeq, parallel}
import clustering4ever.scala.vectorizables.{RealVector, BinaryVector, MixtVector}
import clustering4ever.scala.clusterizables.{RealClusterizable, BinaryClusterizable, MixtClusterizable}
import clustering4ever.scala.measurableclass.BinaryScalarVector

object ScalaClassicClusteringAlgorithmsImplicits extends CommonTypes
{
	type ShortMixtVector = BinaryScalarVector[MB[Int], MB[Double]]

	implicit def prepareDataWithIDToEuclideanKMeans[ID, V <: Seq[Double]](genSeq: GenSeq[(V, ID)])(implicit num: Numeric[ID]): GenSeq[RealClusterizable[Long, MB[Double], MB[Double]]] =
		genSeq.map{ case (vector, id) => GenerateClusterizable.obtainSimpleRealClusterizable(num.toLong(id), vector.toBuffer) }

	implicit def prepareDataWithIDToEuclideanKModes[ID, V <: Seq[Int]](genSeq: GenSeq[(V, ID)])(implicit num: Numeric[ID]): GenSeq[BinaryClusterizable[Long, MB[Int], MB[Int]]] =
		genSeq.map{ case (vector, id) => GenerateClusterizable.obtainSimpleBinaryClusterizable(num.toLong(id), vector.toBuffer) }

	implicit def prepareDataWithIDToEuclideanHammingKPrototypes[
		ID,
		Vb <: Seq[Int],
		Vs <: Seq[Double],
		V <: BinaryScalarVector[Vb, Vs]
		]
		(genSeq: GenSeq[(V, ID)])
		(implicit num: Numeric[ID]): GenSeq[
			MixtClusterizable[
				Long,
				ShortMixtVector,
				MB[Int],
				MB[Double],
				ShortMixtVector
			]
		] =
		genSeq.map{ case (vectors, id) => GenerateClusterizable.obtainSimpleMixtClusterizable[Long, MB[Int], MB[Double], ShortMixtVector](num.toLong(id), new BinaryScalarVector(vectors.binary.toBuffer, vectors.scalar.toBuffer)) }

	implicit def prepareToEuclideanKMeans[V <: Seq[Double]](genSeq: GenSeq[V]): GenSeq[RealClusterizable[Long, MB[Double], MB[Double]]] =
		prepareDataWithIDToEuclideanKMeans(genSeq.zipWithIndex)

	implicit def prepareToEuclideanKModes[V <: Seq[Int]](genSeq: GenSeq[V]): GenSeq[BinaryClusterizable[Long, MB[Int], MB[Int]]] =
		prepareDataWithIDToEuclideanKModes(genSeq.zipWithIndex)

	implicit def prepareToEuclideanHammingKPrototypes[Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs]](genSeq: GenSeq[V]): GenSeq[MixtClusterizable[Long, ShortMixtVector, MB[Int], MB[Double], ShortMixtVector]] =
		prepareDataWithIDToEuclideanHammingKPrototypes[Int, Vb, Vs, V](genSeq.zipWithIndex)
}