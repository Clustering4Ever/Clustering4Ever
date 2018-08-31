package clustering4ever.util

import scala.collection.{mutable, GenSeq, parallel}
import clustering4ever.scala.vectorizables.{RealVector, BinaryVector, MixtVector}
import clustering4ever.scala.clusterizables.{RealClusterizable, BinaryClusterizable, MixtClusterizable}
import clustering4ever.scala.measurableclass.BinaryScalarVector

object ScalaClassicClusteringAlgorithmsImplicits extends CommonTypes
{
	type FastMixtVector = BinaryScalarVector[MB[Int], MB[Double]]

	implicit def prepareDataWithIDToEuclideanKMeans[ID, V <: Seq[Double]](seq: GenSeq[(V, ID)])(implicit num: Numeric[ID]): parallel.ParSeq[RealClusterizable[Long, MB[Double], MB[Double]]] =
		seq.par.map{ case (vector, id) => GenerateClusterizable.obtainSimpleRealClusterizable(num.toLong(id), vector.toBuffer) }

	implicit def prepareDataWithIDToEuclideanKModes[ID, V <: Seq[Int]](seq: GenSeq[(V, ID)])(implicit num: Numeric[ID]): parallel.ParSeq[BinaryClusterizable[Long, MB[Int], MB[Int]]] =
		seq.par.map{ case (vector, id) => GenerateClusterizable.obtainSimpleBinaryClusterizable(num.toLong(id), vector.toBuffer) }

	implicit def prepareDataWithIDToEuclideanHammingKPrototypes[
		ID,
		Vb <: Seq[Int],
		Vs <: Seq[Double],
		V <: BinaryScalarVector[Vb, Vs]
		]
		(seq: GenSeq[(V, ID)])
		(implicit num: Numeric[ID]): parallel.ParSeq[MixtClusterizable[
			Long,
			FastMixtVector,
			MB[Int],
			MB[Double],
			FastMixtVector
			]] =
		seq.par.map{ case (vectors, id) => GenerateClusterizable.obtainSimpleMixtClusterizable[Long, MB[Int], MB[Double], FastMixtVector](num.toLong(id), new BinaryScalarVector(vectors.binary.toBuffer, vectors.scalar.toBuffer)) }

	implicit def prepareToEuclideanKMeans[V <: Seq[Double]](seq: GenSeq[V]): parallel.ParSeq[RealClusterizable[Long, MB[Double], MB[Double]]] =
		prepareDataWithIDToEuclideanKMeans(seq.zipWithIndex)

	implicit def prepareToEuclideanKModes[V <: Seq[Int]](seq: GenSeq[V]): parallel.ParSeq[BinaryClusterizable[Long, MB[Int], MB[Int]]] =
		prepareDataWithIDToEuclideanKModes(seq.zipWithIndex)

	implicit def prepareToEuclideanHammingKPrototypes[Vb <: Seq[Int], Vs <: Seq[Double], V <: BinaryScalarVector[Vb, Vs]](seq: GenSeq[V]): parallel.ParSeq[MixtClusterizable[Long, FastMixtVector, MB[Int], MB[Double], FastMixtVector]] =
		prepareDataWithIDToEuclideanHammingKPrototypes[Int, Vb, Vs, V](seq.zipWithIndex)
}