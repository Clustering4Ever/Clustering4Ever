package org.clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.GenSeq
import org.clustering4ever.scala.clusterizables.EasyClusterizable
import org.clustering4ever.scala.measurableclass.BinaryScalarVector
import scala.language.implicitConversions
import org.clustering4ever.preprocessing.DFCL
import org.clustering4ever.scala.vectorizables.Vector
/**
 *
 */
object ScalaImplicits {
	/**
	 *
	 */
	implicit def prepareDataWithIDToVectorsClustering[ID, N, V[N] <: Seq[N], GS[X] <: GenSeq[X]](genSeq: GS[(V[N], ID)])(implicit num: Numeric[ID]): GS[EasyClusterizable[Long, V[N], V[N]]] = {
		genSeq.map{ case (vector, id) => ClusterizableGenerator.obtainEasyClusterizable(num.toLong(id), vector) }.asInstanceOf[GS[EasyClusterizable[Long, V[N], V[N]]]]
	}
	/**
	 *
	 */
	implicit def prepareDataWithIDToMixtClustering[ID, Vb <: Seq[Int], Vs <: Seq[Double], GS[X] <: GenSeq[X]](genSeq: GS[(BinaryScalarVector[Vb, Vs], ID)])(implicit num: Numeric[ID]): GS[EasyClusterizable[Long, BinaryScalarVector[Vb, Vs], BinaryScalarVector[Vb, Vs]]] = {
		genSeq.map{ case (vectors, id) => ClusterizableGenerator.obtainEasyClusterizable(num.toLong(id), new BinaryScalarVector(vectors.binary, vectors.scalar)) }.asInstanceOf[GS[EasyClusterizable[Long, BinaryScalarVector[Vb, Vs], BinaryScalarVector[Vb, Vs]]]]
	}
	/**
	 *
	 */
	implicit def prepareToVectorsClustering[N, V[X] <: Seq[X], GS[Y] <: GenSeq[Y]](genSeq: GS[V[N]]): GS[EasyClusterizable[Long, V[N], V[N]]] = genSeq.zipWithIndex.asInstanceOf[GS[(V[N], Int)]]
	/**
	 *
	 */
	implicit def prepareToMixtClustering[Vb <: Seq[Int], Vs <: Seq[Double], GS[X] <: GenSeq[X]](genSeq: GenSeq[BinaryScalarVector[Vb, Vs]]): GenSeq[EasyClusterizable[Long, BinaryScalarVector[Vb, Vs], BinaryScalarVector[Vb, Vs]]] = prepareDataWithIDToMixtClustering(genSeq.zipWithIndex.asInstanceOf[GS[(BinaryScalarVector[Vb, Vs], Int)]])
	/**
	 *
	 */
	implicit def rawDataToDFCL[T, V[T] <: Seq[T], GS[X] <: GenSeq[X]](gs: GS[(V[T], Int)]): GS[DFCL[Int, V[T]]] = gs.zipWithIndex.map{ case ((v, l), id) => new DFCL(id, new Vector(v), l) }.asInstanceOf[GS[DFCL[Int, V[T]]]]
}