package org.clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.collection.GenSeq
import org.clustering4ever.scala.clusterizables.EasyClusterizable
import org.clustering4ever.preprocessing.DFCL
import org.clustering4ever.scala.vectorizables.Vectorizable
import org.clustering4ever.scala.vectors.{BinaryVector, ScalarVector, MixtVector}
/**
 *
 */
object ScalaImplicits {
	/**
	 *
	 */
	implicit def scalarDataWithIDToClusterizable[ID, V <: Seq[Double], GS[Y] <: GenSeq[Y]](genSeq: GS[(V, ID)]): GS[EasyClusterizable[ID, ScalarVector[V], ScalarVector[V]]] = {
		genSeq.map{ case (vector, id) => ClusterizableGenerator.obtainEasyClusterizable(id, new ScalarVector(vector)) }.asInstanceOf[GS[EasyClusterizable[ID, ScalarVector[V], ScalarVector[V]]]]
	}
	/**
	 *
	 */
	implicit def binaryDataWithIDToClusterizable[ID, V <: Seq[Int], GS[Y] <: GenSeq[Y]](genSeq: GS[(V, ID)]): GS[EasyClusterizable[ID, BinaryVector[V], BinaryVector[V]]] = {
		genSeq.map{ case (vector, id) => ClusterizableGenerator.obtainEasyClusterizable(id, new BinaryVector(vector)) }.asInstanceOf[GS[EasyClusterizable[ID, BinaryVector[V], BinaryVector[V]]]]
	}
	/**
	 *
	 */
	implicit def mixtDataWithIDToClusterizable[ID, Vb <: Seq[Int], Vs <: Seq[Double], GS[X] <: GenSeq[X]](genSeq: GS[((Vb, Vs), ID)]): GS[EasyClusterizable[ID, MixtVector[Vb, Vs], MixtVector[Vb, Vs]]] = {
		genSeq.map{ case ((binary, scalar), id) => ClusterizableGenerator.obtainEasyClusterizable(id, new MixtVector(binary, scalar)) }.asInstanceOf[GS[EasyClusterizable[ID, MixtVector[Vb, Vs], MixtVector[Vb, Vs]]]]
	}
	/**
	 *
	 */
	implicit def scalarToClusterizable[V <: Seq[Double], GS[Y] <: GenSeq[Y]](genSeq: GS[V]): GS[EasyClusterizable[Int, ScalarVector[V], ScalarVector[V]]] = genSeq.zipWithIndex.asInstanceOf[GS[(V, Int)]]
	/**
	 *
	 */
	implicit def binaryToClusterizable[V <: Seq[Int], GS[Y] <: GenSeq[Y]](genSeq: GS[V]): GS[EasyClusterizable[Int, BinaryVector[V], BinaryVector[V]]] = genSeq.zipWithIndex.asInstanceOf[GS[(V, Int)]]
	/**
	 *
	 */
	implicit def mixtToClusterizable[Vb <: Seq[Int], Vs <: Seq[Double], GS[X] <: GenSeq[X]](genSeq: GS[(Vb, Vs)]): GS[EasyClusterizable[Int, MixtVector[Vb, Vs], MixtVector[Vb, Vs]]] = genSeq.zipWithIndex.asInstanceOf[GS[((Vb, Vs), Int)]]
	/**
	 *
	 */
	implicit def rawDataToDFCL[T, V[X] <: Seq[X], GS[Y] <: GenSeq[Y]](gs: GS[(V[T], Int)]): GS[DFCL[Int, V[T]]] = gs.zipWithIndex.map{ case ((v, l), id) => new DFCL(id, l, v) }.asInstanceOf[GS[DFCL[Int, V[T]]]]
}