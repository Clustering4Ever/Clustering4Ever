package org.clustering4ever.clustering.chaining
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.GenSeq
import shapeless._
import org.clustering4ever.scala.clusterizables.Clusterizable
import org.clustering4ever.scala.vectors.{GVector, ScalarVector, BinaryVector}
import org.clustering4ever.shapelesslinked.VMapping
import org.clustering4ever.enums.ClusteringAlgorithmEnum
import org.clustering4ever.enums.ClusteringAlgorithmEnum._
import org.clustering4ever.scala.clustering.kcenters.KCenters
import org.clustering4ever.clustering.{ClusteringChaining, ClusteringAlgorithmGen}
/**
 *
 */
class ClusteringChainingLocal[
    ID,
    O,
    V <: GVector,
    Cz[X, Y, Z <: GVector] <: Clusterizable[X, Y, Z, Cz],
    GS[X] <: GenSeq[X]
](val data: GS[Cz[ID, O, V]]) extends ClusteringChaining[ID, O, V, Cz, GS, ClusteringChainingLocal[ID, O, _, Cz, GS]] {
	/**
	 *
	 */
	// def enumToClustAlgo(enum: ClusteringAlgorithmEnum): ClusteringAlgorithmGen[V, GS] = {
	// 	enum match {
	// 		case KMeans => new KCenters[V, GS]
	// 	}
	// }
    /**
     *
     */
    def newVectorization[NV <: GVector](vectorizationID: Int, towardNewVector: O => NV): ClusteringChainingLocal[ID, O, NV, Cz, GS] = {
        new ClusteringChainingLocal(data.map(_.addVectorized(vectorizationID, towardNewVector)).asInstanceOf[GS[Cz[ID, O, NV]]])
    }
    /**
     *
     */
    def updtWorkingVector[NV <: GVector](vectorizationID: Int)(implicit vMapping: VMapping[Int, NV] = new VMapping[Int, NV]): ClusteringChainingLocal[ID, O, NV, Cz, GS] = {
        new ClusteringChainingLocal(data.map(_.updtWorkingVector(vectorizationID)).asInstanceOf[GS[Cz[ID, O, NV]]])
    }

}

