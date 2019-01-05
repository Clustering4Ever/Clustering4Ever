package org.clustering4ever.clustering.chaining
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.GenSeq
import shapeless._
import org.clustering4ever.clustering.{ClusteringArgs, ClusteringModelCz}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.scala.vectors.{GVector, ScalarVector, BinaryVector}
import org.clustering4ever.shapeless.VMapping
import org.clustering4ever.enums.ClusteringAlgorithmEnum
import org.clustering4ever.enums.ClusteringAlgorithmEnum._
import org.clustering4ever.scala.clustering.kcenters.KCenters
import org.clustering4ever.clustering.{ClusteringChaining, ClusteringAlgorithmCz}
/**
 *
 */
class ClusteringChainingLocal[
    ID,
    O,
    V <: GVector[V],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    GS[X] <: GenSeq[X]
](val data: GS[Cz[ID, O, V]]) extends ClusteringChaining[ID, O, V, Cz, GS, ClusteringChainingLocal[ID, O, _, Cz, GS]] {
    /**
     *
     */
    def runAlgorithms(algorithms: (ClusteringAlgorithmCz[V, GS, ClusteringArgs, ClusteringModelCz[V, GS]])*)(implicit ct: ClassTag[Cz[ID, O, V]]) = {
        algorithms.par.foreach( algo => runAlgorithm(algo) )
    }
    /**
     *
     */
    def newVectorization[NV <: GVector[NV]](vectorizationID: Int, towardNewVector: O => NV): ClusteringChainingLocal[ID, O, NV, Cz, GS] = {
        new ClusteringChainingLocal(data.map(_.addVectorized(vectorizationID, towardNewVector)).asInstanceOf[GS[Cz[ID, O, NV]]])
    }
    /**
     *
     */
    def updtV[NV <: GVector[NV]](vectorizationID: Int)(implicit vMapping: VMapping[Int, NV] = new VMapping[Int, NV]): ClusteringChainingLocal[ID, O, NV, Cz, GS] = {
        new ClusteringChainingLocal(data.map(_.updtV(vectorizationID)).asInstanceOf[GS[Cz[ID, O, NV]]])
    }

}

