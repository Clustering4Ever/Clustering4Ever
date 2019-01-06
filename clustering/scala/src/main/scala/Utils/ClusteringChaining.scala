package org.clustering4ever.scala.clustering.chaining
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{GenSeq, mutable, immutable}
import shapeless._
import org.clustering4ever.clustering.{ClusteringArgs, ClusteringModelCz}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector}
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping}
import org.clustering4ever.enums.ClusteringAlgorithmEnum
import org.clustering4ever.enums.ClusteringAlgorithmEnum._
import org.clustering4ever.scala.clustering.kcenters.KCenters
import shapeless.HMap
import org.clustering4ever.clustering.{ClusteringChaining, ClusteringAlgorithmCz, LocalClusteringAlgorithm, EmployedVectorization, IthVectorization, DefaultWorkingVector}
/**
 *
 */
class ClusteringChainingLocal[
    ID,
    O,
    V <: GVector[V],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    GS[X] <: GenSeq[X]
](
    val data: GS[Cz[ID, O, V]],
    val currentVectorization: EmployedVectorization = new DefaultWorkingVector,
    val vectorizations: HMap[VectorizationMapping] = HMap.empty[VectorizationMapping],
    val clusteringInfo: immutable.Vector[((Int, Int), EmployedVectorization, ClusteringArgs, ClusteringModelCz[_, GS])] = immutable.Vector.empty[((Int, Int), EmployedVectorization, ClusteringArgs, ClusteringModelCz[_, GS])],
    val globalClusteringRunNumber: Int = 0
) extends ClusteringChaining[ID, O, V, Cz, GS, LocalClusteringAlgorithm[V, GS, ClusteringArgs, ClusteringModelCz[V, GS]], ClusteringChainingLocal[ID, O, V, Cz, GS], ClusteringChainingLocal[ID, O, _, Cz, GS]] {
    /**
     *
     */
    implicit val initialVectorNatureMapping = new VMapping[Int, V]
    /**
     *
     */
    def fusionChainable(ccl: ClusteringChainingLocal[ID, O, V, Cz, GS]): ClusteringChainingLocal[ID, O, V, Cz, GS] = {

        require(currentVectorization == ccl.currentVectorization && vectorizations == ccl.vectorizations)
        
        def ltr(sr: Int) = {
            (            
                clusteringInfo ++ ccl.clusteringInfo.takeRight(sr),
                data.zip(ccl.data).map{ case (cz1, cz2) => cz1.addClusterIDs(cz2.clusterIDs.takeRight(sr):_*) }
            )
        }

        val (newData, newClusteringInfo, newSecurityReduce) = if(securityReduce == -1 && ccl.securityReduce == -1) {
            (
                data.zip(ccl.data).map{ case (cz1, cz2) => cz1.addClusterIDs(cz2.clusterIDs:_*) },
                clusteringInfo ++ ccl.clusteringInfo,
                data.head.clusterIDs.size + ccl.data.head.clusterIDs.size
            )
        }
        else if(securityReduce == -1 && ccl.securityReduce != -1) {
            val (newSec, ndata) = ltr(ccl.securityReduce)
            (
                ndata,
                newSec,
                data.head.clusterIDs.size + ccl.securityReduce
            )
        }
        else if(securityReduce != -1 && ccl.securityReduce == -1) {
            (
                ccl.data.zip(data).map{ case (cz2, cz1) => cz2.addClusterIDs(cz1.clusterIDs.takeRight(securityReduce):_*) },
                ccl.clusteringInfo ++ clusteringInfo.takeRight(securityReduce),
                ccl.data.head.clusterIDs.size + securityReduce
            )
        }
        else {
            val (newSec, ndata) = ltr(ccl.securityReduce)
            (
                ndata,
                newSec,
                securityReduce + ccl.securityReduce
            )
        }

        new ClusteringChainingLocal(
            newData.asInstanceOf[GS[Cz[ID, O, V]]],
            currentVectorization,
            vectorizations,
            newClusteringInfo,
            scala.math.max(globalClusteringRunNumber, ccl.globalClusteringRunNumber)
        ) { override val securityReduce = newSecurityReduce }
    }
    /**
     *
     */
    def newVectorization[NV <: GVector[NV]](vectorizationID: Int, towardNewVector: O => NV)(implicit vMapping: VectorizationMapping[Int, IthVectorization[O, NV]] = new VectorizationMapping[Int, IthVectorization[O, NV]]): ClusteringChainingLocal[ID, O, V, Cz, GS] = {
        
        val vectorizationsUpdated = vectorizations + ((vectorizationID, new IthVectorization(vectorizationID, towardNewVector)))

        new ClusteringChainingLocal(
            data,
            currentVectorization,
            vectorizationsUpdated,
            clusteringInfo,
            globalClusteringRunNumber
        )

    }
    /**
     * Run one algorithm defined by user
     */
    def runAlgorithm(algorithm: LocalClusteringAlgorithm[V, GS, ClusteringArgs, ClusteringModelCz[V, GS]], specificRunID: Int = -1)(implicit ct: ClassTag[Cz[ID, O, V]]): ClusteringChainingLocal[ID, O, V, Cz, GS]
     = {
        val model = algorithm.run(data)
        val updatedRunNumber = globalClusteringRunNumber + 1
        val updatedClusteringInfo = clusteringInfo :+ (((updatedRunNumber, specificRunID), currentVectorization, algorithm.args, model))
        val clusterizedData = model.obtainClustering(data)
        new ClusteringChainingLocal(clusterizedData, currentVectorization, vectorizations, updatedClusteringInfo, updatedRunNumber) { override val securityReduce = 1 }
    }
    /**
     * Run multiples algorithms defined by user
     */
    def runAlgorithms(algorithms: LocalClusteringAlgorithm[V, GS, ClusteringArgs, ClusteringModelCz[V, GS]]*)(implicit ct: ClassTag[Cz[ID, O, V]]): ClusteringChainingLocal[ID, O, V, Cz, GS] = {

        algorithms.par.zipWithIndex.map{ case (alg, algIdx) =>
            new ClusteringChainingLocal(data, currentVectorization, vectorizations, clusteringInfo, globalClusteringRunNumber + algIdx).runAlgorithm(alg, algIdx + 1)
        }.reduce(_.fusionChainable(_))

    }
    /**
     * Update the current vector for another
     */
    def updtV[NV <: GVector[NV]](vectorizationID: Int, vMapping: VMapping[Int, NV] = new VMapping[Int, NV])(implicit ct: ClassTag[Cz[ID, O, NV]]): ClusteringChainingLocal[ID, O, NV, Cz, GS] = {
        val explicitVectorization = new VectorizationMapping[Int, IthVectorization[O, NV]]
        val newCurrentVectorization = vectorizations.get[Int, IthVectorization[O, NV]](vectorizationID)(explicitVectorization).get
        val updatedVectorData = data.map(_.updtV[NV](vectorizationID)(vMapping)).asInstanceOf[GS[Cz[ID, O, NV]]]
        new ClusteringChainingLocal(updatedVectorData, newCurrentVectorization, vectorizations, clusteringInfo, globalClusteringRunNumber)
    }
    /**
     *
     */
    private def applyDifferentVectorizationsOfSameNature[NV <: GVector[NV]](
        vectorizationIDs: Seq[Int],
        otherVectorizationMapping: VMapping[Int, NV],
        algorithms: LocalClusteringAlgorithm[NV, GS, ClusteringArgs, ClusteringModelCz[NV, GS]]*
    )(implicit ct: ClassTag[Cz[ID, O, NV]]): ClusteringChainingLocal[ID, O, NV, Cz, GS] = {

        vectorizationIDs.par.map( vectorizationID => updtV[NV](vectorizationID, otherVectorizationMapping)(ct).runAlgorithms(algorithms:_*) ).reduce(_.fusionChainable(_))
    
    }
    /**
     *
     */
    def runAlgorithmsOnVectorizationEqualToDefaultVectorNature(vectorizationIDs: Seq[Int], algorithms: LocalClusteringAlgorithm[V, GS, ClusteringArgs, ClusteringModelCz[V, GS]]*)(implicit ct: ClassTag[Cz[ID, O, V]]): ClusteringChainingLocal[ID, O, V, Cz, GS] = {

        applyDifferentVectorizationsOfSameNature(vectorizationIDs, initialVectorNatureMapping, algorithms:_*)
   
    }
    /**
     *
     */
    def runAlgorithmsOnOthersVectorizationNature[NV <: GVector[NV]](vectorizationIDs: Seq[Int], algorithms: LocalClusteringAlgorithm[NV, GS, ClusteringArgs, ClusteringModelCz[NV, GS]]*)(implicit ct: ClassTag[Cz[ID, O, NV]]): ClusteringChainingLocal[ID, O, NV, Cz, GS] = {

        applyDifferentVectorizationsOfSameNature(vectorizationIDs, new VMapping[Int, NV], algorithms:_*)

    }

}