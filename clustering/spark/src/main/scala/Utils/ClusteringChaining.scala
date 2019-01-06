package org.clustering4ever.spark.clustering.chaining
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{mutable, immutable}
import org.apache.spark.rdd.RDD
import shapeless._
import org.clustering4ever.clustering.{ClusteringArgs, ClusteringModelCz}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector}
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping}
import org.clustering4ever.enums.ClusteringAlgorithmEnum
import org.clustering4ever.enums.ClusteringAlgorithmEnum._
import org.clustering4ever.scala.clustering.kcenters.KCenters
import shapeless.HMap
import org.clustering4ever.clustering.{ClusteringChaining, ClusteringAlgorithmCz, DistributedClusteringAlgorithm, EmployedVectorization, IthVectorization, DefaultWorkingVector}
/**
 *
 */
class ClusteringChainingDistributed[
    ID,
    O,
    V <: GVector[V],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]
](
    val data: RDD[Cz[ID, O, V]],
    val currentVectorization: EmployedVectorization = new DefaultWorkingVector,
    val vectorizations: HMap[VectorizationMapping] = HMap.empty[VectorizationMapping],
    val clusteringInfo: immutable.Vector[((Int, Int), EmployedVectorization, ClusteringArgs, ClusteringModelCz[_, RDD])] = immutable.Vector.empty[((Int, Int), EmployedVectorization, ClusteringArgs, ClusteringModelCz[_, RDD])],
    val globalClusteringRunNumber: Int = 0
)(implicit ct: ClassTag[Cz[ID, O, V]]) extends ClusteringChaining[ID, O, V, Cz, RDD, DistributedClusteringAlgorithm[V, ClusteringArgs, ClusteringModelCz[V, RDD]], ClusteringChainingDistributed[ID, O, V, Cz], ClusteringChainingDistributed[ID, O, _, Cz]] {
    /**
     *
     */
    implicit val initialVectorNatureMapping = new VMapping[Int, V]
    /**
     *
     */
    def fusionChainable(ccl: ClusteringChainingDistributed[ID, O, V, Cz]): ClusteringChainingDistributed[ID, O, V, Cz] = {

        require(currentVectorization == ccl.currentVectorization && vectorizations == ccl.vectorizations)
        


        val (newData, newSecurityReduce) = if(securityReduce == -1 && ccl.securityReduce == -1) {
            (
                data.zip(ccl.data).map{ case (cz1, cz2) => cz1.addClusterIDs(cz2.clusterIDs:_*) },
                data.first.clusterIDs.size + ccl.data.first.clusterIDs.size
            )
        }
        else if(securityReduce == -1 && ccl.securityReduce != -1) {
            (
                data.zip(ccl.data).map{ case (cz1, cz2) => cz1.addClusterIDs(cz2.clusterIDs.takeRight(ccl.securityReduce):_*) },
                data.first.clusterIDs.size + ccl.securityReduce
            )
        }
        else if(securityReduce != -1 && ccl.securityReduce == -1) {
            (
                ccl.data.zip(data).map{ case (cz2, cz1) => cz2.addClusterIDs(cz1.clusterIDs.takeRight(securityReduce):_*) },
                ccl.data.first.clusterIDs.size + securityReduce
            )
        }
        else {
            (
                data.zip(ccl.data).map{ case (cz1, cz2) => cz1.addClusterIDs(cz2.clusterIDs.takeRight(ccl.securityReduce):_*) },
                securityReduce + ccl.securityReduce
            )
        }

        new ClusteringChainingDistributed(
            newData.asInstanceOf[RDD[Cz[ID, O, V]]],
            currentVectorization,
            vectorizations,
            clusteringInfo ++ ccl.clusteringInfo,
            scala.math.max(globalClusteringRunNumber, ccl.globalClusteringRunNumber)
        ) { override val securityReduce = newSecurityReduce }
    }
    /**
     *
     */
    def newVectorization[NV <: GVector[NV]](vectorizationID: Int, towardNewVector: O => NV)(implicit vMapping: VectorizationMapping[Int, IthVectorization[O, NV]] = new VectorizationMapping[Int, IthVectorization[O, NV]]): ClusteringChainingDistributed[ID, O, V, Cz] = {
        
        val vectorizationsUpdated = vectorizations + ((vectorizationID, new IthVectorization(vectorizationID, towardNewVector)))

        new ClusteringChainingDistributed(
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
    def runAlgorithm(algorithm: DistributedClusteringAlgorithm[V, ClusteringArgs, ClusteringModelCz[V, RDD]], specificRunID: Int = -1)(implicit ct: ClassTag[Cz[ID, O, V]]): ClusteringChainingDistributed[ID, O, V, Cz]
     = {
        val model = algorithm.run(data)
        val updatedRunNumber = globalClusteringRunNumber + 1
        val updatedClusteringInfo = clusteringInfo :+ (((updatedRunNumber, specificRunID), currentVectorization, algorithm.args, model))
        val clusterizedData = model.obtainClustering(data)
        new ClusteringChainingDistributed(clusterizedData, currentVectorization, vectorizations, updatedClusteringInfo, updatedRunNumber) { override val securityReduce = 1 }
    }
    /**
     * Run multiples algorithms defined by user
     */
    def runAlgorithms(algorithms: DistributedClusteringAlgorithm[V, ClusteringArgs, ClusteringModelCz[V, RDD]]*)(implicit ct: ClassTag[Cz[ID, O, V]]): ClusteringChainingDistributed[ID, O, V, Cz] = {

        algorithms.par.zipWithIndex.map{ case (alg, algIdx) =>
            new ClusteringChainingDistributed(data, currentVectorization, vectorizations, clusteringInfo, globalClusteringRunNumber + algIdx).runAlgorithm(alg, algIdx + 1)
        }.reduce(_.fusionChainable(_))

    }
    /**
     * Update the current vector for another
     */
    def updtV[NV <: GVector[NV]](vectorizationID: Int, vMapping: VMapping[Int, NV] = new VMapping[Int, NV])(implicit ct: ClassTag[Cz[ID, O, NV]]): ClusteringChainingDistributed[ID, O, NV, Cz] = {
        val explicitVectorization = new VectorizationMapping[Int, IthVectorization[O, NV]]
        val newCurrentVectorization = vectorizations.get[Int, IthVectorization[O, NV]](vectorizationID)(explicitVectorization).get
        val updatedVectorData = data.map(_.updtV[NV](vectorizationID)(vMapping))
        new ClusteringChainingDistributed(updatedVectorData, newCurrentVectorization, vectorizations, clusteringInfo, globalClusteringRunNumber)
    }
    /**
     *
     */
    private def applyDifferentVectorizationsOfSameNature[NV <: GVector[NV]](
        vectorizationIDs: Seq[Int],
        otherVectorizationMapping: VMapping[Int, NV],
        algorithms: DistributedClusteringAlgorithm[NV, ClusteringArgs, ClusteringModelCz[NV, RDD]]*
    )(implicit ct: ClassTag[Cz[ID, O, NV]]): ClusteringChainingDistributed[ID, O, NV, Cz] = {

        vectorizationIDs.par.map( vectorizationID => updtV[NV](vectorizationID, otherVectorizationMapping)(ct).runAlgorithms(algorithms:_*) ).reduce(_.fusionChainable(_))
    
    }
    /**
     *
     */
    def runAlgorithmsOnVectorizationEqualToDefaultVectorNature(vectorizationIDs: Seq[Int], algorithms: DistributedClusteringAlgorithm[V, ClusteringArgs, ClusteringModelCz[V, RDD]]*)(implicit ct: ClassTag[Cz[ID, O, V]]): ClusteringChainingDistributed[ID, O, V, Cz] = {

        applyDifferentVectorizationsOfSameNature(vectorizationIDs, initialVectorNatureMapping, algorithms:_*)
   
    }
    /**
     *
     */
    def runAlgorithmsOnOthersVectorizationNature[NV <: GVector[NV]](vectorizationIDs: Seq[Int], algorithms: DistributedClusteringAlgorithm[NV, ClusteringArgs, ClusteringModelCz[NV, RDD]]*)(implicit ct: ClassTag[Cz[ID, O, NV]]): ClusteringChainingDistributed[ID, O, NV, Cz] = {

        applyDifferentVectorizationsOfSameNature(vectorizationIDs, new VMapping[Int, NV], algorithms:_*)

    }

}