package org.clustering4ever.spark.clustering.chaining
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{mutable, immutable}
import org.apache.spark.rdd.RDD
import org.clustering4ever.clustering.{ClusteringArgs, ClusteringModelCz}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector}
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping}
import org.clustering4ever.extensibleAlgorithmNature._
import org.clustering4ever.scala.clustering.kcenters.KCenters
import shapeless.HMap
import org.clustering4ever.clustering.{ClusteringChaining, ClusteringAlgorithmCz, DistributedClusteringAlgorithm}
import org.clustering4ever.vectorizations.{EmployedVectorization, DefaultWorkingVector, IthVectorization}
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
    val chainableID: Int = scala.util.Random.nextInt,
    val currentVectorization: EmployedVectorization = new DefaultWorkingVector,
    val vectorizations: HMap[VectorizationMapping] = HMap.empty[VectorizationMapping],
    val clusteringInfo: immutable.Vector[((Int, Int), EmployedVectorization, ClusteringArgs, ClusteringModelCz[_, RDD])] = immutable.Vector.empty[((Int, Int), EmployedVectorization, ClusteringArgs, ClusteringModelCz[_, RDD])]
)(implicit ct: ClassTag[Cz[ID, O, V]]) extends ClusteringChaining[ID, O, V, Cz, RDD, DistributedClusteringAlgorithm[V, ClusteringArgs, ClusteringModelCz[V, RDD]], ClusteringChainingDistributed[ID, O, V, Cz], ClusteringChainingDistributed[ID, O, _, Cz]] {
    /**
     *
     */
    val initialVectorNatureMapping = new VMapping[Int, V]
    /**
     *
     */
    protected[ClusteringChainingDistributed] def fusionChainable(ccl: ClusteringChainingDistributed[ID, O, V, Cz]): ClusteringChainingDistributed[ID, O, V, Cz] = {

        val newFusionSecurity = fusionChainableSecurity + ccl.fusionChainableSecurity
        val updatedRunNumber = scala.math.max(globalClusteringRunNumber, ccl.globalClusteringRunNumber)

        new ClusteringChainingDistributed(
            data.zip(ccl.data).map{ case (cz1, cz2) => cz1.addClusterIDs(cz2.clusterIDs.takeRight(ccl.fusionChainableSecurity):_*) },
            chainableID,
            currentVectorization,
            vectorizations,
            clusteringInfo ++ ccl.clusteringInfo.takeRight(ccl.fusionChainableSecurity)
        ) { 
        	override protected[ClusteringChainingDistributed] val fusionChainableSecurity = newFusionSecurity
        	override val globalClusteringRunNumber = updatedRunNumber
        }    

    }
    /**
     *
     */
    def addAnyVectorizationNature[NV <: GVector[NV]](vectorizationNature: Int, vectorizationID: Int, towardNewVector: O => NV): ClusteringChainingDistributed[ID, O, V, Cz] = {

        implicit val vMapping = new VectorizationMapping[Int, Map[Int, IthVectorization[O, NV]]]

        val itIsEmpty = vectorizations.get(vectorizationNature)(vMapping)
        val added = new IthVectorization(vectorizationID, towardNewVector)
        val vectorizationsUpdated = vectorizations + (
            (
                vectorizationNature,
                if(itIsEmpty.isDefined) itIsEmpty.get + ((vectorizationID, added))
                else Map(vectorizationID -> added)
            )
        )

        val updatedRunNumber = globalClusteringRunNumber
        val updatedFusionChainableSecurity = fusionChainableSecurity

        new ClusteringChainingDistributed(
            data.map(_.addVectorized[NV](vectorizationID, towardNewVector)(new VMapping[Int, NV])),
            chainableID,
            currentVectorization,
            vectorizationsUpdated,
            clusteringInfo
        ) {
            override protected[ClusteringChainingDistributed] val fusionChainableSecurity = updatedFusionChainableSecurity
            override val globalClusteringRunNumber = updatedRunNumber
        }

    }
    /**
     * Run one algorithm defined by user
     */
    def runAlgorithm(algorithm: DistributedClusteringAlgorithm[V, ClusteringArgs, ClusteringModelCz[V, RDD]])(implicit ct: ClassTag[Cz[ID, O, V]]): ClusteringChainingDistributed[ID, O, V, Cz]
     = {
        val model = algorithm.run(data)
        val updatedRunNumber = globalClusteringRunNumber + 1
        val updatedClusteringInfo = clusteringInfo :+ (((updatedRunNumber, runNumberWhenMultiRuns), currentVectorization, algorithm.args, model))
        val clusterizedData = model.obtainClustering(data)
        new ClusteringChainingDistributed(clusterizedData, chainableID, currentVectorization, vectorizations, updatedClusteringInfo) {
        	override protected[ClusteringChainingDistributed] val fusionChainableSecurity = 1
        	override val globalClusteringRunNumber = updatedRunNumber
        }
    }
    /**
     * Run multiples algorithms defined by user
     */
    def runAlgorithms(algorithms: DistributedClusteringAlgorithm[V, ClusteringArgs, ClusteringModelCz[V, RDD]]*)(implicit ct: ClassTag[Cz[ID, O, V]]): ClusteringChainingDistributed[ID, O, V, Cz] = {

        val updatedFusionChainableSecurity = fusionChainableSecurity

        algorithms.par.zipWithIndex.map{ case (alg, algIdx) =>

        	val updatedRunNumber = globalClusteringRunNumber + algIdx

            (new ClusteringChainingDistributed(data, chainableID, currentVectorization, vectorizations, clusteringInfo) { 
                override protected[ClusteringChainingDistributed] val fusionChainableSecurity = updatedFusionChainableSecurity
            	override protected[ClusteringChainingDistributed] val runNumberWhenMultiRuns = algIdx + 1 
        		override val globalClusteringRunNumber = updatedRunNumber         	
         	}).runAlgorithm(alg)
        }.reduce(_.fusionChainable(_))

    }
    /**
     * Update the current vector for another
     */
    def updtV[NV <: GVector[NV]](vectorizationNature: Int, vectorizationID: Int, vMapping: VMapping[Int, NV] = new VMapping[Int, NV])(implicit ct: ClassTag[Cz[ID, O, NV]]): ClusteringChainingDistributed[ID, O, NV, Cz] = {
        val explicitVectorization = new VectorizationMapping[Int, Map[Int, IthVectorization[O, NV]]]
        val newCurrentVectorization = vectorizations.get[Int, Map[Int, IthVectorization[O, NV]]](vectorizationNature)(explicitVectorization).get.get(vectorizationID).get
        val updatedVectorData = data.map(_.updtV[NV](vectorizationID)(vMapping))
        val updatedRunNumber = globalClusteringRunNumber
        val updatedFusionChainableSecurity = fusionChainableSecurity
        new ClusteringChainingDistributed(updatedVectorData, chainableID, newCurrentVectorization, vectorizations, clusteringInfo) {
            override protected[ClusteringChainingDistributed] val fusionChainableSecurity = updatedFusionChainableSecurity
        	override val globalClusteringRunNumber = updatedRunNumber
        }
    }
    /**
     *
     */
    private def applyDifferentVectorizationsOfSameNature[NV <: GVector[NV]](
    	vectorizationNature: Int,
        vectorizationIDs: Seq[Int],
        algorithms: Seq[DistributedClusteringAlgorithm[NV, ClusteringArgs, ClusteringModelCz[NV, RDD]]],
        otherVectorizationMapping: VMapping[Int, NV]
    )(implicit ct: ClassTag[Cz[ID, O, NV]]): ClusteringChainingDistributed[ID, O, NV, Cz] = {
        vectorizationIDs.par.map( vectorizationID => updtV[NV](vectorizationNature, vectorizationID, otherVectorizationMapping)(ct).runAlgorithms(algorithms:_*) ).reduce(_.fusionChainable(_))
    }
    /**
     *
     */
    def runAlgorithmsOnVectorizationEqualToDefaultVectorNature(vectorizationIDs: Seq[Int], algorithms: DistributedClusteringAlgorithm[V, ClusteringArgs, ClusteringModelCz[V, RDD]]*)(implicit ct: ClassTag[Cz[ID, O, V]]): ClusteringChainingDistributed[ID, O, V, Cz] = {
        applyDifferentVectorizationsOfSameNature(0, vectorizationIDs, algorithms, initialVectorNatureMapping)
    }
    /**
     *
     */
    def runAlgorithmsOnOthersVectorizationNature[NV <: GVector[NV]](vectorizationNature: Int, vectorizationIDs: Seq[Int], algorithms: DistributedClusteringAlgorithm[NV, ClusteringArgs, ClusteringModelCz[NV, RDD]]*)(implicit ct: ClassTag[Cz[ID, O, NV]]): ClusteringChainingDistributed[ID, O, NV, Cz] = {
        applyDifferentVectorizationsOfSameNature(vectorizationNature, vectorizationIDs, algorithms, new VMapping[Int, NV])
    }

}
/**
 *
 */
object ClusteringChainingDistributed {

    def apply[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: RDD[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]) = new ClusteringChainingDistributed(data)

}