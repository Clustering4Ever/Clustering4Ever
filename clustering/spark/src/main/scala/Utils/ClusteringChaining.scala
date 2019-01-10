package org.clustering4ever.clustering.chaining
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import shapeless.HMap
import org.apache.spark.SparkContext
import scala.collection.{mutable, immutable, GenSeq}
import scala.collection.parallel.immutable.ParSeq
import org.apache.spark.rdd.RDD
import org.clustering4ever.clustering.{ClusteringArgs, ClusteringModelCz}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector}
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping}
import org.clustering4ever.extensibleAlgorithmNature._
import org.clustering4ever.scala.clustering.kcenters.KCenters
import org.clustering4ever.clustering.{ClusteringChaining, ClusteringAlgorithmCz, DistributedClusteringAlgorithm, LocalClusteringAlgorithm, DataExplorator, AlgorithmsRestrictions}
import org.clustering4ever.vectorizations.{EmployedVectorization, DefaultWorkingVector, IthVectorization}
import org.clustering4ever.types.ClusteringInformationTypes._
import org.clustering4ever.types.VectorizationIDTypes._
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
    val clusteringInfo: immutable.Vector[(GlobalClusteringRunNumber, EmployedVectorization, ClusteringArgs, ClusteringModelCz[_, RDD])] = immutable.Vector.empty[(GlobalClusteringRunNumber, EmployedVectorization, ClusteringArgs, ClusteringModelCz[_, RDD])]
)(implicit ct: ClassTag[Cz[ID, O, V]]) extends ClusteringChaining[ID, O, V, Cz, RDD] {
    /**
     * A securty value in order to allow proper reduce of Chaining models
     */
    protected[chaining] val fusionChainableSecurity: Int = 0
    /**
     *
     */
    type Self[NV <: GVector[NV]] = ClusteringChainingDistributed[ID, O, NV, Cz]
    /**
     *
     */
    type AlgorithmsRestrictions[NV <: GVector[NV]] = DistributedClusteringAlgorithm[NV, ClusteringArgs, ClusteringModelCz[NV, RDD]]
    /**
     *
     */
    protected[chaining] def fusionChainable(ccl: ClusteringChainingDistributed[ID, O, V, Cz]): ClusteringChainingDistributed[ID, O, V, Cz] = {

        val newFusionSecurity = fusionChainableSecurity + ccl.fusionChainableSecurity
        val updatedRunNumber = scala.math.max(globalClusteringRunNumber, ccl.globalClusteringRunNumber)

        new ClusteringChainingDistributed(
            data.zip(ccl.data).map{ case (cz1, cz2) => cz1.addClusterIDs(cz2.clusterIDs.takeRight(ccl.fusionChainableSecurity):_*) },
            chainableID,
            currentVectorization,
            vectorizations,
            clusteringInfo ++ ccl.clusteringInfo.takeRight(ccl.fusionChainableSecurity)
        ) { 
        	override protected[chaining] val fusionChainableSecurity = newFusionSecurity
        	override val globalClusteringRunNumber = updatedRunNumber
        }    

    }
    /**
     *
     */
    def addAnyVectorizationNature[NV <: GVector[NV]](vectorizationNature: VectorizationNature, vectorizationID: VectorizationID, towardNewVector: O => NV): (ClusteringChainingDistributed[ID, O, V, Cz], VectorizationMapping[VectorizationNature, Map[VectorizationID, IthVectorization[O, NV]]]) = {

        implicit val vMapping = new VectorizationMapping[VectorizationNature, Map[VectorizationID, IthVectorization[O, NV]]]

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
        (
            new ClusteringChainingDistributed(
                data.map(_.addVectorized[NV](vectorizationID, towardNewVector)(new VMapping[Int, NV])),
                chainableID,
                currentVectorization,
                vectorizationsUpdated,
                clusteringInfo
            ) {
                override protected[chaining] val fusionChainableSecurity = updatedFusionChainableSecurity
                override val globalClusteringRunNumber = updatedRunNumber
            },
            vMapping
        )

    }
    /**
     * Run one algorithm defined by user
     */
    def runAlgorithm(algorithm: DistributedClusteringAlgorithm[V, ClusteringArgs, ClusteringModelCz[V, RDD]])(implicit ct: ClassTag[Cz[ID, O, V]]): ClusteringChainingDistributed[ID, O, V, Cz]
     = {
        val model = algorithm.run(data)
        val updatedRunNumber = globalClusteringRunNumber + 1
        val updatedClusteringInfo = clusteringInfo :+ ((updatedRunNumber, currentVectorization, algorithm.args, model))
        val clusterizedData = model.obtainClustering(data)
        new ClusteringChainingDistributed(clusterizedData, chainableID, currentVectorization, vectorizations, updatedClusteringInfo) {
        	override protected[chaining] val fusionChainableSecurity = 1
        	override val globalClusteringRunNumber = updatedRunNumber
        }
    }
    /**
     * Run multiples algorithms defined by user
     */
    def runAlgorithms(algorithms: DistributedClusteringAlgorithm[V, ClusteringArgs, ClusteringModelCz[V, RDD]]*)(implicit ct: ClassTag[Cz[ID, O, V]]): ClusteringChainingDistributed[ID, O, V, Cz] = {

        val updatedFusionChainableSecurity = fusionChainableSecurity

        algorithms.zipWithIndex.map{ case (alg, algIdx) =>

        	val updatedRunNumber = globalClusteringRunNumber + algIdx

            (new ClusteringChainingDistributed(data, chainableID, currentVectorization, vectorizations, clusteringInfo) { 
                override protected[chaining] val fusionChainableSecurity = updatedFusionChainableSecurity
        		override val globalClusteringRunNumber = updatedRunNumber         	
         	}).runAlgorithm(alg)
        }.reduce(_.fusionChainable(_))

    }
    /**
     * Update the current vector for another
     */
    def updtV[NV <: GVector[NV]](vectorizationNature: VectorizationNature, vectorizationID: VectorizationID, vMapping: VMapping[Int, NV] = new VMapping[Int, NV])(implicit ct: ClassTag[Cz[ID, O, NV]]): ClusteringChainingDistributed[ID, O, NV, Cz] = {
        val explicitVectorization = new VectorizationMapping[VectorizationNature, Map[VectorizationID, IthVectorization[O, NV]]]
        val newCurrentVectorization = vectorizations.get[Int, Map[Int, IthVectorization[O, NV]]](vectorizationNature)(explicitVectorization).get.get(vectorizationID).get
        val updatedVectorData = data.map(_.updtV[NV](vectorizationID)(vMapping))
        val updatedRunNumber = globalClusteringRunNumber
        val updatedFusionChainableSecurity = fusionChainableSecurity
        new ClusteringChainingDistributed(updatedVectorData, chainableID, newCurrentVectorization, vectorizations, clusteringInfo) {
            override protected[chaining] val fusionChainableSecurity = updatedFusionChainableSecurity
        	override val globalClusteringRunNumber = updatedRunNumber
        }
    }
    /**
     *
     */
    protected def applyDifferentVectorizationsOfSameNature[NV <: GVector[NV]](
    	vectorizationNature: VectorizationNature,
        vectorizationIDs: Seq[Int],
        algorithms: Seq[DistributedClusteringAlgorithm[NV, ClusteringArgs, ClusteringModelCz[NV, RDD]]],
        otherVectorizationMapping: VMapping[Int, NV] = new VMapping[Int, NV]
    )(implicit ct: ClassTag[Cz[ID, O, NV]]): ClusteringChainingDistributed[ID, O, NV, Cz] = {
        vectorizationIDs.par.map( vectorizationID => updtV[NV](vectorizationNature, vectorizationID, otherVectorizationMapping)(ct).runAlgorithms(algorithms:_*) ).reduce(_.fusionChainable(_))
    }
}
/**
 *
 */
object ClusteringChainingDistributed {

    def apply[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: RDD[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]) = new ClusteringChainingDistributed(data)

}

/**
 *
 */
class ClusteringChainingLocalOnDistributedSystem[
    ID,
    O,
    V <: GVector[V],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]
](
    @transient val sc: SparkContext,
    val data: List[Cz[ID, O, V]],
    val chainableID: Int = scala.util.Random.nextInt,
    val currentVectorization: EmployedVectorization = new DefaultWorkingVector,
    val vectorizations: HMap[VectorizationMapping] = HMap.empty[VectorizationMapping],
    val clusteringInfo: immutable.Vector[((Int, Int), EmployedVectorization, ClusteringArgs, ClusteringModelCz[_, ParSeq])] = immutable.Vector.empty[((Int, Int), EmployedVectorization, ClusteringArgs, ClusteringModelCz[_, ParSeq])]
)(implicit ct: ClassTag[Cz[ID, O, V]], ct2: ClassTag[List[Cz[ID, O, V]]]) extends DataExplorator[ID, O, V, Cz, List] with AlgorithmsRestrictions[V, ParSeq] {
    /**
     * A securty value in order to allow proper reduce of Chaining models
     */
    protected val fusionChainableSecurity: Int = 0
    /**
     *
     */
    val globalClusteringRunNumber: Int = 0
    /**
     *
     */
    type Self[NV <: GVector[NV]] = ClusteringChainingLocalOnDistributedSystem[ID, O, NV, Cz]
    /**
     *
     */
    type AlgorithmsRestrictions[NV <: GVector[NV]] = LocalClusteringAlgorithm[NV, ParSeq, ClusteringArgs, ClusteringModelCz[NV, ParSeq]]
    /**
     *
     */
    // def runAlgorithmsInParallel(defaultParallelism: Int, algorithms: AlgorithmsRestrictions[V]*): Self[V] = {

    //     val dataBrodcasted = sc.broadcast(data)

    //     val clusteringChainingLocalFinal@ClusteringChainingLocal(dataFinal, chainableIDFinal, currentVectorizationFinal, vectorizationsFinal, clusteringInfoFinal) = sc.parallelize(algorithms.zipWithIndex, defaultParallelism).map{ case (algo, algIdx) =>
    //         val updatedFusionChainableSecurity = fusionChainableSecurity
    //         val updatedRunNumber = globalClusteringRunNumber + algIdx
    //         (new ClusteringChainingLocal(dataBrodcasted.value.par, chainableID, currentVectorization, vectorizations, clusteringInfo) {
    //             override protected[chaining] val fusionChainableSecurity = updatedFusionChainableSecurity
    //             override protected[chaining] val runNumberWhenMultiRuns = algIdx + 1 
    //             override val globalClusteringRunNumber = updatedRunNumber
    //         }).runAlgorithm(algo)
    //     }.reduce(_.fusionChainable(_))

    //     new ClusteringChainingLocalOnDistributedSystem(sc, dataFinal., chainableIDFinal, currentVectorizationFinal, vectorizationsFinal, clusteringInfoFinal) {
    //         override protected[chaining] val fusionChainableSecurity = clusteringChainingLocalFinal.fusionChainableSecurity
    //         override val globalClusteringRunNumber = clusteringChainingLocalFinal.globalClusteringRunNumber
    //     }
    // }
}