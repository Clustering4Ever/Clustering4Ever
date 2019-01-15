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
import org.clustering4ever.clustering.{ClusteringArgsLocal, ClusteringArgsDistributed, ClusteringModelLocal, ClusteringModelDistributed}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector}
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping}
import org.clustering4ever.extensibleAlgorithmNature._
import org.clustering4ever.clustering.{ClusteringChaining, ClusteringAlgorithmCz, ClusteringInformationsLocal, ClusteringAlgorithmDistributed, ClusteringInformationsDistributed, ClusteringAlgorithmLocal, DataExplorator, AlgorithmsRestrictions}
import org.clustering4ever.vectorizations.{Vectorization, EasyVectorization}
import org.clustering4ever.types.ClusteringInformationTypes._
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.vectorizations.{VectorizationNature, Default}
/**
 * This classe intend to run many distributed algorithms in Spark sequentially through a distributed system for large size datasets
 */
class ClusteringChainingDistributed[
    ID,
    O,
    V <: GVector[V],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]
](
    val data: RDD[Cz[ID, O, V]],
    val chainableID: Int = scala.util.Random.nextInt,
    val currentVectorization: Vectorization[O, V] = EasyVectorization[O, V](0, None),
    val clusteringInfo: ClusteringInformationsDistributed[ID, O, Cz] = new ClusteringInformationsDistributed[ID, O, Cz]
)(implicit val ct: ClassTag[Cz[ID, O, V]]) extends ClusteringChaining[ID, O, V, Cz, RDD] {
    /**
     * HMap of original and added Vectorization
     */
    val vectorizations: HMap[VectorizationMapping] = HMap[VectorizationMapping](currentVectorization.vectorizationID -> currentVectorization)
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
    type AlgorithmsRestrictions[NV <: GVector[NV]] = ClusteringAlgorithmDistributed[ID, O, NV, Cz, ClusteringArgsDistributed[NV], ClusteringModelDistributed[ID, O, NV, Cz, ClusteringArgsDistributed[NV]]]
    /**
     *
     */
    protected[chaining] def fusionChainable(ccl: ClusteringChainingDistributed[ID, O, V, Cz]): ClusteringChainingDistributed[ID, O, V, Cz] = {

        val newFusionSecurity = fusionChainableSecurity + ccl.fusionChainableSecurity
        val updatedRunNumber = scala.math.max(globalClusteringRunNumber, ccl.globalClusteringRunNumber)
        val aVecto = vectorizations.get(currentVectorization.vectorizationID).get
        val bVecto = ccl.vectorizations.get(currentVectorization.vectorizationID).get
        val updatedCurrentVectorization = currentVectorization.updateClustering(aVecto.clusteringNumbers.toSeq ++ bVecto.clusteringNumbers:_*)
        val updatedVectorizations = vectorizations + ((updatedCurrentVectorization.vectorizationID, updatedCurrentVectorization))


        new ClusteringChainingDistributed(
            data.zip(ccl.data).map{ case (cz1, cz2) => cz1.addClusterIDs(cz2.clusterIDs.takeRight(ccl.fusionChainableSecurity):_*) },
            chainableID,
            currentVectorization,
            clusteringInfo.copy(clusteringInformations = clusteringInfo.clusteringInformations ++ ccl.clusteringInfo.clusteringInformations.takeRight(ccl.fusionChainableSecurity))
        ) { 
        	override protected[chaining] val fusionChainableSecurity = newFusionSecurity
        	override val globalClusteringRunNumber = updatedRunNumber
            override val vectorizations = updatedVectorizations
        }    

    }
    /**
     * Run one algorithm defined by user
     */
    def runAlgorithm(algorithm: AlgorithmsRestrictions[V]): ClusteringChainingDistributed[ID, O, V, Cz]
     = {

        val model = algorithm.run(data)
        val updatedRunNumber = globalClusteringRunNumber + 1
        val updatedClusteringInfo = clusteringInfo.copy(clusteringInformations = clusteringInfo.clusteringInformations :+ ((updatedRunNumber, currentVectorization, algorithm.args, model)))
        val updatedData = model.obtainClustering(data)
        val updatedCurrentVectorization = currentVectorization.updateClustering(updatedRunNumber)
        val updatedVectorizations = vectorizations + ((updatedCurrentVectorization.vectorizationID, updatedCurrentVectorization))
        new ClusteringChainingDistributed(updatedData, chainableID, currentVectorization, updatedClusteringInfo) {
            override val vectorizations = updatedVectorizations
            override val globalClusteringRunNumber = updatedRunNumber
        	override protected[chaining] val fusionChainableSecurity = 1
        }
    }
    /**
     * Run multiples algorithms defined by user
     */
    def runAlgorithms(algorithms: AlgorithmsRestrictions[V]*): ClusteringChainingDistributed[ID, O, V, Cz] = {

        val updatedFusionChainableSecurity = fusionChainableSecurity

        algorithms.view.zipWithIndex.map{ case (alg, algIdx) =>

        	val updatedRunNumber = globalClusteringRunNumber + algIdx
            val updatedVectorizations = vectorizations

            (new ClusteringChainingDistributed(data, chainableID, currentVectorization, clusteringInfo) { 
                override val vectorizations = updatedVectorizations
                override val globalClusteringRunNumber = updatedRunNumber           
                override protected[chaining] val fusionChainableSecurity = updatedFusionChainableSecurity
         	}).runAlgorithm(alg)
        }.reduce(_.fusionChainable(_))

    }
    /**
     *
     */
    def addAnyVectorization[NV <: GVector[NV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B]](vectorization: Vecto[O, NV]): (ClusteringChainingDistributed[ID, O, V, Cz], VectorizationMapping[VectorizationID, Vecto[O, NV]]) = {

        implicit val vectoMapping = new VectorizationMapping[VectorizationID, Vecto[O, NV]]
        val updatedVectorizations = vectorizations + ((vectorization.vectorizationID, vectorization))
        val updatedRunNumber = globalClusteringRunNumber
        val updatedFusionChainableSecurity = fusionChainableSecurity

        (
            new ClusteringChainingDistributed(
                data.map(_.addVectorized(vectorization.vectorizationID, vectorization.vectorizationFct.get)),
                chainableID,
                currentVectorization,
                clusteringInfo
            ) {
                override val vectorizations = updatedVectorizations
                override val globalClusteringRunNumber = updatedRunNumber
                override protected[chaining] val fusionChainableSecurity = updatedFusionChainableSecurity
            },
            vectoMapping
        )

    }
    /**
     * Update the current vector for another
     */
    def updateAnyVector[NV <: GVector[NV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B]](vectorization: Vecto[O, NV])(implicit ct: ClassTag[Cz[ID, O, NV]]): ClusteringChainingDistributed[ID, O, NV, Cz] = {

        val updatedVectorData = data.map(_.updateVector[NV](vectorization.vectorizationID))
        val newCurrentVectorization = vectorizations.get(vectorization.vectorizationID)(new VectorizationMapping[VectorizationID, Vecto[O, NV]]).get
        val updatedGlobalRunNumber = globalClusteringRunNumber
        val fusionChainableSecurityCopy = fusionChainableSecurity
        val updatedVectorizations = vectorizations

        new ClusteringChainingDistributed(updatedVectorData, chainableID, newCurrentVectorization, clusteringInfo) {
            override val vectorizations = updatedVectorizations
            override protected[chaining] val fusionChainableSecurity = fusionChainableSecurityCopy
        	override val globalClusteringRunNumber = updatedGlobalRunNumber
        }
    }
    /**
     *
     */
    def runAlgorithmsOnMultipleVectorizations[Vecto[A, B <: GVector[B]] <: Vectorization[A, B]](
        vectorizations: Seq[Vecto[O, V]],
        algorithms: AlgorithmsRestrictions[V]*
    ): ClusteringChainingDistributed[ID, O, V, Cz] = {
        vectorizations.view.map( vectorization => updateVector(vectorization).runAlgorithms(algorithms:_*) ).reduce(_.fusionChainable(_))
    }
}
/**
 * This classe intend to run many algorithms parallely on a distributed system for medium size datasets
 */
class ClusteringChainingClusteredSystem[
    ID,
    O,
    V <: GVector[V],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    S[X] <: Seq[X]
](
    @transient val sc: SparkContext,
    val data: S[Cz[ID, O, V]],
    val chainableID: Int = scala.util.Random.nextInt,
    val currentVectorization: Vectorization[O, V] = EasyVectorization[O, V](0, None),
    val clusteringInfo: ClusteringInformationsLocal[ID, O, Cz, S] = new ClusteringInformationsLocal[ID, O, Cz, S]
)(implicit ct: ClassTag[Cz[ID, O, V]], ct2: ClassTag[S[Cz[ID, O, V]]]) extends DataExplorator[ID, O, V, Cz, S] with AlgorithmsRestrictions[ID, O, V, Cz, S] {
    /**
     *
     */
    protected implicit val currentVectorizationMapping = currentVectorization.vectoMapping
    /**
     * HMap of original and added Vectorization
     */
    val vectorizations: HMap[VectorizationMapping] = HMap[VectorizationMapping](currentVectorization.vectorizationID -> currentVectorization)
    /**
     * A securty value in order to allow proper reduce of Chaining models
     */
    protected val fusionChainableSecurity: Int = 0
    /**
     *
     */
    val globalClusteringRunNumber: Int = -1
    /**
     *
     */
    type Self[NV <: GVector[NV]] = ClusteringChainingClusteredSystem[ID, O, NV, Cz, S]
    /**
     *
     */
    type AlgorithmsRestrictions[NV <: GVector[NV]] = ClusteringAlgorithmLocal[ID, O, NV, Cz, S, ClusteringArgsLocal[NV], ClusteringModelLocal[ID, O, NV, Cz, S, ClusteringArgsLocal[NV]]]
    /**
     * Run one algorithm defined by user
     */
    def runAlgorithm(algorithm: AlgorithmsRestrictions[V]): ClusteringChainingClusteredSystem[ID, O, V, Cz, S] = {

        val model = algorithm.run(data)
        val updatedRunNumber = globalClusteringRunNumber + 1
        val updatedClusteringInfo = clusteringInfo.copy(clusteringInformations = clusteringInfo.clusteringInformations :+ ((updatedRunNumber, currentVectorization, algorithm.args, model)))
        val updatedData = model.obtainClustering(data)
        val updatedCurrentVectorization = currentVectorization.updateClustering(updatedRunNumber)
        val updatedVectorizations = vectorizations + ((updatedCurrentVectorization.vectorizationID, updatedCurrentVectorization))
        new ClusteringChainingClusteredSystem(sc, updatedData, chainableID, currentVectorization, updatedClusteringInfo) {
            override val vectorizations = updatedVectorizations
            override val globalClusteringRunNumber = updatedRunNumber
            override protected[chaining] val fusionChainableSecurity = 1
        }
    }
    /**
     *
     */
    def runAlgorithms(defaultParallelism: Int, algorithms: AlgorithmsRestrictions[V]*): Self[V] = {

        val dataBrodcasted = sc.broadcast(data)

        val clusteringChainingLocalFinal@ClusteringChainingLocal(dataFinal, chainableIDFinal, currentVectorizationFinal, clusteringInfoFinal) = sc.parallelize(algorithms.zipWithIndex, defaultParallelism).map{ case (algo, algIdx) =>
            val updatedFusionChainableSecurity = fusionChainableSecurity
            val updatedRunNumber = globalClusteringRunNumber + algIdx
            val updateVectorization = vectorizations
            (new ClusteringChainingLocal(dataBrodcasted.value, chainableID, currentVectorization, clusteringInfo) {
                override protected[chaining] val fusionChainableSecurity = updatedFusionChainableSecurity
                override val globalClusteringRunNumber = updatedRunNumber
                override val vectorizations = updateVectorization
            }).runAlgorithm(algo)
        }.reduce(_.fusionChainable(_))

        new ClusteringChainingClusteredSystem(sc, dataFinal, chainableIDFinal, currentVectorizationFinal, clusteringInfoFinal) {
            override val vectorizations = clusteringChainingLocalFinal.vectorizations
            override val globalClusteringRunNumber = clusteringChainingLocalFinal.globalClusteringRunNumber
            override protected[chaining] val fusionChainableSecurity = clusteringChainingLocalFinal.fusionChainableSecurity
        }
    }
    /**
     *
     */
    def addAnyVectorization[NV <: GVector[NV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B]](vectorization: Vecto[O, NV]): (ClusteringChainingClusteredSystem[ID, O, V, Cz, S], VectorizationMapping[VectorizationID, Vecto[O, NV]]) = {

        implicit val vectoMapping = new VectorizationMapping[VectorizationID, Vecto[O, NV]]
        val updatedVectorizations = vectorizations + ((vectorization.vectorizationID, vectorization))
        val updatedRunNumber = globalClusteringRunNumber
        val updatedFusionChainableSecurity = fusionChainableSecurity

        (
            new ClusteringChainingClusteredSystem(
                sc,
                data.map(_.addVectorized(vectorization.vectorizationID, vectorization.vectorizationFct.get)).asInstanceOf[S[Cz[ID, O, V]]],
                chainableID,
                currentVectorization,
                clusteringInfo
            ) {
                override val vectorizations = updatedVectorizations
                override val globalClusteringRunNumber = updatedRunNumber
                override protected[chaining] val fusionChainableSecurity = updatedFusionChainableSecurity
            },
            vectoMapping
        )

    }
    /**
     * Update the current vector for another
     */
    def updateAnyVector[NV <: GVector[NV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B]](vectorization: Vecto[O, NV])(implicit ct: ClassTag[Cz[ID, O, NV]], ct2: ClassTag[S[Cz[ID, O, NV]]]): ClusteringChainingClusteredSystem[ID, O, NV, Cz, S] = {

        val updatedVectorData = data.map(_.updateVector[NV](vectorization.vectorizationID)).asInstanceOf[S[Cz[ID, O, NV]]]
        val newCurrentVectorization = vectorizations.get(vectorization.vectorizationID)(new VectorizationMapping[VectorizationID, Vecto[O, NV]]).get
        val updatedGlobalRunNumber = globalClusteringRunNumber
        val fusionChainableSecurityCopy = fusionChainableSecurity
        val updatedVectorizations = vectorizations

        new ClusteringChainingClusteredSystem(sc, updatedVectorData, chainableID, newCurrentVectorization, clusteringInfo) {
            override val vectorizations = updatedVectorizations
            override val globalClusteringRunNumber = updatedGlobalRunNumber
            override protected[chaining] val fusionChainableSecurity = fusionChainableSecurityCopy
        }
    }
}