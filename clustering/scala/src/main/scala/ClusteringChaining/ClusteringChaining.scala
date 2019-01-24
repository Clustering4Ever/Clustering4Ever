package org.clustering4ever.clustering.chaining
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{GenSeq, mutable, immutable}
import shapeless.{HMap, HNil}
import org.clustering4ever.clustering.{ClusteringArgsLocal, ClusteringModelLocal}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping, ClusteringInformationsMapping}
import org.clustering4ever.extensibleAlgorithmNature._
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import org.clustering4ever.clustering.kcenters.scala.KCenters
import org.clustering4ever.clustering.{ClusteringChaining, ClusteringAlgorithm, ClusteringAlgorithmLocal, ClusteringInformationsLocal}
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.types.ClusteringInformationTypes._
import org.clustering4ever.enums.ClusteringAlgorithmNatureEnum
import org.clustering4ever.enums.ClusteringAlgorithmNatureEnum._
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixtVector}
import org.clustering4ever.vectorizations.{VectorizationLocal, EasyVectorizationLocal}
/**
 * This classe intend to run many algorithms parallely on a local system for medium size datasets
 */
case class ClusteringChainingLocal[
    ID,
    O,
    V <: GVector[V],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    Vecto <: VectorizationLocal[O, V, Vecto],
    GS[X] <: GenSeq[X]
](
    val data: GS[Cz[ID, O, V]],
    val chainableID: Int,
    val currentVectorization: Vecto,
    val clusteringInformations: HMap[ClusteringInformationsMapping] = HMap.empty[ClusteringInformationsMapping]
)(implicit val ct: ClassTag[Cz[ID, O, V]]) extends ClusteringChaining[ID, O, V, Cz, Vecto, GS] {
    /**
     *
     */
    private implicit val currentVectorizationMapping = currentVectorization.vectoMapping
    /**
     *
     */
    private implicit val currentClusteringInformationMapping = ClusteringInformationsMapping[VectorizationID, ClusteringInformationsLocal[ID, O, V, Cz, Vecto, GS]]
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
    type Self[GV <: GVector[GV], OtherVecto <: VectorizationLocal[O, GV, OtherVecto]] = ClusteringChainingLocal[ID, O, GV, Cz, OtherVecto, GS]
    /**
     *
     */
    type AlgorithmsRestrictions[GV <: GVector[GV]] = ClusteringAlgorithmLocal[ID, O, GV, Cz, GS, ClusteringArgsLocal[GV], ClusteringModelLocal[ID, O, GV, Cz, GS, ClusteringArgsLocal[GV]]]
    /**
     *
     */
    protected[chaining] def fusionChainable(ccl: Self[V, Vecto]): Self[V, Vecto] = {
        val newFusionSecurity = fusionChainableSecurity + ccl.fusionChainableSecurity
        val updatedRunNumber = scala.math.max(globalClusteringRunNumber, ccl.globalClusteringRunNumber)
        val aVecto = vectorizations.get(currentVectorization.vectorizationID).get
        val bVecto = ccl.vectorizations.get(currentVectorization.vectorizationID).get
        val updatedCurrentVectorization = currentVectorization.updateClustering(aVecto.clusteringNumbers.union(bVecto.clusteringNumbers).toSeq:_*)
        val updatedVectorizations = vectorizations + ((updatedCurrentVectorization.vectorizationID, updatedCurrentVectorization))
        // To handle with clustering indices
        val aInfos = clusteringInformations.get(updatedCurrentVectorization.vectorizationID).get
        val bClusteringInfos = ccl.clusteringInformations.get(updatedCurrentVectorization.vectorizationID).get.clusteringInformations
        val updatedInfos = clusteringInformations + ((updatedCurrentVectorization.vectorizationID, aInfos.copy(clusteringInformations = aInfos.clusteringInformations ++ bClusteringInfos)))

        new ClusteringChainingLocal(
            data.zip(ccl.data).map{ case (cz1, cz2) => cz1.addClusterIDs(cz2.clusterIDs.takeRight(ccl.fusionChainableSecurity):_*) }.asInstanceOf[GS[Cz[ID, O, V]]],
            chainableID,
            updatedCurrentVectorization,
            updatedInfos
        ) {
            override val vectorizations = updatedVectorizations
            override val globalClusteringRunNumber = updatedRunNumber
            override protected[chaining] val fusionChainableSecurity = newFusionSecurity
        }
    }
    /**
     * Run one algorithm defined by user
     */
    def runAlgorithm(algorithm: AlgorithmsRestrictions[V]): Self[V, Vecto] = {
        val model = algorithm.run(data)
        val updatedRunNumber = globalClusteringRunNumber + 1
        val updatedData = model.obtainClustering(data)
        val updatedCurrentVectorization = currentVectorization.updateClustering(updatedRunNumber)
        val updatedVectorizations = vectorizations + ((updatedCurrentVectorization.vectorizationID, updatedCurrentVectorization))
        val aInfos = clusteringInformations.get(updatedCurrentVectorization.vectorizationID)
        val updatedInfos = if(aInfos.isDefined) clusteringInformations + ((
                updatedCurrentVectorization.vectorizationID,
                aInfos.get.copy(clusteringInformations = aInfos.get.clusteringInformations + ((updatedRunNumber, updatedCurrentVectorization, algorithm.args, model)))
            ))
            else HMap[ClusteringInformationsMapping](
                updatedCurrentVectorization.vectorizationID -> ClusteringInformationsLocal(immutable.HashSet((updatedRunNumber, updatedCurrentVectorization, algorithm.args, model)))
            )
        new ClusteringChainingLocal(updatedData, chainableID, updatedCurrentVectorization, updatedInfos) {
            override val vectorizations = updatedVectorizations
            override val globalClusteringRunNumber = updatedRunNumber
            override protected[chaining] val fusionChainableSecurity = 1
        }
    }
    /**
     * Run multiples algorithms defined by user
     */
    def runAlgorithms(algorithms: AlgorithmsRestrictions[V]*): Self[V, Vecto] = {

        val updatedFusionChainableSecurity = fusionChainableSecurity

        algorithms.par.zipWithIndex.map{ case (alg, algIdx) =>

            val updatedRunNumber = globalClusteringRunNumber + algIdx
            val updatedVectorizations = vectorizations
            (new ClusteringChainingLocal(data, chainableID, currentVectorization, clusteringInformations) {
                override val vectorizations = updatedVectorizations
                override val globalClusteringRunNumber = updatedRunNumber
                override protected[chaining] val fusionChainableSecurity = updatedFusionChainableSecurity
            }).runAlgorithm(alg)

        }.reduce(_.fusionChainable(_))

    }
    /**
     *
     */
    private def internalUpdating[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto[A, B]]](vectorization: OtherVecto[O, GV]) = {
        val updatedRunNumber = globalClusteringRunNumber
        val updatedVectorizations = vectorizations.+((vectorization.vectorizationID, vectorization))(vectorization.vectoMapping)
        val updatedFusionChainableSecurity = fusionChainableSecurity
        (updatedRunNumber, updatedVectorizations, updatedFusionChainableSecurity)
    }
    /**
     *
     */
    def updateVectorizationOnData[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto[A, B]]](vectorization: OtherVecto[O, GV])(implicit ct: ClassTag[Cz[ID, O, GV]]): Self[GV, OtherVecto[O, GV]] = {

        val (updatedRunNumber, updatedVectorizations, updatedFusionChainableSecurity) = internalUpdating(vectorization)
        new ClusteringChainingLocal(
            data.map(_.updateVectorization(vectorization)).asInstanceOf[GS[Cz[ID, O, GV]]],
            chainableID,
            vectorization,
            clusteringInformations
        ) {
            override val vectorizations = updatedVectorizations
            override val globalClusteringRunNumber = updatedRunNumber
            override protected[chaining] val fusionChainableSecurity = updatedFusionChainableSecurity
        }
    }
    /**
     *
     */
    def addVectorization[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto[A, B]]](vectorization: OtherVecto[O, GV]): Self[V, Vecto] = {

        val (updatedRunNumber, updatedVectorizations, updatedFusionChainableSecurity) = internalUpdating(vectorization)
        new ClusteringChainingLocal(
            data,
            chainableID,
            currentVectorization,
            clusteringInformations
        ) {
            override val vectorizations = updatedVectorizations
            override val globalClusteringRunNumber = updatedRunNumber
            override protected[chaining] val fusionChainableSecurity = updatedFusionChainableSecurity
        }
    }
    /**
     *
     */
    def addVectorizationOnData[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto[A, B]]](vectorization: OtherVecto[O, GV]): Self[V, Vecto] = {

        val (updatedRunNumber, updatedVectorizations, updatedFusionChainableSecurity) = internalUpdating(vectorization)
        new ClusteringChainingLocal(
            data.map(_.addVectorization(vectorization)).asInstanceOf[GS[Cz[ID, O, V]]],
            chainableID,
            currentVectorization,
            clusteringInformations
        ) {
            override val vectorizations = updatedVectorizations
            override val globalClusteringRunNumber = updatedRunNumber
            override protected[chaining] val fusionChainableSecurity = updatedFusionChainableSecurity
        }
    }
    /**
     * Update the current vector for another
     */
    def switchToAnotherExistantVector[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto[A, B]]](vectorization: OtherVecto[O, GV])(implicit ct: ClassTag[Cz[ID, O, GV]]): Self[GV, OtherVecto[O, GV]] = {
        val updatedVectorData = data.map(_.switchForExistingVector(vectorization)).asInstanceOf[GS[Cz[ID, O, GV]]]
        val updatedCurrentVectorization = vectorizations.get(vectorization.vectorizationID)(VectorizationMapping[VectorizationID, OtherVecto[O, GV]]).get
        val updatedGlobalRunNumber = globalClusteringRunNumber
        val fusionChainableSecurityCopy = fusionChainableSecurity
        val updatedVectorizations = vectorizations
        new ClusteringChainingLocal(updatedVectorData, chainableID, updatedCurrentVectorization, clusteringInformations) {
            override val vectorizations = updatedVectorizations
            override val globalClusteringRunNumber = updatedGlobalRunNumber
            override protected[chaining] val fusionChainableSecurity = fusionChainableSecurityCopy
        }
    }
    /**
     *
     */
    // def runAlgorithmsOnMultipleVectorizationsOfSameNature[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto]](
    //     vectorizations: Seq[OtherVecto],
    //     algorithms: AlgorithmsRestrictions[V]*
    // ): Self[V, OtherVecto] = {
    //     vectorizations.par.map( vectorization => switchToAnotherExistantVector(vectorization).runAlgorithms(algorithms:_*) ).reduce(_.fusionChainable(_))
    // }
}
/**
 *
 */
object ClusteringChainingLocal extends Serializable {
    /**
     *
     */
    def apply[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, V]], chainingID: Int)(implicit ct: ClassTag[Cz[ID, O, V]]): ClusteringChainingLocal[ID, O, V, Cz, EasyVectorizationLocal[O, V], GS] = {
        new ClusteringChainingLocal(
            data,
            chainingID,
            EasyVectorizationLocal[O, V](0)
        )
    }
    /**
     *
     */
    def apply[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): ClusteringChainingLocal[ID, O, V, Cz, EasyVectorizationLocal[O, V], GS] = {
        apply(data, scala.util.Random.nextInt)
    }
}