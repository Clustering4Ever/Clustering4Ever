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
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping}
import org.clustering4ever.extensibleAlgorithmNature._
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import org.clustering4ever.scala.clustering.kcenters.KCenters
import org.clustering4ever.clustering.{ClusteringChaining, ClusteringAlgorithmCz, ClusteringAlgorithmLocal, ClusteringInformationsLocal}
import org.clustering4ever.vectorizations.{Vectorization, EasyVectorization}
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.types.ClusteringInformationTypes._
import org.clustering4ever.enums.ClusteringAlgorithmNatureEnum
import org.clustering4ever.enums.ClusteringAlgorithmNatureEnum._
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixtVector}
import org.clustering4ever.vectorizations.{VectorizationNature, Default}
/**
 * This classe intend to run many algorithms parallely on a local system for medium size datasets
 */
case class ClusteringChainingLocal[
    ID,
    O,
    V <: GVector[V],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    GS[X] <: GenSeq[X]
](
    val data: GS[Cz[ID, O, V]],
    val chainableID: Int = scala.util.Random.nextInt,
    val currentVectorization: Vectorization[O, V] = EasyVectorization[O, V](0),
    val clusteringInfo: ClusteringInformationsLocal[ID, O, Cz, GS] = new ClusteringInformationsLocal[ID, O, Cz, GS]
)(implicit val ct: ClassTag[Cz[ID, O, V]]) extends ClusteringChaining[ID, O, V, Cz, GS] {
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
    type Self[NV <: GVector[NV]] = ClusteringChainingLocal[ID, O, NV, Cz, GS]
    /**
     *
     */
    type AlgorithmsRestrictions[NV <: GVector[NV]] = ClusteringAlgorithmLocal[ID, O, NV, Cz, GS, ClusteringArgsLocal[NV], ClusteringModelLocal[ID, O, NV, Cz, GS, ClusteringArgsLocal[NV]]]
    /**
     *
     */
    protected[chaining] def fusionChainable(ccl: ClusteringChainingLocal[ID, O, V, Cz, GS]): ClusteringChainingLocal[ID, O, V, Cz, GS] = {

        val newFusionSecurity = fusionChainableSecurity + ccl.fusionChainableSecurity
        val updatedRunNumber = scala.math.max(globalClusteringRunNumber, ccl.globalClusteringRunNumber)
        val aVecto = vectorizations.get(currentVectorization.vectorizationID).get
        val bVecto = ccl.vectorizations.get(currentVectorization.vectorizationID).get
        val updatedCurrentVectorization = currentVectorization.updateClustering(aVecto.clusteringNumbers.toSeq ++ bVecto.clusteringNumbers:_*)
        val updatedVectorizations = vectorizations + ((updatedCurrentVectorization.vectorizationID, updatedCurrentVectorization))

        new ClusteringChainingLocal(
            data.zip(ccl.data).map{ case (cz1, cz2) => cz1.addClusterIDs(cz2.clusterIDs.takeRight(ccl.fusionChainableSecurity):_*) }.asInstanceOf[GS[Cz[ID, O, V]]],
            chainableID,
            updatedCurrentVectorization,
            clusteringInfo.copy(clusteringInformations = clusteringInfo.clusteringInformations ++ ccl.clusteringInfo.clusteringInformations.takeRight(ccl.fusionChainableSecurity))
        ) {
            override val vectorizations = updatedVectorizations
            override val globalClusteringRunNumber = updatedRunNumber
            override protected[chaining] val fusionChainableSecurity = newFusionSecurity
        }
    }
    /**
     * Run one algorithm defined by user
     */
    def runAlgorithm(algorithm: AlgorithmsRestrictions[V]): ClusteringChainingLocal[ID, O, V, Cz, GS]
     = {
        val model = algorithm.run(data)
        val updatedRunNumber = globalClusteringRunNumber + 1
        val updatedClusteringInfo = clusteringInfo.copy(clusteringInformations = clusteringInfo.clusteringInformations :+ ((updatedRunNumber, currentVectorization, algorithm.args, model)))
        val updatedData = model.obtainClustering(data)
        val updatedCurrentVectorization = currentVectorization.updateClustering(updatedRunNumber)
        val updatedVectorizations = vectorizations + ((updatedCurrentVectorization.vectorizationID, updatedCurrentVectorization))
        new ClusteringChainingLocal(updatedData, chainableID, updatedCurrentVectorization, updatedClusteringInfo) {
            override val vectorizations = updatedVectorizations
            override val globalClusteringRunNumber = updatedRunNumber
            override protected[chaining] val fusionChainableSecurity = 1
        }
    }
    /**
     * Run multiples algorithms defined by user
     */
    def runAlgorithms(algorithms: AlgorithmsRestrictions[V]*): ClusteringChainingLocal[ID, O, V, Cz, GS] = {

        val updatedFusionChainableSecurity = fusionChainableSecurity

        algorithms.par.zipWithIndex.map{ case (alg, algIdx) =>

            val updatedRunNumber = globalClusteringRunNumber + algIdx
            val updatedVectorizations = vectorizations

            (new ClusteringChainingLocal(data, chainableID, currentVectorization, clusteringInfo) {
                override val vectorizations = updatedVectorizations
                override val globalClusteringRunNumber = updatedRunNumber
                override protected[chaining] val fusionChainableSecurity = updatedFusionChainableSecurity
            }).runAlgorithm(alg)

        }.reduce(_.fusionChainable(_))

    }
    /**
     * Run multiples algorithms defined by user as runAlgorithms but with another way that i suppose less memory greedy
     */
    // def runAlgorithms2(algorithms: AlgorithmsRestrictions[V]*) : ClusteringChainingLocal[ID, O, V, Cz, GS] = {

    //     val updatedFusionChainableSecurity = fusionChainableSecurity + algorithms.size
    //     val updatedGlobalClusteringRunNumber = globalClusteringRunNumber + algorithms.size

    //     val pAlgo = algorithms.par
    //     val argss = pAlgo.zipWithIndex.map{ case (algo, idx) => (globalClusteringRunNumber + idx + 1, currentVectorization, algo.args) }
    //     val models = pAlgo.map(_.run(data))
    //     val clusterIDs = models.map(_.obtainClusteringIDs(data)).map(_.map(mutable.Buffer(_))).reduce(_.zip(_).map{ case (a, b) => a ++= b})
    //     val clusteringInfoss = ClusteringInformationsLocal(argss.zip(models).map{ case ((a, cv, algo), model) => (a, cv, algo, model) }.toVector)
    //     val updatedData = data.zip(clusterIDs).map{ case (cz, ids) => cz.addClusterIDs(ids:_*) }.asInstanceOf[GS[Cz[ID, O, V]]]
    //     val updatedCurrentVectorization = currentVectorization.updateClustering((globalClusteringRunNumber until algorithms.size):_*)
    //     val updatedVectorizations = vectorizations + ((updatedCurrentVectorization.vectorizationID, updatedCurrentVectorization))

    //     (new ClusteringChainingLocal(updatedData, chainableID, currentVectorization, clusteringInfoss) {
    //         override protected[chaining] val fusionChainableSecurity = updatedFusionChainableSecurity
    //         override val vectorizations = updatedVectorizations
    //         override val globalClusteringRunNumber = updatedGlobalClusteringRunNumber
    //     })

    // }
    /**
     *
     */
    def addAnyVectorization[NV <: GVector[NV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B]](vectorization: Vecto[O, NV]): (ClusteringChainingLocal[ID, O, V, Cz, GS], VectorizationMapping[VectorizationID, Vecto[O, NV]]) = {

        implicit val vectoMapping = new VectorizationMapping[VectorizationID, Vecto[O, NV]]
        val updatedVectorizations = vectorizations + ((vectorization.vectorizationID, vectorization))
        val updatedRunNumber = globalClusteringRunNumber
        val updatedFusionChainableSecurity = fusionChainableSecurity

        (
            new ClusteringChainingLocal(
                data.map(_.addVectorized(vectorization.vectorizationID, vectorization.vectorizationFct.get)).asInstanceOf[GS[Cz[ID, O, V]]],
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
    def updateAnyVector[NV <: GVector[NV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B]](vectorization: Vecto[O, NV])(implicit ct: ClassTag[Cz[ID, O, NV]]): ClusteringChainingLocal[ID, O, NV, Cz, GS] = {
        val updatedVectorData = data.map(_.updateVector[NV](vectorization.vectorizationID)).asInstanceOf[GS[Cz[ID, O, NV]]]
        val newCurrentVectorization = vectorizations.get(vectorization.vectorizationID)(new VectorizationMapping[VectorizationID, Vecto[O, NV]]).get
        val updatedGlobalRunNumber = globalClusteringRunNumber
        val fusionChainableSecurityCopy = fusionChainableSecurity
        val updatedVectorizations = vectorizations
        new ClusteringChainingLocal(updatedVectorData, chainableID, newCurrentVectorization, clusteringInfo) {
            override val vectorizations = updatedVectorizations
            override val globalClusteringRunNumber = updatedGlobalRunNumber
            override protected[chaining] val fusionChainableSecurity = fusionChainableSecurityCopy
        }
    }
    /**
     *
     */
    def runAlgorithmsOnMultipleVectorizations[Vecto[A, B <: GVector[B]] <: Vectorization[A, B]](
        vectorizations: Seq[Vecto[O, V]],
        algorithms: AlgorithmsRestrictions[V]*
    ): ClusteringChainingLocal[ID, O, V, Cz, GS] = {
        vectorizations.par.map( vectorization => updateVector(vectorization).runAlgorithms(algorithms:_*) ).reduce(_.fusionChainable(_))
    }
}