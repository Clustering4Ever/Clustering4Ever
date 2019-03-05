package org.clustering4ever.clustering.chaining
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{GenSeq, mutable, immutable, parallel}
import shapeless.{HMap, HNil}
import org.clustering4ever.clustering.ClusteringModelLocal
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping, ClusteringInformationsMapping, ModelsMapping}
import org.clustering4ever.extensibleAlgorithmNature._
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixedDistance}
import org.clustering4ever.clustering.kcenters.scala.KCenters
import org.clustering4ever.clustering.{ClusteringAlgorithmLocalBinary, ClusteringAlgorithmLocalScalar, ClusteringModelLocalScalar, ClusteringModelLocalBinary, ClusteringSharedTypes, ClusteringAlgorithmLocal}
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.types.ClusteringInformationTypes._
import org.clustering4ever.enums.ClusteringIndices
import org.clustering4ever.enums.ExternalsIndices._
import org.clustering4ever.enums.InternalsIndices._
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixedVector}
import org.clustering4ever.vectorizations.{VectorizationGenLocal, VectorizationLocalScalar, VectorizationLocalBinary}
import org.clustering4ever.clustering.indices.MultiExternalIndicesLocal
import org.clustering4ever.clustering.keeper.ModelsKeeper
import org.clustering4ever.extensibleAlgorithmNature.ClusteringAlgorithmNature
import org.clustering4ever.utils.SortGsCz
/**
 * This classe intend to run many algorithms parallely on a local system for medium size datasets, it works for one version of an algorithm with various parameters
 * @tparam O the raw object from which vectorizations came from
 * @tparam V the nature of the working vector
 * @tparam Cz a clusterizable descendant, EasyClusterizable is the basic advise instance
 * @tparam Vecto the current vectorization which gives the current Vector nature
 * @tparam GS the nature of the collection containing Cz[O, V]
 * @tparam CM the clustering model type corresponding to the nature of the given algorithm type
 * @tparam Algorithms the clustering algorith type
 * @param data the dataset ideally sorted by Cz's IDs, if not it's done automatically
 * @param vectorizations vectorizations employed on the algorithms list
 * @param algorithms the sequence of algorithms which will be executed at the instantiation of the class
 * @param isDatasetSortedByID a neccessary security due to specific algorithm where model is the entire dataset and then require to be aligned with input data
 */
trait ChainingOneAlgorithm[
    O,
    V <: GVector[V],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    GS[X] <: GenSeq[X],
    Vecto <: VectorizationGenLocal[O, V, Vecto],
    CM <: ClusteringModelLocal[V],
    Algorithms <: ClusteringAlgorithmLocal[V, CM]
] extends ClusteringSharedTypes {
    /**
     * the dataset of clusterizable
     */
    val data: GS[Cz[O, V]]
    /**
     * Necessary security for specific algorithm which require to have same data order
     */
    val isDatasetSortedByID: Boolean
    /**
     * the current vectorization employed
     */
    val vectorizations: Seq[Vecto]
    /**
     * Seq the algorithm which will be launched with various parameters
     */
    val algorithms: Seq[Algorithms]
    /**
     *
     */
    private final val previousClusteringNumber: Int = data.head.clusterIDs.size
    /**
     * Indices of launched algorithm, ie (previousClusteringNumber until previousClusteringNumber + algorithms.size)
     */
    final val algorithmsIndices: Seq[Seq[Int]] = {
        @annotation.tailrec
        def go(i: Int, buff: Seq[Seq[Int]]): Seq[Seq[Int]] = {
            if(i == 0) go(i + 1, buff :+ (previousClusteringNumber until (previousClusteringNumber + algorithms.size)))
            else if(i < vectorizations.size) go(i + 1, buff :+ ((buff(i - 1).last + 1) until (buff(i - 1).last + 1 + algorithms.size)))
            else buff
        }
        go(0, Seq.empty[Seq[Int]])
    }
    /**
     *
     */
    final val algorithmNature: ClusteringAlgorithmNature = algorithms.head.algorithmID
    /**
     * Model Mapping required to access to specific model in ModelsKeeper HMAP
     */
    final val modelMapping: ModelsMapping[Int, CM] = ModelsMapping[Int, CM]
    /**
     *
     */
    val modelsKeeper: ModelsKeeper
    /**
     *
     */
    final val dataSortedByID: GS[Cz[O, V]] = if(!isDatasetSortedByID) SortGsCz.sortByID(data) else data
    /**
     * Add to current vectorization algorithm made with their indices
     */
    vectorizations.zip(algorithmsIndices).foreach{ case (vecto, algIndices) => vecto.updateAlgorithms(algIndices.map((_, algorithmNature)):_*) }
    /**
     * ParSeq of Collection of ClusterID,  each ParSeq elem correspond to the given algorithms order
     */
    final val clusteringIDsResults: parallel.ParSeq[GS[ClusterID]] = {

        if(vectorizations.size == 1) {   
            val (modelsIn, clusteringIDsResultsIn) = algorithms.par.map{ alg =>
                val model = alg.fit(dataSortedByID)
                val clusteringIDs = model.obtainClusteringIDs(dataSortedByID)
                (model, clusteringIDs)
            }.unzip
            // Update modelsKeeper with resulting models
            modelsKeeper.addModels(modelsIn.seq.zip(algorithmsIndices.head):_*)
            clusteringIDsResultsIn
        }
        else {
            vectorizations.zipWithIndex.par.map{ case (vecto, vectoIdx) =>
                val newVectorizationDS = dataSortedByID.map(_.updateVectorizationOfSameNature(vecto)).asInstanceOf[GS[Cz[O, V]]]
                val (modelsIn, clusteringIDsResultsIn) = algorithms.par.map{ alg =>
                    val model = alg.fit(newVectorizationDS)
                    val clusteringIDs = model.obtainClusteringIDs(newVectorizationDS)
                    (model, clusteringIDs)
                }.unzip
                // Update modelsKeeper with resulting models
                modelsKeeper.addModels(modelsIn.seq.zip(algorithmsIndices(vectoIdx)):_*)
                clusteringIDsResultsIn
            }.flatten
        }

    }
    /**
     * get one model from those resulting from given clustering algorithm
     */
    final def getModel(i: Int): Option[CM] = modelsKeeper.getModel(i, modelMapping)
    /**
     * get all models resulting from given clustering algorithm for a particular vectorization
     */
    final def getModelsFromVecto(vecto: Vecto): Seq[CM] = vecto.runnedAlgorithms.map(_._1).map( algIdx => modelsKeeper.getModel(algIdx, modelMapping).get )
    /**
     * get all models resulting from given clustering algorithm
     */
    final def getAllModels: Seq[CM] = algorithmsIndices.flatten.map(getModel(_).get) 
    /**
     * The Dataset vectorization is the original one
     *
     * @return the linked dataset between clusterizable and their associate clusteringIDs sorted by clusterizable ID
     */
    final def obtainSortedClusterizedData: GS[Cz[O, V]] = {
        val allClusteringIDs = clusteringIDsResults.map( clusteringIDsFromOneAlgo => clusteringIDsFromOneAlgo.map(mutable.ArrayBuffer(_)) ).reduce( (a, b) => a.zip(b).map{ case (c, d) => c ++= d } )
        dataSortedByID.zip(allClusteringIDs).map{ case (cz, clusteringIDs) => cz.addClusterIDs(clusteringIDs:_*) }.asInstanceOf[GS[Cz[O, V]]]
    }
    /**
     *
     */
    final def getEveryMIPerClustering(groundTruth: GenSeq[Int]): GenSeq[(Double, Double, Double)] = {
        clusteringIDsResults.map{ clusteringIDsFromOneAlgo => 
            val externalIndices = MultiExternalIndicesLocal(groundTruth.zip(clusteringIDsFromOneAlgo))
            (externalIndices.mutualInformation, externalIndices.nmiSQRT, externalIndices.nmiMAX)
        }
    }

}
/**
 * This classe intend to run many algorithms parallely on a local system for medium size datasets, it works for one version of an algorithm with various parameters
 * @tparam O the raw object from which vectorizations came from
 * @tparam V the nature of the working vector
 * @tparam Cz a clusterizable descendant, EasyClusterizable is the basic advise instance
 * @tparam Vecto the current vectorization which gives the current Vector nature
 * @tparam GS the nature of the collection containing Cz[O, V]
 * @tparam CM the clustering model type corresponding to the nature of the given algorithm type
 * @tparam Algorithms the clustering algorith type
 * @param data the dataset ideally sorted by Cz's IDs, if not it's done automatically
 * @param vectorizations vectorizations employed on the algorithms list
 * @param algorithms the sequence of algorithms which will be executed at the instantiation of the class
 * @param isDatasetSortedByID a neccessary security due to specific algorithm where model is the entire dataset and then require to be aligned with input data
 */
final case class LocalClusteringChainingScalar[
    O,
    V <: Seq[Double],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    CM <: ClusteringModelLocalScalar[V],
    Vecto[A, B <: Seq[Double]] <: VectorizationLocalScalar[A, B, Vecto[A, B]],
    GS[X] <: GenSeq[X],
    Algorithms[A <: Seq[Double], B <: ClusteringModelLocalScalar[A]] <: ClusteringAlgorithmLocalScalar[A, B]
](
    final val data: GS[Cz[O, ScalarVector[V]]],
    final val isDatasetSortedByID: Boolean,
    final val vectorizations: Seq[Vecto[O, V]],
    final val algorithms: Seq[Algorithms[V, CM]],
    final val modelsKeeper: ModelsKeeper = new ModelsKeeper
) extends ChainingOneAlgorithm[O, ScalarVector[V], Cz, GS, Vecto[O, V], CM, Algorithms[V, CM]]
/**
 * This classe intend to run many algorithms parallely on a local system for medium size datasets, it works for one version of an algorithm with various parameters
 * @tparam O the raw object from which vectorizations came from
 * @tparam V the nature of the working vector
 * @tparam Cz a clusterizable descendant, EasyClusterizable is the basic advise instance
 * @tparam Vecto the current vectorization which gives the current Vector nature
 * @tparam GS the nature of the collection containing Cz[O, V]
 * @tparam CM the clustering model type corresponding to the nature of the given algorithm type
 * @tparam Algorithms the clustering algorith type
 * @param data the dataset ideally sorted by Cz's IDs, if not it's done automatically
 * @param vectorizations vectorizations employed on the algorithms list
 * @param algorithms the sequence of algorithms which will be executed at the instantiation of the class
 * @param isDatasetSortedByID a neccessary security due to specific algorithm where model is the entire dataset and then require to be aligned with input data
 */
final case class LocalClusteringChainingBinary[
    O,
    V <: Seq[Int],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    CM <: ClusteringModelLocalBinary[V],
    Vecto[A, B <: Seq[Int]] <: VectorizationLocalBinary[A, B, Vecto[A, B]],
    GS[X] <: GenSeq[X],
    Algorithms[A <: Seq[Int], B <: ClusteringModelLocalBinary[A]] <: ClusteringAlgorithmLocalBinary[A, B]
](
    final val data: GS[Cz[O, BinaryVector[V]]],
    final val isDatasetSortedByID: Boolean,
    final val vectorizations: Seq[Vecto[O, V]],
    final val algorithms: Seq[Algorithms[V, CM]],
    final val modelsKeeper: ModelsKeeper = new ModelsKeeper
) extends ChainingOneAlgorithm[O, BinaryVector[V], Cz, GS, Vecto[O, V], CM, Algorithms[V, CM]]