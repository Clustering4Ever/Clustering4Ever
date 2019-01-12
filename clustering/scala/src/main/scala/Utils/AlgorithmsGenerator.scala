package org.clustering4ever.clustering.generator
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{GenSeq, immutable}
import org.clustering4ever.clustering.{ClusteringArgs, ClusteringModelCz}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping}
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import org.clustering4ever.clustering.{ClusteringChaining, ClusteringAlgorithmCz, LocalClusteringAlgorithm}
import org.clustering4ever.extensibleAlgorithmNature._
import org.clustering4ever.scala.clustering.rla.{RLA, RLAArgs, RLAArgsReal, RLAArgsBinary, RLAArgsMixt}
import org.clustering4ever.scala.clustering.kcenters.{KCenters, KCentersArgs, KMeansArgs, KModesArgs, KPrototypesArgs}
import org.clustering4ever.enums.ClusteringAlgorithmNatureEnum
import org.clustering4ever.enums.ClusteringAlgorithmNatureEnum._
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixtVector}
/**
 *
 */
object AlgorithmsGenerator {
    /**
     *
     */     
    def generateAlgorithmsWithSameMetricOnRealData[ID, O, V <: Seq[Double] : ClassTag, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Double]] <: ContinuousDistance[X], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, ScalarVector[V]]], metric: D[V], algorithmsEnumsAndArgs: (ClusteringAlgorithmNatureEnum, ClusteringArgs)*)(implicit ct: ClassTag[Cz[ID, O, ScalarVector[V]]]): Seq[LocalClusteringAlgorithm[ID, O, ScalarVector[V], Cz, GS, ClusteringArgs, ClusteringModelCz[ID, O, ScalarVector[V], Cz, GS]]] = {
                
        algorithmsEnumsAndArgs.map{ case (algEnum, args) =>
            algEnum match {
                case enum if enum == KMEANS => new KCenters[ID, O, ScalarVector[V], Cz, D[V], GS, KMeansArgs[V, D]](args.asInstanceOf[KMeansArgs[V, D]])
                case enum if enum == RLA => new RLA[ID, O, ScalarVector[V], Cz, D[V], GS, RLAArgsReal[V, D]](args.asInstanceOf[RLAArgsReal[V, D]])
            }
        }
    }
    /**
     *
     */     
    def generateAlgorithmsWithSameMetricOnBinaryData[ID, O, V <: Seq[Int] : ClassTag, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Int]] <: BinaryDistance[X], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, BinaryVector[V]]], metric: D[V], algorithmsEnumsAndArgs: (ClusteringAlgorithmNatureEnum, ClusteringArgs)*)(implicit ct: ClassTag[Cz[ID, O, BinaryVector[V]]]): Seq[LocalClusteringAlgorithm[ID, O, BinaryVector[V], Cz, GS, ClusteringArgs, ClusteringModelCz[ID, O, BinaryVector[V], Cz, GS]]] = {
        algorithmsEnumsAndArgs.map{ case (algEnum, args) =>
            algEnum match {
                case enum if enum == KModes => new KCenters[ID, O, BinaryVector[V], Cz, D[V], GS, KModesArgs[V, D]](args.asInstanceOf[KModesArgs[V, D]])
                case enum if enum == RLA => new RLA[ID, O, BinaryVector[V], Cz, D[V], GS, RLAArgsBinary[V, D]](args.asInstanceOf[RLAArgsBinary[V, D]])
            }
        }
    }
    /**
     *
     */     
    def generateAlgorithmsWithSameMetricOnMixtData[ID, O, Vb <: Seq[Int] : ClassTag, Vs <: Seq[Double] : ClassTag, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Int], Y <: Seq[Double]] <: MixtDistance[X, Y], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, MixtVector[Vb, Vs]]], metric: D[Vb, Vs], algorithmsEnumsAndArgs: (ClusteringAlgorithmNatureEnum, ClusteringArgs)*)(implicit ct: ClassTag[Cz[ID, O, MixtVector[Vb, Vs]]]): Seq[LocalClusteringAlgorithm[ID, O, MixtVector[Vb, Vs], Cz, GS, ClusteringArgs, ClusteringModelCz[ID, O, MixtVector[Vb, Vs], Cz, GS]]] = {
        algorithmsEnumsAndArgs.map{ case (algEnum, args) =>
            algEnum match {
                case enum if enum == KPrototypes => new KCenters[ID, O, MixtVector[Vb, Vs], Cz, D[Vb, Vs], GS, KPrototypesArgs[Vb, Vs, D]](args.asInstanceOf[KPrototypesArgs[Vb, Vs, D]])
                case enum if enum == RLA => new RLA[ID, O, MixtVector[Vb, Vs], Cz, D[Vb, Vs], GS, RLAArgsMixt[Vb, Vs, D]](args.asInstanceOf[RLAArgsMixt[Vb, Vs, D]])
            }
        }
    }

}