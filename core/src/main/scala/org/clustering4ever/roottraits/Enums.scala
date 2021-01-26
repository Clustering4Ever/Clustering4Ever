package org.clustering4ever.roottraits

/**
 * @author Beck Gaël
 */
/**
 *
 */
trait ClusteringIndices extends Serializable
/**
 *
 */
object InternalsIndices extends Enumeration with ClusteringIndices {
    type InternalsIndicesType = Value
    val DaviesBouldin,
        BallHall,
        Silhouette = Value
}
/**
 *
 */
object ExternalsIndices extends Enumeration with ClusteringIndices {
    type ExternalsIndicesType = Value
    val MI,
        NMI_Sqrt,
        NMI_Max,
        Purity,
        Accuracy,
        Precision,
        Recall,
        F1,
        MCC,
        CzekanowskiDice,
        RAND,
        RogersTanimoto,
        FolkesMallows,
        Jaccard,
        Kulcztnski,
        McNemar,
        RusselRao,
        SokalSneath1,
        SokalSneath2 = Value
}
/**
 *
 */
object NmiNormalizationNature extends Enumeration {
    type Normalization = Value
    val SQRT,
    	MAX = Value
}
/**
 * ENUM of different kernel types
 */
object KernelNature extends Enumeration {
    type KernelType = Value
    val Flat,
        KNN,
        KNN_Real,
        KNN_Binary,
        KNN_Euclidean,
        KNN_Hamming,
        Gaussian,
        Sigmoid = Value
}
/**
 *
 */
object ClusteringAlgorithmNatureEnum extends Enumeration {

    type ClusteringAlgorithmNatureEnum = Value

    val KCENTERS,
        KMEANS,
        KMODES,
        KPROTOTYPES,
        NOARGSALGO,
        RLA = Value
}