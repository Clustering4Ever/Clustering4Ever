package org.clustering4ever.enums
/**
 * @author Beck Gaël
 */
/**
 *
 */
object InternalsIndices extends Enumeration {
    type InternalsIndicesType = Value
    val DaviesBouldin,
        BallHall,
        Silhouette = Value
}
/**
 *
 */
object ExternalsIndices extends Enumeration {
    type ExternalsIndicesType = Value
    val MI,
        NMI_Sqrt,
        NMI_Max = Value
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