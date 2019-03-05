package Quantum

import batchStream.DQC.sc
import breeze.linalg.Vector
import breeze.numerics.{exp, sqrt}
import org.apache.spark.rdd.RDD
import org.clustering4ever.clustering.{ClusteringArgsDistributed, ClusteringModelDistributed}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.spark.clustering.kcenters.{KCentersArgsAncestor, KCentersModelAncestor}
import org.clustering4ever.vectors.GVector

import scala.collection.mutable.ArrayBuffer
/*
trait DQCAncestor[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D <: Distance[V], Args <: DQCAArgsAncestor[V, D], Model <: DQCAsModelAncestor[ID, O, V, Cz, D] {


}

trait DQCAArgsAncestor[V <: GVector[V], D <: Distance[V]] extends ClusteringArgsDistributed[V]


trait DQCAsModelAncestor[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D <: Distance[V]] extends ClusteringModelDistributed[ID, O, V, Cz]*/