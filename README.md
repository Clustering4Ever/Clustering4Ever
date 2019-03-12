# Clustering :four: Ever  [ ![Download](https://api.bintray.com/packages/clustering4ever/C4E/clustering4ever/images/download.svg) ](https://bintray.com/clustering4ever/C4E/clustering4ever/_latestVersion) [![Maven Central](https://img.shields.io/maven-central/v/org.clustering4ever/clustering4ever_2.11.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:%22org.clustering4ever%22%20AND%20a:%22clustering4ever_2.11%22)

**Welcome** to Big Data Clustering Library gathering clustering algorithms and quality indices as immutabely as possible. Don't hesitate to check our **[Wiki](https://github.com/Clustering4Ever/Clustering4Ever/wiki)**, ask questions or make recommendations in our **[Gitter](https://gitter.im/Clustering4Ever/Lobby)**.

## [API documentation](http://www.clustering4ever.org/API%20Documentation/)

## Include it in your project

Add following line in your build.sbt :

  * `"org.clustering4ever" % "clustering4ever_2.11" % "0.9.3"` to your `libraryDependencies`

Eventually add one of these resolvers :

  * `resolvers += Resolver.bintrayRepo("clustering4ever", "C4E")`
  * `resolvers += "mvnrepository" at "http://mvnrepository.com/artifact/"`

You can also take [specifics parts](https://bintray.com/clustering4ever/C4E) :

  * Core [ ![Download](https://api.bintray.com/packages/clustering4ever/C4E/core/images/download.svg) ](https://bintray.com/clustering4ever/C4E/core/_latestVersion)
  * Scala Clustering [ ![Download](https://api.bintray.com/packages/clustering4ever/C4E/clusteringscala/images/download.svg) ](https://bintray.com/clustering4ever/C4E/clusteringscala/_latestVersion)
  * Spark Clustering [ ![Download](https://api.bintray.com/packages/clustering4ever/C4E/clusteringspark/images/download.svg) ](https://bintray.com/clustering4ever/C4E/clusteringspark/_latestVersion)

## Available algorithms

  * _emphasized algorithms_ are in Scala.
  * **bold algorithms** are implemented in Spark.
  * They can be available in **_both versions_**

### Clustering algorithms

  * _Jenks Natural Breaks_
  * **_Epsilon Proximity_** *
  * * **_Scalar Epsilon Proximity_** *
  * * _Binary Epsilon Proximity_ *
  * * _Mixed Epsilon Proximity_ *
  * * _Any Object Epsilon Proximity_ *
  * **_K-Centers_** *
    * **_K-Means_** *
    * **_K-Modes_** *
    * **_K-Prototypes_** *
    * **_Any Object K-Centers_** *
  * **Self Organizing Maps** ([Original project](https://github.com/TugdualSarazin/spark-clustering))
  * **G-Stream** ([Original project](https://github.com/Spark-clustering-notebook/G-stream))
  * **PatchWork** ([Original project](https://github.com/crim-ca/patchwork))
  * _Random Local Area_ *
  * **Clusterwize** 
  * _Tensor Biclustering algorithms_ ([Original project](https://github.com/SoheilFeizi/Tensor-Biclustering))
    * _Folding-Spectral_
    * _Unfolding-Spectral_
    * _Thresholding Sum Of Squared Trajectory Length_
    * _Thresholding Individuals Trajectory Length_
    * _Recursive Biclustering_
    * _Multiple Biclustering_


Algorithm followed with a * can be executed by benchmarking classes.

### Preprocessing

  * **_Gradient Ascent_** (Mean-Shift related)
    * **_Scalar Gradient Ascent_**
    * _Binary Gradient Ascent_
    * _Mixed Gradient Ascent_
    * _Any Object Gradient Ascent_
  * **_Rough Set Features Selection_**

### Quality Indices

You can realize manually your quality measures with dedicated class for local or distributed collection. Helpers _ClustersIndicesAnalysisLocal_ and _ClustersIndicesAnalysisDistributed_ allow you to test indices on multiple clustering at once.

  * Internal Indices
    * **_Davies Bouldin_**
    * **_Ball Hall_**
  * External Indices
    * **_Multiple Classification_**
      * **_Mutual Information_**
      * **_Normalized Mutual Information_**
      * **_Purity_**
      * **_RAND_**
      * **_ARAND_**
      * **_Accuracy_**
      * **_Precision_**
      * **_Recall_**
      * **_fBeta_**
      * **_f1_**
      * **_Matthews correlation coefficient_**
      * **_CzekanowskiDice_**
      * **_RogersTanimoto_**
      * **_FolkesMallows_**
      * **_Jaccard_**
      * **_Kulcztnski_**
      * **_McNemar_**
      * **_RusselRao_**
      * **_SokalSneath1_**
      * **_SokalSneath2_**
    * **_Binary Classification_**
      * **_Accuracy_**
      * **_Precision_**
      * **_Recall_**
      * **_fBeta_**
      * **_f1_**

### Clustering benchmarking and analysis

Using classes _ClusteringChainingLocal_, _BigDataClusteringChaining_, _DistributedClusteringChaining_, and _ChainingOneAlgorithm_ descendants you have the possibility to run multiple clustering algorithms respectively **locally and parallely**, **in a sequentially distributed way**, and **parallely on a distributed system**, **locally and parallely**, generate many different vectorizations of the data whilst keeping active information on each clustering including **used vectorization, clustering model, clustering number and clustering arguments**.

Classes _ClustersIndicesAnalysisLocal_ and _ClustersIndicesAnalysisDistributed_ are devoted for clustering indices analysis.

Classes _ClustersAnalysisLocal_ and _ClustersAnalysisDistributed_ will be use to describe obtained clustering in term of distributions, proportions of categorical features...

### Incoming soon
  
  * [UMAP](https://github.com/lmcinnes/umap)
  * **_Gaussian Mixture Models_**


## Citation

If you publish material based on informations obtained from this repository, then, in your acknowledgements, please note the assistance you received by using this community work. This will help others to obtain the same informations and **replicate your experiments**, because having results is cool but being able to compare to others is better.
Citation: `@misc{C4E, url = “https://github.com/Clustering4Ever/Clustering4Ever“, institution = “Paris 13 University, LIPN UMR CNRS 7030”}`

## C4E-Notebook examples

Basic usages of implemented algorithms are exposed with SparkNotebooks in [Spark-Clustering-Notebook](https://github.com/Spark-clustering-notebook/Clustering4Ever-Notebooks) organization.

## Miscellaneous

### Helper functions to generate Clusterizable collections 

You can easily generate your collections with basic Clusterizable using helpers in `org.clustering4ever.util.{ArrayAndSeqTowardGVectorImplicit, ScalaCollectionImplicits, SparkImplicits}` or explore Clusterizable and EasyClusterizable for more advanced usages.

### [References](https://github.com/Clustering4Ever/Clustering4Ever/wiki/5.-References)

### What data structures are recommended for best performances

  * ArrayBuffer as vector are a good start
  * ArrayBuffer, ParArray or ParSeq as vector containers are recommended