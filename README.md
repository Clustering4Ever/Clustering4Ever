# Clustering :four: Ever  [ ![Download](https://api.bintray.com/packages/clustering4ever/C4E/clustering4ever/images/download.svg) ](https://bintray.com/clustering4ever/C4E/clustering4ever/_latestVersion) [![Maven Central](https://img.shields.io/maven-central/v/org.clustering4ever/clustering4ever_2.11.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:%22org.clustering4ever%22%20AND%20a:%22clustering4ever_2.11%22)

**Welcome** to Big Data Clustering Library gathering clustering algorithms and quality indices as immutabely as possible. Don't hesitate to check our **[Wiki](https://github.com/Clustering4Ever/Clustering4Ever/wiki)**, ask questions or make recommendations in our **[Gitter](https://gitter.im/Clustering4Ever/Lobby)**.

## [API documentation](http://www.clustering4ever.org/API%20Documentation/)

## Include it in your project

Add following line in your build.sbt :

* `"org.clustering4ever" % "clustering4ever_2.11" % "0.8.4"` to your `libraryDependencies`

Eventually add this resolver :

* `resolvers += Resolver.bintrayRepo("clustering4ever", "C4E")`

You can also take [specifics parts](https://bintray.com/clustering4ever/C4E) :

* Core [ ![Download](https://api.bintray.com/packages/clustering4ever/C4E/core/images/download.svg) ](https://bintray.com/clustering4ever/C4E/core/_latestVersion)
* Scala Clustering [ ![Download](https://api.bintray.com/packages/clustering4ever/C4E/clusteringscala/images/download.svg) ](https://bintray.com/clustering4ever/C4E/clusteringscala/_latestVersion)
* Spark Clustering [ ![Download](https://api.bintray.com/packages/clustering4ever/C4E/clusteringspark/images/download.svg) ](https://bintray.com/clustering4ever/C4E/clusteringspark/_latestVersion)

## Available algorithms

* _emphasized algorithms_ are in scala.
* **bold algorithms** are implemented in spark.
* They can be available in **_both versions_**

### Clustering algorithms

* _Jenks Natural Breaks_
* **_K-Centers_** *
  * **_K-Means_** *
  * **_K-Modes_** *
  * **_K-Prototypes_** *
* _Tensor Biclustering algorithms_
  * _Folding-Spectral_
  * _Unfolding-Spectral_
  * _Thresholding Sum Of Squared Trajectory Length_
  * _Thresholding Individuals Trajectory Length_
  * _Recursive Biclustering_
  * _Multiple Biclustering_
* **Self Organizing Maps** ([Original project](https://github.com/TugdualSarazin/spark-clustering))
* **G-Stream** ([Original project](https://github.com/Spark-clustering-notebook/G-stream))
* **PatchWork** ([Original project](https://github.com/crim-ca/patchwork))
* _Random Local Area_ *
* **Clusterwize** 

Algorithm followed with a * implement _ClusteringAlgorithm_ trait and can be run by benchmarking classes.

### Quality Indices

Classes _ClustersIndicesAnalysisLocal_ and _ClustersIndicesAnalysisDistributed_ allow you to test following indices on every realized clustering.

* External Indices
  * **_Mutual Information_**
  * **_Normalized Mutual Information_**
* Internal Indices
  * **_Davies Bouldin_**
  * **_Ball Hall_**
  * _Silhouette_

### Preprocessing

* _Gradient Ascent_
* **_Rough Set Features Selection_**

### Clustering benchmarking and analysis

Using classes _ClusteringChainingLocal_, _BigDataClusteringChaining_, and _DistributedClusteringChaining_ you have the possibility to run multiple clustering algorithms respectively **locally and parallely**, **in a sequentially distributed way**, and **parallely on a distributed system**, generate many different vectorizations of the data whilst keeping active information on each clustering including **used vectorization, clustering model, clustering number and clustering arguments**.

Classes _ClustersIndicesAnalysisLocal_ and _ClustersIndicesAnalysisDistributed_ are devoted for clustering indices analysis.

Classes _ClustersAnalysisLocal_ and _ClustersAnalysisDistributed_ will be use to describe obtained clustering in term of distributions, proportions of categorical features...

### Incoming soon

* **Improved Spark Gradient Ascent**
* **new scalable clustering algorithms**
* **_Gaussian Mixture Models_**
* _Meta heuristic_
* **More clustering indices**


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

### Others recommendations

* It is advise to use Numeric value for the ID generic, else depending the case you will have to provide an according Ordering on ID.