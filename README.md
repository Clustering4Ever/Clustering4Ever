# Clustering 4 Ever  [ ![Download](https://api.bintray.com/packages/clustering4ever/C4E/clustering4ever/images/download.svg) ](https://bintray.com/clustering4ever/C4E/clustering4ever/_latestVersion) [![Maven Central](https://img.shields.io/maven-central/v/org.clustering4ever/clustering4ever_2.11.svg?label=Maven%20Central)](https://search.maven.org/search?q=g:%22org.clustering4ever%22%20AND%20a:%22clustering4ever_2.11%22)

**Welcome** to Big Data Clustering Library gathering clustering algorithms and quality indexes. Don't hesitate to check our **[Wiki](https://github.com/Clustering4Ever/Clustering4Ever/wiki)**, ask questions or make recommendations in our **[Gitter](https://gitter.im/Clustering4Ever/Lobby)**.
You will find additional contents about clustering algorithms **[here](https://github.com/PhDStudentsP13/Clustering)**.
<p align="center"><img src ="https://media.giphy.com/media/2viYwU7kHW8uNmmKvd/giphy.gif" /></p>

## [API documentation](http://www.clustering4ever.org/API%20Documentation/)

## Include it in your project

Add following line in your build.sbt :

* `"org.clustering4ever" % "clustering4ever_2.11" % "0.7.3"` to your `libraryDependencies`

Eventually add this resolver :

* `resolvers += Resolver.bintrayRepo("clustering4ever", "C4E")`

You can also take [specifics parts](https://bintray.com/clustering4ever/C4E) :

* Core [ ![Download](https://api.bintray.com/packages/clustering4ever/C4E/core/images/download.svg) ](https://bintray.com/clustering4ever/C4E/core/_latestVersion)
* Scala Clustering [ ![Download](https://api.bintray.com/packages/clustering4ever/C4E/clusteringscala/images/download.svg) ](https://bintray.com/clustering4ever/C4E/clusteringscala/_latestVersion)
* Spark Clustering [ ![Download](https://api.bintray.com/packages/clustering4ever/C4E/clusteringspark/images/download.svg) ](https://bintray.com/clustering4ever/C4E/clusteringspark/_latestVersion)

## Citation

If you publish material based on informations obtained from this repository, then, in your acknowledgements, please note the assistance you received by using this community work. This will help others to obtain the same informations and **replicate your experiments**, because having results is cool but being able to compare to others is better.
Citation: `@misc{C4E, url = “https://github.com/Clustering4Ever/Clustering4Ever“, institution = “Paris 13 University, LIPN UMR CNRS 7030”}`

## Available algorithms

* _emphasized algorithms_ are in scala.
* **bold algorithms** are implemented in spark.
* They can be available in **_both versions_**

### Clustering algorithms

* _Jenks Natural Breaks_
* **_K-Means_**
* **_K-Modes_**
* **_K-Prototypes_**
* _Tensor Biclustering_
* _Mean Shift Gradient Ascent_
* **[Self Organizing Maps](https://github.com/TugdualSarazin/spark-clustering)**
* **[G-Stream](https://github.com/Spark-clustering-notebook/G-stream)**
* **[PatchWork](https://github.com/crim-ca/patchwork)**
* _Random Local Area_
* **Clusterwize** 

### Quality indexes

* **_Mutual Information_**
* **_Normalized Mutual Information_**
* **_Davies Bouldin_**
* _Silhouette_
* **_Ball Hall_**

### Features selection

* **_Rough Set_**

## C4E-Notebook examples

Basic usages of implemented algorithms are exposed with SparkNotebooks in [Spark-Clustering-Notebook](https://github.com/Spark-clustering-notebook/Clustering4Ever-Notebooks) organization.

## Miscellaneous

### Helper functions to generate Clusterizable collections 

You can generate your collections with basic Clusterizable using helpers in `org.clustering4ever.util.{ClusterizableGenerator, ScalaImplicits, SparkImplicits}`

### [References](https://github.com/Clustering4Ever/Clustering4Ever/wiki/References)

### Incoming soon 

* **Improved Spark Mean Shift**
* new scalable clustering algorithms
* Gaussian Mixture Models
* Meta heuristic

### What data structures are recommended for best performances

* ArrayBuffer as vector are a good start
* ArrayBuffer or ParArray as vector containers are also recommended
