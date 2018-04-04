# Clustering 4 Ever  [ ![Download](https://api.bintray.com/packages/clustering4ever/Clustering4Ever/clustering4ever/images/download.svg) ](https://bintray.com/clustering4ever/Clustering4Ever/clustering4ever/_latestVersion)

<p align="center"><img src ="https://media.giphy.com/media/2viYwU7kHW8uNmmKvd/giphy.gif" /></p>

Welcome to the LIPN Big Data Clustering Library gathering algorithms and quality indexes.

You will find additional contents about clustering algorithms [here](https://github.com/PhDStudentsP13/Clustering).

Don't hesitate to ask questions or recommendations in our [Gitter](https://gitter.im/Clustering4Ever/Lobby).

## [API documentation](http://www.beckgael.fr/doc/clustering4ever/)

## [SparkNotebook](https://github.com/spark-notebook/spark-notebook)
Basic usages of implemented algorithms are exposed with SparkNotebooks in [Spark-Clustering-Notebook](https://github.com/Spark-clustering-notebook/Clustering4Ever-Notebooks) organization.

## Include it in your project

Add following lines in your build.sbt:
* `"clustering4ever" % "clustering4ever_2.11" % "0.2.3"` to your `libraryDependencies`
* `resolvers += Resolver.bintrayRepo("clustering4ever", "Clustering4Ever")`

You can also take [specifics parts](https://bintray.com/clustering4ever/Clustering4Ever) :
* core [ ![Download](https://api.bintray.com/packages/clustering4ever/Clustering4Ever/core/images/download.svg) ](https://bintray.com/clustering4ever/Clustering4Ever/core/_latestVersion)
* clusteringscala [ ![Download](https://api.bintray.com/packages/clustering4ever/Clustering4Ever/clusteringscala/images/download.svg) ](https://bintray.com/clustering4ever/Clustering4Ever/clusteringscala/_latestVersion)
* clusteringspark[ ![Download](https://api.bintray.com/packages/clustering4ever/Clustering4Ever/clusteringspark/images/download.svg) ](https://bintray.com/clustering4ever/Clustering4Ever/clusteringspark/_latestVersion)

## Distributed algorithms, through Spark

### Clustering algorithms

#### Scalar data

##### Batch
* _K_-Means
  * Implementation allowing the choice of the dissimilarity measure.
  * Complexity **_O(k.n.t)_**
  * **Warning*** -> works only with Euclidean distance at the moment
* Self Organizing Maps
* [Mean Shift](https://github.com/beckgael/Mean-Shift-LSH)
  * Complexity
    * Initial complexity **_O(n<sup>2</sup>)_**
    * Improved complexity **_O(n)_** under some conditions

##### Streaming
* [GStream](https://github.com/Spark-clustering-notebook/G-stream)

#### Binary data
* _K_-Modes
  * Complexity **_O(k.n.t)_**
  * Implementation allowing the choice of the dissimilarity measure.
  * **Warning*** -> works only with Hamming distance at the moment

#### Mixed data
* Self Organizing Maps
  * Mixed topological Map

`*` We deliberately choose to not implement other distances than Hamming and Euclidean for Spark version of _K_-Modes and _K_-Means for reason explain in their Scala cousins versions.

### Preprocessing algorithms
* Gradient ascent
* Feature selection


## Pure Scala algorithms

### Clustering algorithms

#### A good scala clustering complementary library aka [Smile](https://haifengl.github.io/smile/clustering.html)

#### Scalar data
* [Jenks Natural Breaks](https://en.wikipedia.org/wiki/Jenks_natural_breaks_optimization)
  * A mono dimensionnal clustering
* [_K_-Means](clustering/scala/src/main/scala/K-Means/README.md)
  * Complexity **_O(k.n.t)_**
  * Implementation allowing the choice of the dissimilarity measure.
  * **Warning** -> with another distance than Euclidean, similarity matrix in _O(n<sup>2</sup>)_ of each cluster is computed to find the best prototype, depending on cluster size it can becomes way slower than Euclidean

#### Binary data
* _K_-Modes
  * Complexity **_O(k.n.t)_**
  * Implementation allowing the choice of the dissimilarity measure.
  * **Warning** -> with another distance than Hamming, similarity matrix in _O(n<sup>2</sup>)_ of each cluster is computed to find the best prototype, depending on cluster size it can becomes way slower than Hamming

## [Quality Indexes](Documentation/doc/QualityIndexes.md)

#### External indexes
* Mutual Information (scala & spark)
* Normalized Mutual Information (scala & spark)

#### Internal indexes
* Davies Bouldin (scala)
* Silhouette (scala)