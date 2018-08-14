# Clustering 4 Ever  [ ![Download](https://api.bintray.com/packages/clustering4ever/Clustering4Ever/clustering4ever/images/download.svg) ](https://bintray.com/clustering4ever/Clustering4Ever/clustering4ever/_latestVersion)

**Welcome** to Big Data Clustering Library gathering clustering algorithms and quality indexes. Don't hesitate to ask questions or make recommendations in our [Gitter](https://gitter.im/Clustering4Ever/Lobby).
You will find additional contents about clustering algorithms [here](https://github.com/PhDStudentsP13/Clustering).
<p align="center"><img src ="https://media.giphy.com/media/2viYwU7kHW8uNmmKvd/giphy.gif" /></p>

## Citation
If you publish material based on informations obtained from this repository, then, in your acknowledgements, please note the assistance you received by using this community work. This will help others to obtain the same informations and **replicate your experiments**, because having results is cool but being able to compare to others is better.
Citation: `@misc{C4E, url = “https://github.com/Clustering4Ever/Clustering4Ever“, institution = “Paris 13 University, LIPN UMR CNRS 7030”}`

## C4E-Notebook examples
Basic usages of implemented algorithms are exposed with SparkNotebooks in [Spark-Clustering-Notebook](https://github.com/Spark-clustering-notebook/Clustering4Ever-Notebooks) organization.

## Include it in your project

Add following lines in your build.sbt :
* `"clustering4ever" % "clustering4ever_2.11" % "0.5.1"` to your `libraryDependencies`
* `resolvers += Resolver.bintrayRepo("clustering4ever", "Clustering4Ever")`

You can also take [specifics parts](https://bintray.com/clustering4ever/Clustering4Ever) :
* Core [ ![Download](https://api.bintray.com/packages/clustering4ever/Clustering4Ever/core/images/download.svg) ](https://bintray.com/clustering4ever/Clustering4Ever/core/_latestVersion)
* Scala Clustering [ ![Download](https://api.bintray.com/packages/clustering4ever/Clustering4Ever/clusteringscala/images/download.svg) ](https://bintray.com/clustering4ever/Clustering4Ever/clusteringscala/_latestVersion)
* Spark Clustering[ ![Download](https://api.bintray.com/packages/clustering4ever/Clustering4Ever/clusteringspark/images/download.svg) ](https://bintray.com/clustering4ever/Clustering4Ever/clusteringspark/_latestVersion)

## [API documentation](http://clustering4ever.org/API%20Documentation/)

## Incoming soon 
* Improved Spark Mean Shift
* 2 new scalable clustering algorithms
* [G-Stream](https://github.com/Spark-clustering-notebook/G-stream)

## [References](https://github.com/Clustering4Ever/Clustering4Ever/wiki/References)

## Scalable algorithms, through Spark

### Clustering algorithms

#### Scalar data

##### Batch
* _K_-Means
  * Implementation that will allow choice of the dissimilarity measure.
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
  * Implementation that will allow choice of the dissimilarity measure.
  * **Warning*** -> works only with Hamming distance at the moment

#### Mixed data
* _K_-Protoypes
  * Complexity **_O(k.n.t)_**
  * **Warning*** -> works only with Hamming and Euclidean distance at the moment
* Self Organizing Maps
  * Mixed topological Map

`*` We deliberately choose to not implement other distances than Hamming and Euclidean for Spark version of _K_-Modes and _K_-Means for reason explain in their Scala cousins versions.

### Preprocessing algorithms
* Gradient ascent (Euclidean)
* Feature selection


## Pure Scala algorithms

### Clustering algorithms

#### An excelent machine learning scala/java library which keeps your [Smile](https://haifengl.github.io/smile/clustering.html)

#### Scalar data
* [Jenks Natural Breaks](https://en.wikipedia.org/wiki/Jenks_natural_breaks_optimization)
  * A mono dimensionnal clustering
* [_K_-Means](clustering/scala/src/main/scala/K-Means/README.md)
  * Complexity **_O(k.n.t)_**
  * Implementation allowing the choice of the dissimilarity measure.
  * **Warning** -> with another distance than Euclidean, similarity matrix in _O(n<sup>2</sup>)_ of each cluster is computed to find the best prototype, depending on cluster size it can becomes way slower than Euclidean
* [Gaussian Mixture](https://en.wikipedia.org/wiki/Mixture_model)
  * Complexity **_O(k.n.t)_**

#### Binary data
* _K_-Modes
  * Complexity **_O(k.n.t)_**
  * Implementation allowing the choice of the dissimilarity measure.
  * **Warning** -> with another distance than Hamming, similarity matrix in _O(n<sup>2</sup>)_ of each cluster is computed to find the best prototype, depending on cluster size it can becomes way slower than Hamming

#### Mixed data
* _K_-Protoypes
  * Complexity **_O(k.n.t)_**
  * **Warning*** -> works only with Hamming and Euclidean distance at the moment

### Preprocessing algorithms
* Gradient ascent
    * **Warning**, there's no theorical guaranty with other distance than Euclidean
    * 4 kernels
      * K Nearest Neighbors
      * Gaussian
      * Flat
      * Sigmoid

## [Quality Indexes](Documentation/doc/QualityIndexes.md)

#### External indexes
* Mutual Information (scala & spark)
* Normalized Mutual Information (scala & spark)

#### Internal indexes
* Davies Bouldin (scala & spark)
* Ball-Hall (scala & spark)
* Silhouette (scala)


## Miscellaneous

### Implicit conversion

We decided to feed algorithms with Clusterizable type object to ensure to keep original nature of data and transform them into computable vector when needed. In order to stay simple we provide implicit conversion that can be called through following lines:
* `import clustering4ever.util.ScalaImplicits._`
* `import clustering4ever.util.SparkImplicits._`

They allow you to pass directly to our `data` algorithms argument as either `GenSeq` or `RDD` of `immutable.Seq[Int]` or `immutable.Seq[Double]` that will be transformed into the appropriate input format.