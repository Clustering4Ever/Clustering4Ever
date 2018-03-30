# Clustering 4 Ever  [ ![Download](https://api.bintray.com/packages/clustering4ever/Clustering4Ever/clustering4ever/images/download.svg) ](https://bintray.com/clustering4ever/Clustering4Ever/clustering4ever/_latestVersion)

Welcome to the LIPN Big Data Clustering Library gathering algorithms and quality indexes.

You will find additional contents about clustering algorithms [here](https://github.com/PhDStudentsP13/Clustering).

## [Documentation](http://www.beckgael.fr/doc/clustering4ever/#package)

## Include it in your project

Add `"clustering4ever" % "clustering4ever_2.11" % "0.1.3"` into your `libraryDependencies` in your build.sbt.
You can also take [specifics parts](https://bintray.com/clustering4ever/Clustering4Ever) :
* `core`
* `clusteringscala` 
* `clusteringspark`
* `qualitymeasure`

## Distributed algorithms

### Clustering algorithms

#### Scalar data

##### Batch
* Self Organizing Maps
* Mean Shift

##### Streaming
* GStream

### Binary data
* _K_-Modes

### Preprocessing algorithms
* Gradient ascent
* Feature selection

## Pure Scala algorithms

### Clustering algorithms

#### Scalar data
* [_K_-Means](clustering/scala/src/main/scala/K-Means/README.md), a _K_-Means implementation allowing the choice of the dissimilarity measure.

#### Binary data
* _K_-Modes

### [Quality Indexes](qualityMeasure/README.md)


## [SparkNotebook](https://github.com/spark-notebook/spark-notebook)
Basic usages of implemented algorithms are exposed with SparkNotebooks under [SparkNotebooks](SparkNotebooks) directory.