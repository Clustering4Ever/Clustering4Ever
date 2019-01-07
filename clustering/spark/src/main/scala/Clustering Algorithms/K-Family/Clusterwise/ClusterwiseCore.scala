package org.clustering4ever.spark.clustering.clusterwise

import scala.language.higherKinds
import scala.util.Random
import scala.util.control.Breaks._
import scala.collection.{immutable, mutable, GenSeq}
import scala.collection.parallel.mutable.ParArray

class ClusterwiseCore[V <: Seq[Double]](
	val dsXYTrain: GenSeq[(Int, (V, V))],
	val h: Int,
	val g: Int,
	val nbMaxAttemps: Int,
	logOn: Boolean = false
)  extends ClusterwiseTypes[V] with Serializable {
	val rangeOverClasses = (0 until g).toArray

	private[this] def removeLastXY(clusterID: Int, inputX: IDXDS, inputY: YDS) = {
	  	inputX(clusterID).remove( inputX(clusterID).size - 1 )	
	  	inputY(clusterID).remove( inputY(clusterID).size - 1 )
	}

	private[this] def posInClassForMovingPoints(currClass: Int, elemNb: Int, classlimits: Array[Int]) = if( currClass == 0 ) elemNb else elemNb - classlimits( currClass - 1 ) - 1

	private[this] def removeFirstElemXY(clusterID: Int, xDS: IDXDS, yDS: YDS) = {
		xDS(clusterID).remove(0)
		yDS(clusterID).remove(0)
	}

	private[this] def prepareMovingPoint(classedDS: ClassedDS, xDS: IDXDS, yDS: YDS, g: Int, elemNb: Int, currClass: Int, classlimits: Array[Int]) =
	{
		val posInClass = posInClassForMovingPoints(currClass, elemNb, classlimits)
		val (elemToReplaceID, (elemToReplaceX, elemToReplaceY, _)) = classedDS(currClass)._2(posInClass)
		(0 until g).foreach{ j =>
		  if( j == currClass ) removeFirstElemXY(j, xDS, yDS)
		  else {
		    xDS(j) += ( (elemToReplaceID, elemToReplaceX) )
		    yDS(j) += elemToReplaceY
		  }
		}
	}

	private[this] def prepareMovingPointByGroup(
		classedDS: ClassedDSperGrp,
		xDS: IDXDS,
		yDS: YDS,
		g: Int,
		elemNb: Int,
		currClass: Int,
		classlimits: Array[Int],
		orderedBucketSize: Array[Int]
	) =
	{
		val posInClass = posInClassForMovingPoints(currClass, elemNb, classlimits)
		val elemToReplace = classedDS(currClass)._2(posInClass)._3
		for( j <- 0 until g ) {
		  if( j == currClass ) for( i <- 0 until orderedBucketSize(elemNb) ) removeFirstElemXY(j, xDS, yDS)
		  else {
		    xDS(j) ++= elemToReplace.map{ case(grpId, id, x, y) => (id, x) }
		    yDS(j) ++= elemToReplace.map{ case(grpId, id, x, y) => y }
		  }
		}
	}


	private[this] def elseCaseWhenComputingError(errorsIndices: Array[((Double, Double), Int)], boolTab: Array[Boolean], currentClass: Int) = {
		var b = true
		errorsIndices.map{ case ((err1, err2), idx) =>
			if( idx == currentClass ) err2
			else {
	  			if( boolTab(idx) && b ) {
	  				boolTab(idx) = false
	  				b = false
	  				err2
	  			}
	  			else err1
			}
		}
	}


	def plsPerDot() =
	{
		var continue = true
		var cptAttemps = 0
		var classOfEachData = Array.empty[(Int, Int)]
		var dsPerClassF = Array.empty[DSPerClass]
		var regressionPerClassFinal = Array.empty[RegPerClass]
	  	val regressionCriteriaMap = mutable.HashMap.empty[Int, Double]

		do
		{
			try
			{
				cptAttemps += 1
			  	val classedDS: Array[(Int, (V, V, Int))] = dsXYTrain.map{ case (id, (x, y)) => (id, (x, y, Random.nextInt(g))) }.seq.sortBy{ case (id, (x, y, clusterID)) => clusterID }.toArray
			  	val valuesToBrowse = classedDS.map{  case (id, (x, y, clusterID)) => (id, clusterID) }
				val dsPerClass = classedDS.groupBy{ case (id, (x, y, clusterID)) => clusterID }.toArray.sortBy{ case (clusterID, _) => clusterID }
			  	val inputX = dsPerClass.map{ case (clusterID, idXYClass) => mutable.ArrayBuffer(idXYClass.map{ case (id, (x, y, clusterID))  => (id, x) }:_*) }
			  	val inputY = dsPerClass.map{ case (clusterID, idXYClass) => mutable.ArrayBuffer(idXYClass.map{ case (id, (x, y, clusterID)) => y }:_*) }
			  	val preClassLimits = (0 until inputY.size).toArray.map(inputY(_).size)
			  	val classlimits = (0 until preClassLimits.size).toArray.map( i => (0 to i).map(preClassLimits(_)).sum - 1 )
			  	var currentDotIdx = 0
			  	var currentClass = 0
			  	var nbIte = 0
			  	val stop = valuesToBrowse.size - 1

		  		breakable {
			  		if( inputX.size != g || inputX.exists(_.isEmpty) ) break
				  	
				  	while( continue && nbIte != stop )
				  	{
					  	val (currentDotId, currentDotClass) = valuesToBrowse(nbIte)

					  	val regPerClass = rangeOverClasses.map( i => PLS.runClusterwisePLS(inputX, inputY, i, h) )

					  	if( ! regPerClass.map(_._1).filter(_.isNaN).isEmpty ) break

					  	val error1 = regPerClass.map(_._1)
					  	prepareMovingPoint(dsPerClass, inputX, inputY, g, currentDotIdx, currentClass, classlimits)
					  	val regPerClass2 = try rangeOverClasses.map( i => PLS.runClusterwisePLS(inputX, inputY, i, h) )
				  		catch {
				  			case emptyClass : java.lang.IndexOutOfBoundsException => Array.empty[RegPerClass]
				  		}
					  	
					  	if( regPerClass2.isEmpty ) break

					  	val error2 = regPerClass2.map(_._1)
					  	val boolTab = Array.fill(g)(true)
					  	val errorsIndices = error1.zip(error2).zipWithIndex
					  	boolTab(currentDotClass) = false
					  	val errors = rangeOverClasses.map( i => (if( i == currentDotClass ) errorsIndices.map{ case ((err1, err2), idx) => err1 } else elseCaseWhenComputingError(errorsIndices, boolTab, currentDotClass)).sum )
					  	val minError = errors.min
					  	val classToMovePointInto = errors.indexOf(minError)
					  	val (pointID, (pointX, pointY, _)) = classedDS(currentDotIdx)
					  	if( classToMovePointInto != currentDotClass ) {
						  	classedDS(currentDotIdx) = (pointID, (pointX, pointY, classToMovePointInto))
						  	val classWithoutDot = rangeOverClasses.filter( clusterID => clusterID != classToMovePointInto && clusterID != currentDotClass)
						  	for( j <- classWithoutDot ) removeLastXY(j, inputX, inputY)
					  	}
					  	else {
						  	val classWithoutDot = rangeOverClasses.filter(_ != currentDotClass)
						  	for( j <- classWithoutDot ) removeLastXY(j, inputX, inputY)
							inputX(currentDotClass) += ((pointID, pointX))
							inputY(currentDotClass) += pointY
					  	}
					  	continue = inputX.filter(_.isEmpty).isEmpty
					  	regressionCriteriaMap += ( currentDotId -> minError )
					  	nbIte += 1
				  		currentDotIdx += 1
				  		if( currentDotIdx > classlimits(currentClass) ) currentClass += 1
				  	}
			  	}
			  	continue = nbIte != stop
			  	if( continue ) regressionCriteriaMap.clear
			  	else {
					dsPerClassF = rangeOverClasses.map( i => classedDS.filter{ case (_, (_, _, clusterID)) => clusterID == i } )
					regressionPerClassFinal = rangeOverClasses.map( i => PLS.runClusterwisePLS(inputX, inputY, i, h) )
					classOfEachData = classedDS.map{ case (id, (_, _, clusterID)) => (id, clusterID) }	
			  	}
			}
			catch { 
				case svdExcept : breeze.linalg.NotConvergedException => {
					regressionCriteriaMap.clear
					if( logOn ) println("\nThere was an Singular Value Decomposition Issue, retry with new initialisation")	
				}
			}
		}
		while( continue && cptAttemps < nbMaxAttemps )

		if( continue && cptAttemps == nbMaxAttemps ) throw new Exception("There was too many unsuccesufull attemps due to empty classes, try to diminish number of class ")

  		val regressionResults: Array[Double] = regressionPerClassFinal.map{ case (ist, _, _, _) => ist } 
	  	val coXYcoef: Array[Array[Double]] = regressionPerClassFinal.map{ case (_, r, _, _) => r.toArray }
	  	val coIntercept: Array[Array[Double]] = regressionPerClassFinal.map{ case (_, _, co, _) => co }
	  	val predFitted: Array[immutable.IndexedSeq[(Int, Array[Double])]] = regressionPerClassFinal.map{ case (_, _, _, fth) => fth } 

	  	(dsPerClassF, predFitted, coIntercept, coXYcoef, regressionResults, regressionCriteriaMap, classOfEachData)

	}

	def plsPerGroup(allGroupedData: immutable.HashMap[Int, Int], microClusterNumber: Int) = {
		var continue = true
		val microClusterNumberRange = (0 until microClusterNumber).toVector
		var cptAttemps = 0
		var classOfEachData = Array.empty[(Int, Int)]
		var dsPerClassF = Array.empty[ClassedDSperGrp]
		var regressionPerClassFinal = Array.empty[RegPerClass]
	  	val regressionCriteriaMap = mutable.HashMap.empty[Int, Double]
	  	do {
			try {
		  		cptAttemps += 1
			  	// Initialisation par groupe
			  	val perGroupClassInit = microClusterNumberRange.map( x => Random.nextInt(g) )

			  	val dsPerClassPerBucket = dsXYTrain.map{ case (id, (x, y)) => (allGroupedData(id), id, x, y) }.toArray
					.groupBy(_._1)
					.toArray
					.map{ case (microClusterID, aggregate) => (perGroupClassInit(microClusterID), microClusterID, aggregate) }
					.groupBy{ case (clusterID, _, _) => clusterID }
					.toArray
					.sortBy{ case (clusterID, _) => clusterID }

				val bucketOrderPerClass = dsPerClassPerBucket.map{ case (clusterID, buckets) => ((clusterID, buckets.map{ case (clusterID, grpId, grpIdIdXY) => grpId })) }

				val indexedFlatBucketOrder = bucketOrderPerClass.flatMap{ case (clusterID, grpIds) => grpIds.map( grpId => (grpId, clusterID) ) }.zipWithIndex

				val preSize = dsPerClassPerBucket.map{ case (clusterID, dsPerBucket) => dsPerBucket.map{ case (clusterID, grpId, ds) => ds.size } }
				val orderedBucketSize = preSize.flatten
				val classSize = preSize.map(_.size )
				val classlimits = (0 until classSize.size).toArray.map( i => (0 to i).map(classSize(_)).sum - 1 )

			  	val inputX = dsPerClassPerBucket.map{ case (clusterID, dsPerBucket) => mutable.ArrayBuffer(dsPerBucket.flatMap{ case (clusterID, grpId, ds) => ds.map{ case (grpId, id, x, y) => (id, x) } }:_*) }
			  	val inputY = dsPerClassPerBucket.map{ case (clusterID, dsPerBucket) => mutable.ArrayBuffer(dsPerBucket.flatMap{ case (clusterID, grpId, ds) => ds.map{ case (grpId, id, x, y) => y } }:_*) }

			  	var currentDotsGrpIdx = 0
			  	var currentClass = 0
			  	var nbIte = 0
				val stop = indexedFlatBucketOrder.size - 1

		  		breakable {
			  		// if init starts with empty classes, retry
			  		if( inputX.size != g || inputX.exists(_.isEmpty) ) break

				  	while( continue && nbIte != stop ) {
				  		val ((grpId, currentclusterID), currentIdx) = indexedFlatBucketOrder(nbIte)
				  	
					  	// Regression with Point inside one Class and not the Rest
					  	val regPerClass = rangeOverClasses.map( i => PLS.runClusterwisePLS(inputX, inputY, i, h) )
					  	val error1 = regPerClass.map(_._1)
					  	prepareMovingPointByGroup(dsPerClassPerBucket, inputX, inputY, g, currentDotsGrpIdx, currentClass, classlimits, orderedBucketSize)
					  	if( inputX.exists(_.isEmpty) ) {
					  		if( logOn ) println("Problemo one class is becoming empty")
					  		break
					  	}
					  	// Regression with Point inside all other Class and not the former
					  	val regPerClass2 = {
					  		try rangeOverClasses.map( i => PLS.runClusterwisePLS(inputX, inputY, i, h) )
					  		catch {
					  			case emptyClass : java.lang.IndexOutOfBoundsException => Array.empty[RegPerClass]
					  		}
					  	}

					  	if( regPerClass2.isEmpty ) break
			  	  		val error2 = regPerClass2.map(_._1)
					  	val boolTab = Array.fill(g)(true)
					  	val errorsIndices = error1.zip(error2).zipWithIndex

					  	boolTab(currentclusterID) = false
					  	val errors = rangeOverClasses.map( i => (if( i == currentclusterID ) errorsIndices.map{ case ((err1, err2), idx) => err1 } else elseCaseWhenComputingError(errorsIndices, boolTab, currentclusterID)).sum )
					  	val minError = errors.min
					  	val classToMoveGroupInto = errors.indexOf(minError)
					  	val posInClass = posInClassForMovingPoints(currentClass, currentDotsGrpIdx, classlimits)
					  	val (_, bucketIDtoUpdate, grpIdIDXY) = dsPerClassPerBucket(currentClass)._2(posInClass)
					  	if( classToMoveGroupInto != currentclusterID ) {
						  	dsPerClassPerBucket(currentClass)._2(posInClass) = (classToMoveGroupInto, bucketIDtoUpdate, grpIdIDXY)
						  	val classWithoutDots = rangeOverClasses.filter( clusterID => clusterID != classToMoveGroupInto && clusterID != currentclusterID)
						  	val rangeIn = (0 until orderedBucketSize(currentDotsGrpIdx))
						  	classWithoutDots.foreach( j => rangeIn.foreach( i => removeLastXY(j, inputX, inputY) ) )
					  	}
					  	else {
						  	val classWithoutDots = rangeOverClasses.filter(_ != currentclusterID)
						  	val rangeIn = (0 until orderedBucketSize(currentDotsGrpIdx))
						  	classWithoutDots.foreach( j => rangeIn.foreach( i => removeLastXY(j, inputX, inputY) ) )
						  	inputX(currentclusterID) ++= grpIdIDXY.map{ case (grpId, id, x, y) => (id, x) }
						  	inputY(currentclusterID) ++= grpIdIDXY.map{ case (grpId, id, x, y) => y }
					  	}
					  	regressionCriteriaMap += ( currentIdx -> minError )
					  	continue = inputX.filter(_.isEmpty).isEmpty
						nbIte += 1
				  		currentDotsGrpIdx += 1
				  		if( currentDotsGrpIdx > classlimits(currentClass) ) currentClass += 1
			  		}
			  	}
			  	continue = nbIte != stop
			  	if( continue ) regressionCriteriaMap.clear
			  	else {
			  		dsPerClassF = rangeOverClasses.map( i => dsPerClassPerBucket.filter{ case (clusterID, _) => clusterID == i } )
					regressionPerClassFinal = rangeOverClasses.map( i => PLS.runClusterwisePLS(inputX, inputY, i, h) )
			  		classOfEachData = dsPerClassPerBucket.flatMap{ case (clusterID, dsPerBucket) => dsPerBucket.flatMap{ case (_, grpId, ds) => ds.map{ case (_, id, x, y) => (id, clusterID) } } }
			  	}
			}
			catch {
				case svdExcept : breeze.linalg.NotConvergedException => {
					regressionCriteriaMap.clear
					if( logOn ) println("\nThere was an Singular Value Decomposition Issue, retry with new initialisation")
				}
			}
		}
		while( continue && cptAttemps < nbMaxAttemps )

		if( continue && cptAttemps == nbMaxAttemps ) throw new Exception("There was too many unsuccesufull attemps due to empty classes, try to diminish number of class or size of blocs")

  		val regressionResults: Array[Double] = regressionPerClassFinal.map{ case (ist, _, _, _) => ist } 
	  	val coXYcoef: Array[Array[Double]] = regressionPerClassFinal.map{ case (_, r, _, _) => r.toArray }
	  	val coIntercept: Array[Array[Double]] = regressionPerClassFinal.map{ case (_, _, co, _) => co }
	  	val predFitted: Array[immutable.IndexedSeq[(Int, Array[Double])]] = regressionPerClassFinal.map{ case (_, _, _, fth) => fth } 

	  	(dsPerClassF, predFitted, coIntercept, coXYcoef, regressionResults, regressionCriteriaMap, classOfEachData)
	}
}

object ClusterwiseCore extends Serializable {

	def plsPerDot[V <: Seq[Double]](
		dsXYTrain: GenSeq[(Int, (V, V))],
		h: Int,
		g: Int,
		nbMaxAttemps: Int = 30,
		logOn: Boolean = false
	) =	{
		val oneClusterwise = new ClusterwiseCore(dsXYTrain, h, g, nbMaxAttemps, logOn)
		val (dsPerClass, predFitted, coIntercept, coXYcoef, critReg, mapsRegCrit, classedReg) = oneClusterwise.plsPerDot()
		(dsPerClass, predFitted, coIntercept, coXYcoef, critReg, mapsRegCrit, classedReg)
	}

	def plsPerMicroClusters[V <: Seq[Double]](
		dsXYTrain: GenSeq[(Int, (V, V))],
		allGroupedData: immutable.HashMap[Int, Int],
		h: Int,
		g: Int,
		microClusterNumber: Int,
		nbMaxAttemps: Int = 30,
		logOn: Boolean = false
	) =	{
		val oneClusterwise = new ClusterwiseCore(dsXYTrain, h, g, nbMaxAttemps, logOn)
		val (dsPerClass, predFitted, coIntercept, coXYcoef, critReg, mapsRegCrit, classedReg) = oneClusterwise.plsPerGroup(allGroupedData, microClusterNumber)
		(dsPerClass, predFitted, coIntercept, coXYcoef, critReg, mapsRegCrit, classedReg)
	}
} 