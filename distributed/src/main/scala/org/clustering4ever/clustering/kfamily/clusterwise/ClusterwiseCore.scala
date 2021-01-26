package org.clustering4ever.clustering.kfamily.clusterwise

import scala.collection.{GenSeq, immutable, mutable}
import scala.language.higherKinds
import scala.util.Random
import scala.util.control.Breaks._

final case class ClusterwiseCore(
	final val dsXYTrain: GenSeq[(Int, (Array[Double], Array[Double]))],
	final val h: Int,
	final val g: Int,
	final val nbMaxAttemps: Int,
	logOn: Boolean = false
)  extends ClusterwiseTypes with Serializable {

	private[clusterwise] val rangeOverClasses = (0 until g).toArray

	private[clusterwise] def removeLastXY(clusterID: Int, inputX: IDXDS, inputY: YDS): Unit = {
	  	inputX(clusterID).remove(inputX(clusterID).size - 1)	
	  	inputY(clusterID).remove(inputY(clusterID).size - 1)
	}

	private[clusterwise] def posInClassForMovingPoints(currClass: Int, elemNb: Int, classlimits: Array[Int]): Int = {
		if (currClass == 0) elemNb
		else elemNb - classlimits(currClass - 1) - 1
	}

	private[clusterwise] def removeFirstElemXY(clusterID: Int, xDS: IDXDS, yDS: YDS): Unit = {
		xDS(clusterID).remove(0)
		yDS(clusterID).remove(0)
	}

	private[clusterwise] def prepareMovingPoint(classedDS: ClassedDS, xDS: IDXDS, yDS: YDS, g: Int, elemNb: Int, currClass: Int, classlimits: Array[Int]): Unit = {
		val posInClass = posInClassForMovingPoints(currClass, elemNb, classlimits)
		val (elemToReplaceID, (elemToReplaceX, elemToReplaceY, _)) = classedDS(currClass)._2(posInClass)
		(0 until g).foreach{ j =>
		  if (j == currClass) {
		  	removeFirstElemXY(j, xDS, yDS)
		  }
		  else {
		    xDS(j) += ((elemToReplaceID, elemToReplaceX))
		    yDS(j) += elemToReplaceY
		  }
		}
	}

	private[clusterwise] def prepareMovingPointByGroup(
		classedDS: ClassedDSperGrp,
		xDS: IDXDS,
		yDS: YDS,
		g: Int,
		elemNb: Int,
		currClass: Int,
		classlimits: Array[Int],
		orderedBucketSize: Array[Int]
	): Unit = {
		val posInClass = posInClassForMovingPoints(currClass, elemNb, classlimits)

		val (_, ds) = classedDS(currClass)
		val elemToReplace = ds(posInClass)._3

		rangeOverClasses.foreach{ j =>
		  if (j == currClass) for (i <- 0 until orderedBucketSize(elemNb)) {
		  	removeFirstElemXY(j, xDS, yDS)
		  }
		  else {
		    xDS(j) ++= elemToReplace.map{ case (grpId, id, x, y) => (id, x) }
		    yDS(j) ++= elemToReplace.map{ case (grpId, id, x, y) => y }
		  }
		}
	}


	private[clusterwise] def elseCaseWhenComputingError(errorsIndices: Array[((Double, Double), Int)], boolTab: Array[Boolean], currentClass: Int): Array[Double] = {

		@annotation.tailrec
		def go(l: List[((Double, Double), Int)], errorsOut: List[Double], bool: Boolean): Array[Double] = {
			l match {
				case Nil => errorsOut.reverse.toArray
				case ((err1, err2), idx) :: xs => {
					if (idx == currentClass) go(xs, err2 :: errorsOut, bool)
					else {
			  			if (boolTab(idx) && bool) {
			  				boolTab(idx) = false
			  				go(xs, err2 :: errorsOut, false)
			  			}
			  			else go(xs, err1 :: errorsOut, bool)
					}
				}
			}
		}
		go(errorsIndices.toList, List.empty[Double], true)

	}

	private[clusterwise] def computeErrors(errorsIndices: Array[((Double, Double), Int)], boolTab: Array[Boolean], currentClusterID: Int): Array[Double] = {

	  	val errors = rangeOverClasses.map{ i =>
	  		val toSum = if (i == currentClusterID) {
	  			errorsIndices.map{ case ((err1, err2), idx) => err1 }
	  		}
	  		else {
	  			elseCaseWhenComputingError(errorsIndices, boolTab, currentClusterID)
			}
			toSum.sum
		}

		errors

	}


	def plsPerDot(): (Array[DSPerClass], Array[Array[(Int, Array[Double])]], Array[Array[Double]], Array[Array[Double]], Array[Double], mutable.HashMap[Int, Double], Array[(Int, Int)]) = {

		var continue = true
		var cptAttemps = 0
		var classOfEachData = Array.empty[(Int, Int)]
		var dsPerClassF = Array.empty[DSPerClass]
		var regressionPerClassFinal = Array.empty[RegPerClass]
	  	val regressionCriteriaMap = mutable.HashMap.empty[Int, Double]

	  	// tailrec
		do {
			try {
				cptAttemps += 1
			  	val classedDS: Array[(Int, (Array[Double], Array[Double], Int))] = dsXYTrain.map{ case (id, (x, y)) =>
			  		(id, (x, y, Random.nextInt(g)))
			  	}
			  	.seq
				.sortBy{ case (id, (x, y, clusterID)) => clusterID }
				.toArray
			
			  	val valuesToBrowse = classedDS.map{  case (id, (x, y, clusterID)) => (id, clusterID) }
			
				val dsPerClass = classedDS.groupBy{ case (id, (x, y, clusterID)) => clusterID }
					.toArray
					.sortBy{ case (clusterID, _) => clusterID }
			  
			  	val inputX = dsPerClass.map{ case (clusterID, idXYClass) =>
			  		val toBuff = idXYClass.map{ case (id, (x, y, clusterID))  => (id, x) }
			  		mutable.ArrayBuffer(toBuff:_*)
			  	}
			  
			  	val inputY = dsPerClass.map{ case (clusterID, idXYClass) =>
				  	val toBuff = idXYClass.map{ case (id, (x, y, clusterID)) => y }
			  		mutable.ArrayBuffer(toBuff:_*)
			  	}
			  
			  	val preClassLimits = inputY.indices.toArray.map(inputY(_).size)
			  	val classlimits = preClassLimits.indices.toArray.map{ i =>
			  		(0 to i).foldLeft(0)(_ + preClassLimits(_)) - 1
			  	}
			  
			  	var currentDotIdx = 0
			  	var currentClass = 0
			  
			  	var nbIte = 0
			  	val stop = valuesToBrowse.length - 1

			  	// tailrec
		  		breakable {
			  		if (inputX.length != g || inputX.exists(_.isEmpty)) break
				  	
				  	while (continue && nbIte != stop) {

					  	val (currentDotId, currentClusterID) = valuesToBrowse(nbIte)

					  	val regPerClass = rangeOverClasses.map(PLS.runClusterwisePLS(inputX, inputY, _, h))

					  	if (regPerClass.exists(_._1.isNaN)) break

					  	val error1 = regPerClass.map(_._1)
					  	prepareMovingPoint(dsPerClass, inputX, inputY, g, currentDotIdx, currentClass, classlimits)
					  	val regPerClass2 = try rangeOverClasses.map(PLS.runClusterwisePLS(inputX, inputY, _, h))
				  		catch {
				  			case emptyClass : java.lang.IndexOutOfBoundsException => Array.empty[RegPerClass]
				  		}
					  	
					  	if (regPerClass2.isEmpty) break

					  	val error2 = regPerClass2.map(_._1)
					  
					  	val boolTab = Array.fill(g)(true)
					  
					  	val errorsIndices = error1.zip(error2).zipWithIndex
					  
					  	boolTab(currentClusterID) = false
					  
					  	val errors = computeErrors(errorsIndices, boolTab, currentClusterID)

					  	val minError = errors.min
					  
					  	val classToMovePointInto = errors.indexOf(minError)
					  
					  	val (pointID, (pointX, pointY, _)) = classedDS(currentDotIdx)
					  
					  	if (classToMovePointInto != currentClusterID) {

						  	classedDS(currentDotIdx) = (pointID, (pointX, pointY, classToMovePointInto))

						  	val classWithoutDot = rangeOverClasses.filter( clusterID => clusterID != classToMovePointInto && clusterID != currentClusterID)

						  	for (j <- classWithoutDot) {
						  		removeLastXY(j, inputX, inputY)
						  	}

					  	}
					  	else {

						  	val classWithoutDot = rangeOverClasses.filter(_ != currentClusterID)
						  	
						  	classWithoutDot.foreach{ j =>
						  		removeLastXY(j, inputX, inputY)
						  	}
							
							inputX(currentClusterID) += ((pointID, pointX))
							inputY(currentClusterID) += pointY

					  	}

					  	continue = !inputX.exists(_.isEmpty)
					  	regressionCriteriaMap += currentDotId -> minError
					  	nbIte += 1
				  		currentDotIdx += 1

				  		if (currentDotIdx > classlimits(currentClass)) {
				  			currentClass += 1
				  		}

				  	}
			  	}

			  	continue = nbIte != stop

			  	if (continue) regressionCriteriaMap.clear
			  	else {
					dsPerClassF = rangeOverClasses.map{ i =>
						classedDS.filter{ case (_, (_, _, clusterID)) => clusterID == i }
					}
					regressionPerClassFinal = rangeOverClasses.map(PLS.runClusterwisePLS(inputX, inputY, _, h))
					classOfEachData = classedDS.map{ case (id, (_, _, clusterID)) => (id, clusterID) }	
			  	}
			}
			catch { 
				case svdExcept : breeze.linalg.NotConvergedException => {
					regressionCriteriaMap.clear
					if (logOn) println("\nThere was an Singular Value Decomposition Issue, retry with new initialisation")	
				}
			}
		}
		while (continue && cptAttemps < nbMaxAttemps)

		if (continue && cptAttemps == nbMaxAttemps) {
			throw new Exception("There was too many unsuccesufull attemps due to empty classes, try to diminish number of class ")
		}

  		val regressionResults: Array[Double] = regressionPerClassFinal.map{ case (ist, _, _, _) => ist } 
	  	val coXYcoef: Array[Array[Double]] = regressionPerClassFinal.map{ case (_, r, _, _) => r.toArray }
	  	val coIntercept: Array[Array[Double]] = regressionPerClassFinal.map{ case (_, _, co, _) => co }
	  	val predFitted: Array[Array[(Int, Array[Double])]] = regressionPerClassFinal.map{ case (_, _, _, fth) => fth } 

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
	  	
	  	// tailrec
	  	do {
			try {
		  		cptAttemps += 1
			  	// Initialisation per group
			  	val perGroupClassInit = microClusterNumberRange.map( x => Random.nextInt(g) )

			  	val dsPerClassPerBucket = dsXYTrain.map{ case (id, (x, y)) => (allGroupedData(id), id, x, y) }.toArray
					.groupBy(_._1)
					.toArray
					.map{ case (microClusterID, aggregate) => (perGroupClassInit(microClusterID), microClusterID, aggregate) }
					.groupBy{ case (clusterID, _, _) => clusterID }
					.toArray
					.sortBy{ case (clusterID, _) => clusterID }

				val bucketOrderPerClass = dsPerClassPerBucket.map{ case (clusterID, buckets) =>
					((clusterID, buckets.map{ case (clusterID, grpId, grpIdIdXY) => grpId }))
				}

				val indexedFlatBucketOrder = bucketOrderPerClass.flatMap{ case (clusterID, grpIds) => grpIds.map( grpId => (grpId, clusterID) ) }.zipWithIndex

				val preSize = dsPerClassPerBucket.map{ case (clusterID, dsPerBucket) => dsPerBucket.map{ case (clusterID, grpId, ds) => ds.length } }
				
				val orderedBucketSize = preSize.flatten
				
				val classSize = preSize.map(_.length )
				
				val classlimits = classSize.indices.toArray.map(i => (0 to i).map(classSize(_)).sum - 1 )

			  	val inputX = dsPerClassPerBucket.map{ case (clusterID, dsPerBucket) =>
			  		val toBuff = dsPerBucket.flatMap{ case (clusterID, grpId, ds) =>
			  			ds.map{ case (grpId, id, x, y) => (id, x) }
			  		}
			  		mutable.ArrayBuffer(toBuff:_*)
			  	}
			  	val inputY = dsPerClassPerBucket.map{ case (clusterID, dsPerBucket) =>
			  		val toBuff = dsPerBucket.flatMap{ case (clusterID, grpId, ds) =>
		  				ds.map{ case (grpId, id, x, y) => y }
		  			}
			  		mutable.ArrayBuffer(toBuff:_*)
			  	}

			  	var currentDotsGrpIdx = 0
			  	var currentClass = 0
			  	var nbIte = 0
				val stop = indexedFlatBucketOrder.length - 1

				// tailrec
		  		breakable {
			  		// if init starts with empty classes, retry
			  		if (inputX.length != g || inputX.exists(_.isEmpty)) break

				  	while (continue && nbIte != stop) {

				  		val ((grpId, currentClusterID), currentIdx) = indexedFlatBucketOrder(nbIte)
				  	
					  	// Regression with Point inside one Class and not the Rest
					  	val regPerClass = rangeOverClasses.map( i => PLS.runClusterwisePLS(inputX, inputY, i, h) )
					  	val error1 = regPerClass.map(_._1)

					  	prepareMovingPointByGroup(dsPerClassPerBucket, inputX, inputY, g, currentDotsGrpIdx, currentClass, classlimits, orderedBucketSize)
					  	
					  	if (inputX.exists(_.isEmpty)) {
					  		if (logOn) println("Problemo one class is becoming empty")
					  		break
					  	}
					  	// Regression with Point inside all other Class and not the former
					  	val regPerClass2 = {
					  		try rangeOverClasses.map( i => PLS.runClusterwisePLS(inputX, inputY, i, h) )
					  		catch {
					  			case emptyClass : java.lang.IndexOutOfBoundsException => Array.empty[RegPerClass]
					  		}
					  	}

					  	if (regPerClass2.isEmpty) break

			  	  		val error2 = regPerClass2.map(_._1)
					  
					  	val boolTab = Array.fill(g)(true)
					  
					  	val errorsIndices: Array[((Double, Double), Int)] = error1.zip(error2).zipWithIndex

					  	boolTab(currentClusterID) = false

					  	val errors = computeErrors(errorsIndices, boolTab, currentClusterID)

					  	val minError = errors.min

					  	val classToMoveGroupInto = errors.indexOf(minError)

					  	val posInClass = posInClassForMovingPoints(currentClass, currentDotsGrpIdx, classlimits)

					  	val (_, bucketIDtoUpdate, grpIdIDXY) = dsPerClassPerBucket(currentClass)._2(posInClass)

					  	if (classToMoveGroupInto != currentClusterID) {

						  	dsPerClassPerBucket(currentClass)._2(posInClass) = (classToMoveGroupInto, bucketIDtoUpdate, grpIdIDXY)

						  	val classWithoutDots = rangeOverClasses.filter( clusterID => clusterID != classToMoveGroupInto && clusterID != currentClusterID)

						  	val rangeIn = (0 until orderedBucketSize(currentDotsGrpIdx))
						  	classWithoutDots.foreach( j => rangeIn.foreach( i => removeLastXY(j, inputX, inputY) ) )

					  	}
					  	else {

						  	val classWithoutDots = rangeOverClasses.filter(_ != currentClusterID)
						  	val rangeIn = (0 until orderedBucketSize(currentDotsGrpIdx))

						  	classWithoutDots.foreach( j => rangeIn.foreach( i => removeLastXY(j, inputX, inputY) ) )

						  	inputX(currentClusterID) ++= grpIdIDXY.map{ case (grpId, id, x, y) => (id, x) }
						  	inputY(currentClusterID) ++= grpIdIDXY.map{ case (grpId, id, x, y) => y }

					  	}

					  	regressionCriteriaMap += ( currentIdx -> minError )
					  	continue = !inputX.exists(_.isEmpty)
						nbIte += 1
				  		currentDotsGrpIdx += 1

				  		if (currentDotsGrpIdx > classlimits(currentClass)) currentClass += 1

			  		}
			  	}
			  	continue = nbIte != stop
			  	if (continue) regressionCriteriaMap.clear
			  	else {
			  		
			  		dsPerClassF = rangeOverClasses.map{ i =>
			  			dsPerClassPerBucket.filter{ case (clusterID, _) => clusterID == i }
			  		}

					regressionPerClassFinal = rangeOverClasses.map(PLS.runClusterwisePLS(inputX, inputY, _, h))

			  		classOfEachData = dsPerClassPerBucket.flatMap{ case (clusterID, dsPerBucket) =>
			  			dsPerBucket.flatMap{ case (_, grpId, ds) =>
			  				ds.map{ case (_, id, x, y) => (id, clusterID) }
			  			}
			  		}

			  	}
			}
			catch {
				case svdExcept : breeze.linalg.NotConvergedException => {
					regressionCriteriaMap.clear
					if (logOn) println("\nThere was an Singular Value Decomposition Issue, retry with new initialisation")
				}
			}
		}
		while (continue && cptAttemps < nbMaxAttemps)

		if (continue && cptAttemps == nbMaxAttemps) throw new Exception("There was too many unsuccesufull attemps due to empty classes, try to diminish number of class or size of blocs")

  		val regressionResults: Array[Double] = regressionPerClassFinal.map{ case (ist, _, _, _) => ist } 
	  	
	  	val coXYcoef: Array[Array[Double]] = regressionPerClassFinal.map{ case (_, r, _, _) => r.toArray }
	  	
	  	val coIntercept: Array[Array[Double]] = regressionPerClassFinal.map{ case (_, _, co, _) => co }
	  	
	  	val predFitted: Array[Array[(Int, Array[Double])]] = regressionPerClassFinal.map{ case (_, _, _, fth) => fth } 

	  	(dsPerClassF, predFitted, coIntercept, coXYcoef, regressionResults, regressionCriteriaMap, classOfEachData)
	}
}

object ClusterwiseCore extends Serializable {

	def plsPerDot(
		dsXYTrain: GenSeq[(Int, (Array[Double], Array[Double]))],
		h: Int,
		g: Int,
		nbMaxAttemps: Int = 30,
		logOn: Boolean = false
	) =	{
		val oneClusterwise = new ClusterwiseCore(dsXYTrain, h, g, nbMaxAttemps, logOn)
		val (dsPerClass, predFitted, coIntercept, coXYcoef, critReg, mapsRegCrit, classedReg) = oneClusterwise.plsPerDot()
		(dsPerClass, predFitted, coIntercept, coXYcoef, critReg, mapsRegCrit, classedReg)
	}

	def plsPerMicroClusters(
		dsXYTrain: GenSeq[(Int, (Array[Double], Array[Double]))],
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