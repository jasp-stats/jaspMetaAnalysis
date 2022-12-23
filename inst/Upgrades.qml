import QtQuick		2.12
import JASP.Module	1.0

Upgrades
{
	Upgrade
	{
		functionName:	"ClassicalMetaAnalysis"
		fromVersion:	"0.16.4"
		toVersion:		"0.17"

		// ClassicalMetaAnalysis.qml
		ChangeRename { from: "dependent"; to: "effectSize" }
		ChangeRename { from: "wlsWeights"; to: "effectSizeSe" }
		ChangeRename { from: "studyLabels"; to: "studyLabel" }
		ChangeRename { from: "includeConstant"; to: "interceptTerm" }

		// ClassicalMetaAnalysisStatistics.qml
		ChangeRename { from: "regressionCoefficientsEstimates"; to: "coefficientEstimate" }
		ChangeRename { from: "regressionCoefficientsConfidenceIntervals"; to: "coefficientCi" }
		ChangeRename { from: "regressionCoefficientsConfidenceIntervalsInterval"; to: "coefficientCiLevel" }
		ChangeRename { from: "test"; to: "estimateTest" }
		ChangeRename { from: "regressionCoefficientsCovarianceMatrix"; to: "covarianceMatrix" }
		ChangeRename { from: "modelFit"; to: "fitMeasure" }
		ChangeRename { from: "rSquaredChange"; to: "funnelPlotRankTestAsymmetry" }
		ChangeRename { from: "funnelPlotAsymmetryTest"; to: "funnelPlotRegressionTestAsymmetry" }
		ChangeRename { from: "residualsParameters"; to: "residualParameter" }

		// ClassicalMetaAnalysisDiagnostics.qml
		ChangeRename { from: "trimFillPlot"; to: "trimFillAnalysis" }
		ChangeRename { from: "plotResidualsPredicted"; to: "profilePlot" }
		ChangeRename { from: "plotResidualsDependent"; to: "diagnosticPlot" }
		ChangeRename { from: "plotResidualsQQ"; to: "diagnosticQqPlot" }
		ChangeRename { from: "plotResidualsCovariates"; to: "failSafeN" }
		ChangeRename { from: "residualsCasewiseDiagnostics"; to: "casewiseDiagnostics" }
	}
	Upgrade
	{
		functionName:	"BayesianMetaAnalysis"
		fromVersion:	"0.16.4"
		toVersion:		"0.17"

		// BayesianMetaAnalysis.qml
		ChangeRename { from: "standardError"; to: "effectSizeSe" }
		ChangeRename { from: "confidenceInterval"; to: "effectSizeCi" }
		ChangeRename { from: "studyLabels"; to: "studyLabel" }

		// BayesianMetaAnalysisInference.qml
		ChangeRename { from: "modelSpecification"; to: "model" }
		ChangeJS
		{
				name:		"model"
				jsFunction:	function(options)
				{
						switch(options["model"])
						{
								case "FE":	return "fixed";
								case "RE":	return "random";
								case "BMA":	return "averaging";
								case "CRE":	return "constrainedRandom";
						}
				}
		}
		ChangeRename { from: "direction"; to: "constrainedRandomDirection" }
		ChangeJS
		{
				name:		"constrainedRandomDirection"
				jsFunction:	function(options)
				{
						switch(options["constrainedRandomDirection"])
						{
								case "allPos":	return "positive";
								case "allNeg":	return "negative";
						}
				}
		}
		ChangeRename { from: "postTable"; to: "modelProbability" }
		ChangeRename { from: "esTable"; to: "effectSizePerStudy" }

		// BayesianMetaAnalysisPlots.qml
		ChangeRename { from: "forestPlot"; to: "forestPlotEffect" }
		ChangeRename { from: "checkForestPlot"; to: "forestPlot" }
		ChangeJS
		{
				name:		"forestPlotEffect"
				jsFunction:	function(options)
				{
						switch(options["forestPlotEffect"])
						{
								case "plotForestObserved":	return "observed";
								case "plotForestEstimated":	return "estimated";
								case "plotForestBoth":	    return "both";
						}
				}
		}
		ChangeRename { from: "showLabels"; to: "forestPlotLabel" }
		ChangeRename { from: "orderForest"; to: "forestPlotRowOrder" }
		ChangeJS
		{
				name:		"forestPlotRowOrder"
				jsFunction:	function(options)
				{
						switch(options["forestPlotRowOrder"])
						{
								case "ascendingForest":	    return "ascending";
								case "descendingForest":	return "descending";
								case "labelForest":	        return "rowOrder";
						}
				}
		}
		ChangeRename { from: "plotCumForest"; to: "cumulativeForestPlot" }
		ChangeRename { from: "addPrior"; to: "cumulativeForestPlotPrior" }
		ChangeRename { from: "plotPosterior"; to: "priorPosterior" }
		ChangeRename { from: "addInfo"; to: "priorPosteriorAdditionalInfo" }
		ChangeRename { from: "addLines"; to: "priorPosteriorFixedAndRandom" }
		ChangeRename { from: "shade"; to: "priorPosteriorCi" }
		ChangeRename { from: "plotSequential"; to: "bfSequentialPlot" }
		ChangeRename { from: "plotSeqPM"; to: "modelProbabilitySequentialPlot" }

		// BayesianMetaAnalysisPriors.qml
		ChangeRename { from: "priorES"; to: "priorEffectSize" }
		ChangeRename { from: "informativeCauchyLocation"; to: "cauchyLocation" }
		ChangeRename { from: "informativeCauchyScale"; to: "cauchyScale" }
		ChangeRename { from: "informativeNormalMean"; to: "normalMean" }
		ChangeRename { from: "informativeNormalStd"; to: "normalSd" }
		ChangeRename { from: "informativeTLocation"; to: "tLocation" }
		ChangeRename { from: "informativeTScale"; to: "tScale" }
		ChangeRename { from: "informativeTDf"; to: "tDf" }
		ChangeRename { from: "checkLowerPrior"; to: "truncationLowerBound" }
		ChangeRename { from: "lowerTrunc"; to: "truncationLowerBoundValue" }
		ChangeRename { from: "checkUpperPrior"; to: "truncationUpperBound" }
		ChangeRename { from: "upperTrunc"; to: "truncationUpperBoundValue" }
		ChangeRename { from: "priorSE"; to: "priorStandardError" }
		ChangeRename { from: "informativehalfTScale"; to: "halfTScale" }
		ChangeRename { from: "informativehalfTDf"; to: "halfTDf" }
		ChangeRename { from: "plotPrior"; to: "priorPlot" }

		// BayesianMetaAnalysisAdvanced.qml
		ChangeRename { from: "priorH0FE"; to: "priorModelProbabilityFixedNull" }
		ChangeRename { from: "priorH1FE"; to: "priorModelProbabilityFixedAlternative" }
		ChangeRename { from: "priorH0RE"; to: "priorModelProbabilityRandomNull" }
		ChangeRename { from: "priorH1RE"; to: "priorModelProbabilityRandomAlternative" }
		ChangeRename { from: "iterMCMC"; to: "samples" }
		ChangeRename { from: "chainsMCMC"; to: "chains" }
		ChangeRename { from: "BFComputation"; to: "bayesFactorComputation" }
		ChangeRename { from: "iterBridge"; to: "bridgeSamplingSamples" }
	}
	Upgrade
	{
		functionName:	"ClassicalPredictionPerformance"
		fromVersion:	"0.16.4"
		toVersion:		"0.17"

		// PredictionPerformanceData.qml
		ChangeRename { from: "inputMeasure"; to: "effectSize" }
		ChangeRename { from: "inputSE"; to: "effectSizeSe" }
		ChangeRename { from: "inputCI"; to: "effectSizeCi" }
		ChangeRename { from: "inputN"; to: "numberOfParticipants" }
		ChangeRename { from: "inputO"; to: "numberOfObservedEvents" }
		ChangeRename { from: "inputE"; to: "numberOfExpectedEvents" }
		ChangeRename { from: "inputLabels"; to: "studyLabel" }

		ChangeJS
		{
			name:		"measure"
			jsFunction:	function(options)
			{
				switch(options["measure"])
				{
					case "OE":		return "oeRatio";
					case "cstat":	return "cStatistic";
				}
			}
		}

		// PredictionPerformanceInference
		ChangeRename { from: "linkOE"; to: "withinStudyVariation" }
		ChangeRename { from: "linkCstat"; to: "withinStudyVariation" }
		ChangeRename { from: "exportColumns"; to: "exportComputedEffectSize" }
		ChangeRename { from: "exportOE"; to: "exportComputedEffectSizeOeRatioColumnName" }
		ChangeRename { from: "exportOElCI"; to: "exportComputedEffectSizeOeRatioLCiColumnName" }
		ChangeRename { from: "exportOEuCI"; to: "exportComputedEffectSizeOeRatioUCiColumnName" }
		ChangeRename { from: "exportCstat"; to: "exportComputedEffectSizeCStatisticColumnName" }
		ChangeRename { from: "exportCstatlCI"; to: "exportComputedEffectSizeCStatisticLCiColumnName" }
		ChangeRename { from: "exportCstatuCI"; to: "exportComputedEffectSizeCStatisticUCiColumnName" }
		ChangeRename { from: "funnelAsymmetryTest"; to: "funnelPlotAsymmetryTest" }
		ChangeRename { from: "funnelAsymmetryTestEggerUW"; to: "funnelPlotAsymmetryTestEggerUnweighted" }
		ChangeRename { from: "funnelAsymmetryTestEggerFIV"; to: "funnelPlotAsymmetryTestEggerMultiplicativeOverdispersion" }
		ChangeRename { from: "funnelAsymmetryTestMacaskillFIV"; to: "funnelPlotAsymmetryTestMacaskill" }
		ChangeRename { from: "funnelAsymmetryTestMacaskillFPV"; to: "funnelPlotAsymmetryTestMacaskillPooled" }
		ChangeRename { from: "funnelAsymmetryTestPeters"; to: "funnelPlotAsymmetryTestPeters" }
		ChangeRename { from: "funnelAsymmetryTestDebrayFIV"; to: "funnelPlotAsymmetryTestDebray" }
		ChangeRename { from: "funnelAsymmetryTestPlot"; to: "funnelPlotAsymmetryTestPlot" }
	}

	Upgrade
	{
		functionName:	"BayesianPredictionPerformance"
		fromVersion:	"0.16.4"
		toVersion:		"0.17"

		// PredictionPerformanceData.qml
		ChangeRename { from: "inputMeasure"; to: "effectSize" }
		ChangeRename { from: "inputSE"; to: "effectSizeSe" }
		ChangeRename { from: "inputCI"; to: "effectSizeCi" }
		ChangeRename { from: "inputN"; to: "numberOfParticipants" }
		ChangeRename { from: "inputO"; to: "numberOfObservedEvents" }
		ChangeRename { from: "inputE"; to: "numberOfExpectedEvents" }
		ChangeRename { from: "inputLabels"; to: "studyLabel" }

		ChangeJS
		{
			name:		"measure"
			jsFunction:	function(options)
			{
				switch(options["measure"])
				{
					case "OE":		return "oeRatio";
					case "cstat":	return "cStatistic";
				}
			}
		}

		// PredictionPerformanceInference
		ChangeRename { from: "linkOE"; to: "withinStudyVariation" }
		ChangeRename { from: "linkCstat"; to: "withinStudyVariation" }
		ChangeRename { from: "exportColumns"; to: "exportComputedEffectSize" }
		ChangeRename { from: "exportOE"; to: "exportComputedEffectSizeOeRatioColumnName" }
		ChangeRename { from: "exportOElCI"; to: "exportComputedEffectSizeOeRatioLCiColumnName" }
		ChangeRename { from: "exportOEuCI"; to: "exportComputedEffectSizeOeRatioUCiColumnName" }
		ChangeRename { from: "exportCstat"; to: "exportComputedEffectSizeCStatisticColumnName" }
		ChangeRename { from: "exportCstatlCI"; to: "exportComputedEffectSizeCStatisticLCiColumnName" }
		ChangeRename { from: "exportCstatuCI"; to: "exportComputedEffectSizeCStatisticUCiColumnName" }
		ChangeRename { from: "funnelAsymmetryTest"; to: "funnelPlotAsymmetryTest" }
		ChangeRename { from: "funnelAsymmetryTestEggerUW"; to: "funnelPlotAsymmetryTestEggerUnweighted" }
		ChangeRename { from: "funnelAsymmetryTestEggerFIV"; to: "funnelPlotAsymmetryTestEggerMultiplicativeOverdispersion" }
		ChangeRename { from: "funnelAsymmetryTestMacaskillFIV"; to: "funnelPlotAsymmetryTestMacaskill" }
		ChangeRename { from: "funnelAsymmetryTestMacaskillFPV"; to: "funnelPlotAsymmetryTestMacaskillPooled" }
		ChangeRename { from: "funnelAsymmetryTestPeters"; to: "funnelPlotAsymmetryTestPeters" }
		ChangeRename { from: "funnelAsymmetryTestDebrayFIV"; to: "funnelPlotAsymmetryTestDebray" }
		ChangeRename { from: "funnelAsymmetryTestPlot"; to: "funnelPlotAsymmetryTestPlot" }
	
		// PredictionPerformancePriors
		ChangeRename { from: "priorMuNMeam"; to: "muNormalPriorMean" }
		ChangeRename { from: "priorMuNSD"; to: "muNormalPriorSd" }
		ChangeRename { from: "priorTau"; to: "tauPrior" }
		ChangeJS
		{
			name:		"tauPrior"
			jsFunction:	function(options)
			{
				switch(options["measure"])
				{
					case "priorTauU":	return "uniformPrior";
					case "priorTauT":	return "tPrior";
				}
			}
		}
		ChangeRename { from: "priorTauUMin"; to: "tauUniformPriorMin" }
		ChangeRename { from: "priorTauUMax"; to: "tauUniformPriorMax" }
		ChangeRename { from: "priorTauTLocation"; to: "tauTPriorLocation" }
		ChangeRename { from: "priorTauTScale"; to: "tauTPriorScale" }
		ChangeRename { from: "priorTauTDf"; to: "tauTPriorDf" }
		ChangeRename { from: "priorTauTMin"; to: "tauTPriorMin" }
		ChangeRename { from: "priorTauTMax"; to: "tauTPriorMax" }
	}

	Upgrade
	{
		functionName:	"WaapWls"
		fromVersion:	"0.16.4"
		toVersion:		"0.17"

		// WaapWls.qml
		ChangeRename { from: "inputES"; to: "effectSize" }
		ChangeRename { from: "inputSE"; to: "effectSizeSe" }
		ChangeRename { from: "inputCI"; to: "effectSizeCi" }
		ChangeRename { from: "inputN"; to: "sampleSize" }
		ChangeRename { from: "inputLabels"; to: "studyLabel" }
		ChangeRename { from: "muTransform"; to: "transformCorrelationsTo" }
		ChangeRename { from: "estimatesMean"; to: "inferenceMeanEstimatesTable" }
		ChangeRename { from: "estimatesSigma"; to: "inferenceMultiplicativeHeterogeneityEstimatesEstimatesTable" }
		ChangeRename { from: "plotModels"; to: "plotsMeanModelEstimatesPlot" }
	}

	Upgrade
	{
		functionName:	"PetPeese"
		fromVersion:	"0.16.4"
		toVersion:		"0.17"

		// PetPeese.qml
		ChangeRename { from: "inputES"; to: "effectSize" }
		ChangeRename { from: "inputSE"; to: "effectSizeSe" }
		ChangeRename { from: "inputCI"; to: "effectSizeCi" }
		ChangeRename { from: "inputN"; to: "sampleSize" }
		ChangeRename { from: "inputLabels"; to: "studyLabel" }
		ChangeRename { from: "muTransform"; to: "transformCorrelationsTo" }
		ChangeRename { from: "estimatesMean"; to: "inferenceMeanEstimatesTable" }
		ChangeRename { from: "estimatesPetPeese"; to: "inferenceRegressionEstimatesTable" }
		ChangeRename { from: "estimatesSigma"; to: "inferenceMultiplicativeHeterogeneityEstimatesEstimatesTable" }
		ChangeRename { from: "regressionPeese"; to: "plotsRegressionEstimatePeesePlot" }
		ChangeRename { from: "regressionPet"; to: "plotsRegressionEstimatePetPlot" }
		ChangeRename { from: "plotModels"; to: "plotsMeanModelEstimatesPlot" }	
	}

	Upgrade
	{
		functionName:	"SelectionModels"
		fromVersion:	"0.16.4"
		toVersion:		"0.17"

		// SelectionModels.qml
		ChangeRename { from: "inputES"; to: "effectSize" }
		ChangeRename { from: "inputSE"; to: "effectSizeSe" }
		ChangeRename { from: "inputCI"; to: "effectSizeCi" }
		ChangeRename { from: "inputN"; to: "sampleSize" }
		ChangeRename { from: "inputPVal"; to: "pValue" }	
		ChangeRename { from: "inputLabels"; to: "studyLabel" }
		ChangeRename { from: "muTransform"; to: "transformCorrelationsTo" }

		ChangeRename { from: "cutoffsPVal"; to: "modelPValueCutoffs" }
		ChangeRename { from: "selectionTwosided"; to: "modelTwoSidedSelection" }
		ChangeRename { from: "tablePVal"; to: "modelPValueFrequencyTable" }
		ChangeRename { from: "joinPVal"; to: "modelAutomaticallyJoinPValueIntervals" }
		ChangeRename { from: "effectDirection"; to: "modelExpectedDirectionOfEffectSizes" }

		ChangeRename { from: "estimatesMean"; to: "inferenceMeanEstimatesTable" }
		ChangeRename { from: "estimatesPetPeese"; to: "inferenceRegressionEstimatesTable" }
		ChangeRename { from: "estimatesSigma"; to: "inferenceMultiplicativeHeterogeneityEstimatesEstimatesTable" }
		ChangeRename { from: "regressionPeese"; to: "plotsRegressionEstimatePeesePlot" }
		ChangeRename { from: "regressionPet"; to: "plotsRegressionEstimatePetPlot" }
		ChangeRename { from: "plotModels"; to: "plotsMeanModelEstimatesPlot" }	
	}	
}