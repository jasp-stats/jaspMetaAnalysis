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
		fromVersion:	"0.17.2"
		toVersion:		"0.17.3"

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
		ChangeJS
		{
			name:		"withinStudyVariation"
			jsFunction:	function(options) {
				switch(options["measure"])
				{
					case "oeRatio":	return options["linkOE"];
					case "cstat":	return options["linkCstat"];
				}
			}
		}
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
		fromVersion:	"0.17.2"
		toVersion:		"0.17.3"

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
		ChangeJS
		{
			name:		"withinStudyVariation"
			jsFunction:	function(options) {
				switch(options["measure"])
				{
					case "oeRatio":	return options["linkOE"];
					case "cstat":	return options["linkCstat"];
				}
			}
		}
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
				switch(options["tauPrior"])
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
		fromVersion:	"0.17.2"
		toVersion:		"0.17.3"

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
		fromVersion:	"0.17.2"
		toVersion:		"0.17.3"

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
		fromVersion:	"0.17.2"
		toVersion:		"0.17.3"

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

		ChangeRename { from: "estimatesFE"; to: "inferenceFixedEffectsMeanEstimatesTable" }
		ChangeRename { from: "weightsFE"; to: "inferenceFixedEffectsEstimatedWeightsTable" }
		ChangeRename { from: "estimatesRE"; to: "inferenceRandomEffectsMeanEstimatesTable" }
		ChangeRename { from: "heterogeneityRE"; to: "inferenceRandomEffectsEstimatedHeterogeneityTable" }
		ChangeRename { from: "weightsRE"; to: "inferenceRandomEffectsEstimatedWeightsTable" }

		ChangeRename { from: "weightFunctionFE"; to: "plotsWeightFunctionFixedEffectsPlot" }
		ChangeRename { from: "weightFunctionRE"; to: "plotsWeightFunctionRandomEffectsPlot" }
		ChangeRename { from: "weightFunctionRescale"; to: "plotsWeightFunctionRescaleXAxis" }
		ChangeRename { from: "plotModels"; to: "plotsMeanModelEstimatesPlot" }	
	}	


	Upgrade
	{
		functionName:	"RobustBayesianMetaAnalysis"
		fromVersion:	"0.17.2"
		toVersion:		"0.17.3"

		// RobustBayesianMetaAnalysis.qml
		ChangeRename { from: "fittedPath"; to: "pathToFittedModel" }
		ChangeRename { from: "inputES"; to: "effectSize" }
		ChangeRename { from: "inputSE"; to: "effectSizeSe" }
		ChangeRename { from: "inputCI"; to: "effectSizeCi" }
		ChangeRename { from: "inputN"; to: "sampleSize" }
		ChangeRename { from: "inputPVal"; to: "pValue" }	
		ChangeRename { from: "inputLabels"; to: "studyLabel" }
		ChangeRename { from: "measures"; to: "inputType" }
		ChangeJS
		{
			name:		"inputType"
			jsFunction:	function(options)
			{
				switch(options["inputType"])
				{
					case "logOR":	return "logOr";
					case "general":	return "unstandardizedEffectSizes";
					case "fitted":	return "fittedModel";
				}
			}
		}
		ChangeRename { from: "modelType"; to: "modelEnsembleType" }
		ChangeJS
		{
			name:		"modelEnsembleType"
			jsFunction:	function(options)
			{
				switch(options["modelEnsembleType"])
				{
					case "2w":	return "original";
				}
			}
		}
		ChangeJS
		{
			name:		"priorScale"
			jsFunction:	function(options)
			{
				switch(options["priorScale"])
				{
					case "cohens_d":	return "cohensD";
					case "fishers_z":	return "fishersZ";
					case "logOR":		return "logOr";
				}
			}
		}
		ChangeRename { from: "plotPriors"; to: "priorDistributionPlot" }
		// inference section
		ChangeRename { from: "resultsConditional"; to: "inferenceConditionalParameterEstimates" }
		ChangeRename { from: "resultsModels"; to: "inferenceModelsOverview" }
		ChangeRename { from: "resultsModelsBf"; to: "inferenceModelsOverviewBfComparison" }
		ChangeRename { from: "resultsModelsOrder"; to: "inferenceModelsOverviewOrder" }
		ChangeJS
		{
			name:		"inferenceModelsOverviewOrder"
			jsFunction:	function(options)
			{
				switch(options["inferenceModelsOverviewOrder"])
				{
					case "default":	return "modelNumber";
					case "marglik":	return "marginalLikelihood";
					case "posterior": return "posteriorProbability";
				}
			}
		}
		ChangeRename { from: "resultsIndividual"; to: "inferenceIndividualModels" }
		ChangeRename { from: "resultsIndividualSingle"; to: "inferenceIndividualModelsSingleModel" }
		ChangeRename { from: "resultsIndividualSingleNumber"; to: "inferenceIndividualModelsSingleModelNumber" }
		ChangeRename { from: "resultsCi"; to: "inferenceCiWidth" }
		ChangeRename { from: "resultsScale"; to: "inferenceOutputScale" }
		ChangeJS
		{
			name:		"inferenceOutputScale"
			jsFunction:	function(options)
			{
				switch(options["inferenceOutputScale"])
				{
					case "cohens_d":	return "cohensD";
					case "fishers_z":	return "fishersZ";
					case "logOR":		return "logOr";
					case "r":			return "correlation";
				}
			}
		}
		ChangeRename { from: "shortNames"; to: "inferenceShortenPriorName" }
		// Plots section
		ChangeRename { from: "plotForest"; to: "plotsForestPlot" }
		ChangeRename { from: "plotForestOrder"; to: "plotsForestPlotOrder" }
		ChangeRename { from: "plotForestType"; to: "plotsForestPlotType" }

		ChangeRename { from: "plotEstimatesMu"; to: "plotsPooledEstimatesEffect" }
		ChangeRename { from: "plotEstimatesTau"; to: "plotsPooledEstimatesHeterogeneity" }
		ChangeRename { from: "plotEstimatesWeightFunction"; to: "plotsPooledEstimatesWeightFunction" }
		ChangeRename { from: "plotEstimatesWeightFunctionRescale"; to: "plotsPooledEstimatesWeightFunctionRescaleXAxis" }
		ChangeRename { from: "plotEstimatesPetPeese"; to: "plotsPooledEstimatesPetPeese" }
		ChangeRename { from: "plotEstimatesType"; to: "plotsPooledEstimatesType" }
		ChangeRename { from: "plotEstimatesPriors"; to: "plotsPooledEstimatesPriorDistribution" }
		
		ChangeRename { from: "plotModelsMu"; to: "plotsIndividualModelsEffect" }
		ChangeRename { from: "plotModelsTau"; to: "plotsIndividualModelsHeterogeneity" }
		ChangeRename { from: "plotModelsType"; to: "plotsIndividualModelsType" }
		ChangeRename { from: "plotModelsOrder"; to: "plotsIndividualModelsOrder" }
		ChangeRename { from: "plotModelsOrderBy"; to: "plotsIndividualModelsOrderBy" }
		ChangeJS
		{
			name:		"plotsIndividualModelsOrderBy"
			jsFunction:	function(options)
			{
				switch(options["plotsIndividualModelsOrderBy"])
				{
					case "model":		return "modelNumber";
					case "BF":			return "bayesFactor";
					case "probability":	return "posteriorProbability";
				}
			}
		}
		ChangeRename { from: "plotModelsShowUpdating"; to: "plotsIndividualModelsShowBayesianUpdating" }
		ChangeRename { from: "plotModelsShowEstimates"; to: "plotsIndividualModelsShowPosteriorEstimates" }
		// MCMC Diagnostics section
		ChangeRename { from: "diagnosticsOverview"; to: "mcmcDiagnosticsOverviewTable" }
		ChangeRename { from: "diagnosticsMu"; to: "mcmcDiagnosticsPlotEffect" }
		ChangeRename { from: "diagnosticsTau"; to: "mcmcDiagnosticsPlotHeterogeneity" }
		ChangeRename { from: "diagnosticsOmega"; to: "mcmcDiagnosticsPlotWeights" }
		ChangeRename { from: "diagnosticsPet"; to: "mcmcDiagnosticsPlotPet" }
		ChangeRename { from: "diagnosticsPeese"; to: "mcmcDiagnosticsPlotPeese" }
		ChangeRename { from: "diagnosticsTrace"; to: "mcmcDiagnosticsPlotTypeTrace" }
		ChangeRename { from: "diagnosticsAutocorrelation"; to: "mcmcDiagnosticsPlotTypeAutocorrelation" }
		ChangeRename { from: "diagnosticsSamples"; to: "mcmcDiagnosticsPlotTypePosteriorSamplesDensity" }
		ChangeRename { from: "diagnosticsSingle"; to: "mcmcDiagnosticsPlotSingleModel" }
		ChangeRename { from: "diagnosticsSingleModel"; to: "mcmcDiagnosticsPlotSingleModelNumber" }
		// Priors section
		ChangeRename { from: "effect"; to: "modelsEffect" }
		ChangeRename { from: "heterogeneity"; to: "modelsHeterogeneity" }
		ChangeRename { from: "omega"; to: "modelsSelectionModels" }
		ChangeRename { from: "pet"; to: "modelsPet" }
		ChangeRename { from: "peese"; to: "modelsPeese" }
		ChangeRename { from: "effectNull"; to: "modelsEffectNull" }
		ChangeRename { from: "heterogeneityNull"; to: "modelsHeterogeneityNull" }
		ChangeRename { from: "omegaNull"; to: "modelsSelectionModelsNull" }
		ChangeRename { from: "petNull"; to: "modelsPetNull" }
		ChangeRename { from: "peeseNull"; to: "modelsPeeseNull" }
		// qml_components/RobustBayesianMetaAnalysisPriors.qml
		ChangeRename { from: "parMean"; to: "mu" }
		ChangeRename { from: "parLocation"; to: "x0" }
		ChangeRename { from: "parScale"; to: "sigma" }
		ChangeRename { from: "parShape"; to: "k" }
		ChangeRename { from: "parScale2"; to: "theta" }
		ChangeRename { from: "parDf"; to: "nu" }
		ChangeRename { from: "parAlpha"; to: "alpha" }
		ChangeRename { from: "parBeta"; to: "beta" }
		ChangeRename { from: "parA"; to: "a" }
		ChangeRename { from: "parB"; to: "b" }
		// qml_components/RobustBayesianMetaAnalysisWeightunctions.qml
		ChangeJS
		{
			name:		"type"
			jsFunction:	function(options)
			{
				switch(options["type"])
				{
					case "two-sided":	return "twoSided";
					case "one-sided":	return "oneSided";
					case "two-sided-fixed":	return "twoSidedFixed";
					case "one-sided-fixed":	return "oneSidedFixed";

				}
			}
		}
		ChangeRename { from: "parCuts"; to: "pValues" }
		ChangeRename { from: "parOmega"; to: "omega" }
		// Advanced section
		ChangeRename { from: "fittingScale"; to: "advancedEstimationScale" }
		ChangeJS
		{
			name:		"advancedEstimationScale"
			jsFunction:	function(options)
			{
				switch(options["advancedEstimationScale"])
				{
					case "cohens_d":	return "cohensD";
					case "fishers_z":	return "fishersZ";
					case "logOR":		return "logOr";
				}
			}
		}
		ChangeRename { from: "advancedAdapt"; to: "advancedMcmcAdaptation" }
		ChangeRename { from: "advancedBurnin"; to: "advancedMcmcBurnin" }
		ChangeRename { from: "advancedIteration"; to: "advancedMcmcSamples" }
		ChangeRename { from: "advancedChains"; to: "advancedMcmcChains" }
		ChangeRename { from: "advancedThin"; to: "advancedMcmcThin" }
		ChangeRename { from: "autofitRhat"; to: "advancedAutofitRHat" }
		ChangeRename { from: "autofitRhatValue"; to: "advancedAutofitRHatTarget" }
		ChangeRename { from: "autofitEss"; to: "advancedAutofitEss" }
		ChangeRename { from: "autofitEssValue"; to: "advancedAutofitEssTarget" }
		ChangeRename { from: "autofitMcmcError"; to: "advancedAutofitMcmcError" }
		ChangeRename { from: "autofitMcmcErrorValue"; to: "advancedAutofitMcmcErrorTarget" }
		ChangeRename { from: "autofitMcmcErrorSd"; to: "advancedAutofitMcmcErrorSd" }
		ChangeRename { from: "autofitMcmcErrorSdValue"; to: "advancedAutofitMcmcErrorSdTarget" }
		ChangeRename { from: "autofitTime"; to: "advancedAutofitMaximumFittingTime" }
		ChangeRename { from: "autofitTimeValue"; to: "advancedAutofitMaximumFittingTimeTarget" }
		ChangeRename { from: "autofitTimeUnit"; to: "advancedAutofitMaximumFittingTimeTargetUnit" }
		ChangeRename { from: "autofitExtendSamples"; to: "advancedAutofitExtendSamples" }
		ChangeRename { from: "removeFailed"; to: "advancedAutofitRemoveFailedModels" }
		ChangeRename { from: "balanceProbability"; to: "advancedAutofitRebalanceComponentProbabilityOnModelFailure" }
		ChangeRename { from: "savePath"; to: "advancedSaveFittedModel" }
	}

	Upgrade
	{
		functionName:	"PenalizedMetaAnalysis"
		fromVersion:	"0.17.2"
		toVersion:		"0.17.3"


		ChangeRename { from: "components"; to: "modelComponents" }
		ChangeRename { from: "interceptTerm"; to: "modelIncludeIntercept" }
		ChangeRename { from: "scalePredictors"; to: "modelScalePredictors" }
		ChangeRename { from: "estimatesCoefficients"; to: "inferenceEstimatesTable" }
		ChangeRename { from: "estimatesTau"; to: "inferenceHeterogeneityTable" }
		ChangeRename { from: "estimatesI2"; to: "inferenceHeterogeneityI2" }
		ChangeRename { from: "availableModelComponentsPlot"; to: "posteriorPlotsAvailableTerms" }
		ChangeRename { from: "plotPosterior"; to: "posteriorPlotsSelectedTerms" }
	}
}
