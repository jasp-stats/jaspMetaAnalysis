import QtQuick
import JASP.Module

Upgrades
{
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
					default:		return options["measure"];
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
					default:		return options["linkOE"];
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
					default:		return options["measure"];
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
					default:		return options["linkOE"];
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
					default:			return options["tauPrior"];
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

	Upgrade
	{
		functionName:	"ClassicalPredictionPerformance"
		fromVersion:	"0.19.1"
		toVersion:		"0.19.2"

		ChangeJS
		{
			name:		"withinStudyVariation"
			jsFunction:	function(options)
			{
				if (options[["measure"]] == "cStatistic") {
					switch(options["withinStudyVariation"])
					{
						case "normal/log":	return "normal/logit";
						default:			return options["withinStudyVariation"];
					}
				} else {
					return options["withinStudyVariation"]
				}
			}
		}

		ChangeJS
		{
			name:		"method"
			jsFunction:	function(options)
			{
				switch(options["withinStudyVariation"])
				{
					case "Fixed Effects"		: return "fixedEffects";
					case "Maximum Likelihood"	: return "maximumLikelihood";
					case "Restricted ML"		: return "restrictedML";
					case "DerSimonian-Laird"	: return "derSimonianLaird";
					case "Hedges"				: return "hedges";
					case "Hunter-Schmidt"		: return "hunterSchmidt";
					case "Sidik-Jonkman"		: return "sidikJonkman";
					case "Empirical Bayes"		: return "empiricalBayes";
					case "Paule-Mandel"			: return "pauleMandel";
				}
			}
		}
	}

	Upgrade
	{
		functionName:	"BayesianPredictionPerformance"
		fromVersion:	"0.19.1"
		toVersion:		"0.19.2"

		ChangeJS
		{
			name:		"withinStudyVariation"
			jsFunction:	function(options)
			{
				if (options[["measure"]] == "cStatistic") {
					switch(options["withinStudyVariation"])
					{
						case "normal/log":	return "normal/logit";
						default:			return options["withinStudyVariation"];
					}
				} else {
					return options["withinStudyVariation"]
				}
			}
		}
	}

	Upgrade
	{
		functionName:	"ClassicalMetaAnalysis"
		fromVersion:	"0.19.1"
		toVersion:		"0.19.2"

		ChangeIncompatible
		{
			msg: qsTr("Results of this analysis cannot be updated. The analysis was created with an older version of JASP and the analysis options are not longer compatible. Please, redo the analysis with the updated module or download the 0.19.1 version of JASP to rerun or edit the analysis.")
		}
	}

	Upgrade
	{
		functionName:	"BayesianMetaAnalysis"
		fromVersion:	"0.19.3"
		toVersion:		"0.95.0"

		ChangeIncompatible
		{
			msg: qsTr("Results of this analysis cannot be updated. The analysis was created with an older version of JASP and the analysis options are not longer compatible. Please, redo the analysis with the updated module or download the 0.19.3 version of JASP to rerun or edit the analysis.")
		}
	}

	Upgrade
	{
		functionName:	"BayesianBinomialMetaAnalysis"
		fromVersion:	"0.19.3"
		toVersion:		"0.95.0"

		ChangeIncompatible
		{
			msg: qsTr("Results of this analysis cannot be updated. The analysis was created with an older version of JASP and the analysis options are not longer compatible. Please, redo the analysis with the updated module or download the 0.19.3 version of JASP to rerun or edit the analysis.")
		}
	}

	Upgrade
	{
		functionName:	"BayesianMetaAnalysis"
		fromVersion:	"0.19.3"
		toVersion:		"0.95.0"

		ChangeIncompatible
		{
			msg: qsTr("Results of this analysis cannot be updated. The analysis was created with an older version of JASP and the analysis options are not longer compatible. Please, redo the analysis with the updated module or download the 0.19.3 version of JASP to rerun or edit the analysis.")
		}
	}
}


