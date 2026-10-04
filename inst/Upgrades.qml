import QtQuick
import JASP.Module

Upgrades
{
	Upgrade
	{
		functionName:	"SelectionModels"
		fromVersion:	"0.97.5"
		toVersion:		"0.98.0"
		msg:			qsTr("Selection Models was reworked in Meta-Analysis module 0.98.0. Refreshing this analysis replaces the saved results and may reset incompatible settings. To rerun the original analysis, use JASP 0.98.1 with Meta-Analysis module 0.95.5.")

		ChangeRename { condition: function(options) { return options["effectSizeSe"] !== undefined && options["effectSizeStandardError"] === undefined; }; from: "effectSizeSe"; to: "effectSizeStandardError" }
		ChangeRename { condition: function(options) { return options["studyLabel"] !== undefined && options["studyLabels"] === undefined; }; from: "studyLabel"; to: "studyLabels" }
		ChangeRename { condition: function(options) { return options["modelExpectedDirectionOfEffectSizes"] !== undefined && options["modelExpectedDirectionOfTheEffect"] === undefined; }; from: "modelExpectedDirectionOfEffectSizes"; to: "modelExpectedDirectionOfTheEffect" }
		ChangeRename { condition: function(options) { return options["modelPValueFrequencyTable"] !== undefined && options["weightFunctionPValueFrequencyTable"] === undefined; }; from: "modelPValueFrequencyTable"; to: "weightFunctionPValueFrequencyTable" }

		ChangeJS
		{
			condition: function(options) { return options["modelPValueCutoffs"] !== undefined && options["selectionModels"] === undefined; }
			name: "selectionModels"
			isNewOption: true
			jsFunction: function(options)
			{
				// Desktop also applies this change to column-encoding metadata.
				if (options["modelPValueCutoffs"] === undefined)
					return { sampleSize: { shouldEncode: true } };

				var steps = options["modelPValueCutoffs"];
				var cutoffs = String(steps).replace(/^\s*c?\s*\(?/, "").replace(/\)?\s*$/, "").split(",").filter(function(value) { return value.trim() !== ""; }).map(Number);
				if (cutoffs.length > 0 && cutoffs.every(function(value) { return isFinite(value) && value > 0 && value < 1; }))
				{
					if (options["modelTwoSidedSelection"] === true)
						cutoffs = cutoffs.map(function(value) { return value / 2; }).concat(cutoffs.map(function(value) { return 1 - value / 2; }));
					cutoffs.sort(function(a, b) { return a - b; });
					steps = "(" + cutoffs.join(", ") + ")";
				}
				// Legacy cutoffs and supplied p-values use the one-sided scale.
				return [{ name: ["model1"], type: "stepfun", steps: steps, delta: "", sidedness: "oneSided", prec: "none", sampleSize: { types: [], value: "" }, scaleprec: true, decreasing: false }];
			}
		}
		ChangeSetValue { condition: function(options) { return options["modelPValueCutoffs"] !== undefined && options["publicationBiasAdjustment"] === undefined; }; name: "publicationBiasAdjustment"; jsonValue: "custom" }
		ChangeSetValue { condition: function(options) { return options["method"] === undefined && ["inferenceFixedEffectsMeanEstimatesTable", "inferenceFixedEffectsEstimatedWeightsTable", "plotsWeightFunctionFixedEffectsPlot"].some(function(name) { return options[name] === true; }) && ["inferenceRandomEffectsMeanEstimatesTable", "inferenceRandomEffectsEstimatedHeterogeneityTable", "inferenceRandomEffectsEstimatedWeightsTable", "plotsWeightFunctionRandomEffectsPlot"].every(function(name) { return options[name] !== true; }); }; name: "method"; jsonValue: "fixedEffects" }
		ChangeSetValue { condition: function(options) { return options["weightFunctionEstimates"] === undefined && (options["inferenceFixedEffectsEstimatedWeightsTable"] === true || options["inferenceRandomEffectsEstimatedWeightsTable"] === true); }; name: "weightFunctionEstimates"; jsonValue: true }
		ChangeSetValue { condition: function(options) { return options["weightFunctionEstimates"] === undefined && options["inferenceFixedEffectsEstimatedWeightsTable"] === false && options["inferenceRandomEffectsEstimatedWeightsTable"] === false; }; name: "weightFunctionEstimates"; jsonValue: false }
		ChangeSetValue { condition: function(options) { return options["weightFunctionPlot"] === undefined && (options["plotsWeightFunctionFixedEffectsPlot"] === true || options["plotsWeightFunctionRandomEffectsPlot"] === true); }; name: "weightFunctionPlot"; jsonValue: true }
		ChangeSetValue { condition: function(options) { return options["weightFunctionPlot"] === undefined && options["plotsWeightFunctionFixedEffectsPlot"] === false && options["plotsWeightFunctionRandomEffectsPlot"] === false; }; name: "weightFunctionPlot"; jsonValue: false }

		ChangeRemove { condition: function(options) { return options["effectSizeSe"] !== undefined; }; name: "effectSizeSe" }
		ChangeRemove { condition: function(options) { return options["effectSizeCi"] !== undefined; }; name: "effectSizeCi" }
		ChangeRemove { condition: function(options) { return options["studyLabel"] !== undefined; }; name: "studyLabel" }
		ChangeRemove { condition: function(options) { return options["modelExpectedDirectionOfEffectSizes"] !== undefined; }; name: "modelExpectedDirectionOfEffectSizes" }
		ChangeRemove { condition: function(options) { return options["modelPValueFrequencyTable"] !== undefined; }; name: "modelPValueFrequencyTable" }
		ChangeRemove { condition: function(options) { return options["modelPValueCutoffs"] !== undefined; }; name: "modelPValueCutoffs" }
		ChangeRemove { condition: function(options) { return options["modelTwoSidedSelection"] !== undefined; }; name: "modelTwoSidedSelection" }
		ChangeRemove { condition: function(options) { return options["modelAutomaticallyJoinPValueIntervals"] !== undefined; }; name: "modelAutomaticallyJoinPValueIntervals" }
		ChangeRemove { condition: function(options) { return options["measures"] !== undefined; }; name: "measures" }
		ChangeRemove { condition: function(options) { return options["transformCorrelationsTo"] !== undefined; }; name: "transformCorrelationsTo" }
		ChangeRemove { condition: function(options) { return options["sampleSize"] !== undefined; }; name: "sampleSize" }
		ChangeRemove { condition: function(options) { return options["inferenceFixedEffectsMeanEstimatesTable"] !== undefined; }; name: "inferenceFixedEffectsMeanEstimatesTable" }
		ChangeRemove { condition: function(options) { return options["inferenceFixedEffectsEstimatedWeightsTable"] !== undefined; }; name: "inferenceFixedEffectsEstimatedWeightsTable" }
		ChangeRemove { condition: function(options) { return options["inferenceRandomEffectsMeanEstimatesTable"] !== undefined; }; name: "inferenceRandomEffectsMeanEstimatesTable" }
		ChangeRemove { condition: function(options) { return options["inferenceRandomEffectsEstimatedHeterogeneityTable"] !== undefined; }; name: "inferenceRandomEffectsEstimatedHeterogeneityTable" }
		ChangeRemove { condition: function(options) { return options["inferenceRandomEffectsEstimatedWeightsTable"] !== undefined; }; name: "inferenceRandomEffectsEstimatedWeightsTable" }
		ChangeRemove { condition: function(options) { return options["plotsMeanModelEstimatesPlot"] !== undefined; }; name: "plotsMeanModelEstimatesPlot" }
		ChangeRemove { condition: function(options) { return options["plotsWeightFunctionFixedEffectsPlot"] !== undefined; }; name: "plotsWeightFunctionFixedEffectsPlot" }
		ChangeRemove { condition: function(options) { return options["plotsWeightFunctionRandomEffectsPlot"] !== undefined; }; name: "plotsWeightFunctionRandomEffectsPlot" }
		ChangeRemove { condition: function(options) { return options["plotsWeightFunctionRescaleXAxis"] !== undefined; }; name: "plotsWeightFunctionRescaleXAxis" }
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
		ChangeRename { condition: function(options) { return options["inputES"] !== undefined && options["effectSize"] === undefined; }; from: "inputES"; to: "effectSize" }
		ChangeRename { condition: function(options) { return options["inputSE"] !== undefined && options["effectSizeSe"] === undefined; }; from: "inputSE"; to: "effectSizeSe" }
		ChangeRename { condition: function(options) { return options["inputCI"] !== undefined && options["effectSizeCi"] === undefined; }; from: "inputCI"; to: "effectSizeCi" }
		ChangeRename { condition: function(options) { return options["inputN"] !== undefined && options["sampleSize"] === undefined; }; from: "inputN"; to: "sampleSize" }
		ChangeRename { condition: function(options) { return options["inputPVal"] !== undefined && options["pValue"] === undefined; }; from: "inputPVal"; to: "pValue" }
		ChangeRename { condition: function(options) { return options["inputLabels"] !== undefined && options["studyLabel"] === undefined; }; from: "inputLabels"; to: "studyLabel" }
		ChangeRename { condition: function(options) { return options["muTransform"] !== undefined && options["transformCorrelationsTo"] === undefined; }; from: "muTransform"; to: "transformCorrelationsTo" }

		ChangeRename { condition: function(options) { return options["cutoffsPVal"] !== undefined && options["modelPValueCutoffs"] === undefined; }; from: "cutoffsPVal"; to: "modelPValueCutoffs" }
		ChangeRename { condition: function(options) { return options["selectionTwosided"] !== undefined && options["modelTwoSidedSelection"] === undefined; }; from: "selectionTwosided"; to: "modelTwoSidedSelection" }
		ChangeRename { condition: function(options) { return options["tablePVal"] !== undefined && options["modelPValueFrequencyTable"] === undefined; }; from: "tablePVal"; to: "modelPValueFrequencyTable" }
		ChangeRename { condition: function(options) { return options["joinPVal"] !== undefined && options["modelAutomaticallyJoinPValueIntervals"] === undefined; }; from: "joinPVal"; to: "modelAutomaticallyJoinPValueIntervals" }
		ChangeRename { condition: function(options) { return options["effectDirection"] !== undefined && options["modelExpectedDirectionOfEffectSizes"] === undefined; }; from: "effectDirection"; to: "modelExpectedDirectionOfEffectSizes" }

		ChangeRename { condition: function(options) { return options["estimatesFE"] !== undefined && options["inferenceFixedEffectsMeanEstimatesTable"] === undefined; }; from: "estimatesFE"; to: "inferenceFixedEffectsMeanEstimatesTable" }
		ChangeRename { condition: function(options) { return options["weightsFE"] !== undefined && options["inferenceFixedEffectsEstimatedWeightsTable"] === undefined; }; from: "weightsFE"; to: "inferenceFixedEffectsEstimatedWeightsTable" }
		ChangeRename { condition: function(options) { return options["estimatesRE"] !== undefined && options["inferenceRandomEffectsMeanEstimatesTable"] === undefined; }; from: "estimatesRE"; to: "inferenceRandomEffectsMeanEstimatesTable" }
		ChangeRename { condition: function(options) { return options["heterogeneityRE"] !== undefined && options["inferenceRandomEffectsEstimatedHeterogeneityTable"] === undefined; }; from: "heterogeneityRE"; to: "inferenceRandomEffectsEstimatedHeterogeneityTable" }
		ChangeRename { condition: function(options) { return options["weightsRE"] !== undefined && options["inferenceRandomEffectsEstimatedWeightsTable"] === undefined; }; from: "weightsRE"; to: "inferenceRandomEffectsEstimatedWeightsTable" }

		ChangeRename { condition: function(options) { return options["weightFunctionFE"] !== undefined && options["plotsWeightFunctionFixedEffectsPlot"] === undefined; }; from: "weightFunctionFE"; to: "plotsWeightFunctionFixedEffectsPlot" }
		ChangeRename { condition: function(options) { return options["weightFunctionRE"] !== undefined && options["plotsWeightFunctionRandomEffectsPlot"] === undefined; }; from: "weightFunctionRE"; to: "plotsWeightFunctionRandomEffectsPlot" }
		ChangeRename { condition: function(options) { return options["weightFunctionRescale"] !== undefined && options["plotsWeightFunctionRescaleXAxis"] === undefined; }; from: "weightFunctionRescale"; to: "plotsWeightFunctionRescaleXAxis" }
		ChangeRename { condition: function(options) { return options["plotModels"] !== undefined && options["plotsMeanModelEstimatesPlot"] === undefined; }; from: "plotModels"; to: "plotsMeanModelEstimatesPlot" }
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
		functionName:	"ClassicalMetaAnalysis"
		fromVersion:	"0.95.5"
		toVersion:		"0.96.1"

		ChangeSetValue
		{
			condition:	function(options) { return options["standardErrors"] === undefined; }
			name:		"standardErrors"
			jsonValue:	false
		}
	}

	Upgrade
	{
		functionName:	"ClassicalMetaAnalysisMultilevelMultivariate"
		fromVersion:	"0.95.5"
		toVersion:		"0.96.1"

		ChangeSetValue
		{
			condition:	function(options) { return options["standardErrors"] === undefined; }
			name:		"standardErrors"
			jsonValue:	false
		}
	}

	Upgrade
	{
		functionName:	"ClassicalMantelHaenszelPeto"
		fromVersion:	"0.95.5"
		toVersion:		"0.96.1"

		ChangeSetValue
		{
			condition:	function(options) { return options["standardErrors"] === undefined; }
			name:		"standardErrors"
			jsonValue:	false
		}
	}

	Upgrade
	{
		functionName:	"ClassicalMetaAnalysis"
		fromVersion:	"0.96.4"
		toVersion:		"0.96.5"

		ChangeCopy
		{
			from:	"diagnosticsCasewiseDiagnosticsExportToDataset"
			to:		"exportDiagnosticsInfluentialCases"
		}

		ChangeJS
		{
			name:		"exportDiagnosticsInfluentialCases"
			jsFunction:	function(options)
			{
				return options["diagnosticsCasewiseDiagnosticsExportToDataset"] === true;
			}
		}

		ChangeCopy
		{
			from:	"diagnosticsCasewiseDiagnosticsExportToDataset"
			to:		"exportDiagnosticsCaseDiagnostics"
		}

		ChangeJS
		{
			name:		"exportDiagnosticsCaseDiagnostics"
			jsFunction:	function(options)
			{
				return options["diagnosticsCasewiseDiagnosticsExportToDataset"] === true &&
					options["diagnosticsCasewiseDiagnosticsExportToDatasetInfluentialIndicatorOnly"] !== true;
			}
		}

		ChangeCopy
		{
			from:	"diagnosticsCasewiseDiagnosticsExportToDataset"
			to:		"exportDiagnosticsModelImpact"
		}

		ChangeJS
		{
			name:		"exportDiagnosticsModelImpact"
			jsFunction:	function(options)
			{
				return options["diagnosticsCasewiseDiagnosticsExportToDataset"] === true &&
					options["diagnosticsCasewiseDiagnosticsExportToDatasetInfluentialIndicatorOnly"] !== true;
			}
		}

		ChangeCopy
		{
			from:	"diagnosticsCasewiseDiagnosticsExportToDataset"
			to:		"exportDiagnosticsCoefficientInfluence"
		}

		ChangeJS
		{
			name:		"exportDiagnosticsCoefficientInfluence"
			jsFunction:	function(options)
			{
				return options["diagnosticsCasewiseDiagnosticsExportToDataset"] === true &&
					options["diagnosticsCasewiseDiagnosticsExportToDatasetInfluentialIndicatorOnly"] !== true &&
					options["diagnosticsCasewiseDiagnosticsDifferenceInCoefficients"] === true;
			}
		}

		ChangeRename { condition: function(options) { return options["forestPlotAllignLeftPanel"] !== undefined && options["forestPlotAlignLeftPanel"] === undefined; }; from: "forestPlotAllignLeftPanel"; to: "forestPlotAlignLeftPanel" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeEstimates"] !== undefined && options["forestPlotSizeEstimates"] === undefined; }; from: "forestPlotRelativeSizeEstimates"; to: "forestPlotSizeEstimates" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeText"] !== undefined && options["forestPlotSizeText"] === undefined; }; from: "forestPlotRelativeSizeText"; to: "forestPlotSizeText" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeAxisLabels"] !== undefined && options["forestPlotSizeAxisLabels"] === undefined; }; from: "forestPlotRelativeSizeAxisLabels"; to: "forestPlotSizeAxisLabels" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeRow"] !== undefined && options["forestPlotSizeRow"] === undefined; }; from: "forestPlotRelativeSizeRow"; to: "forestPlotSizeRow" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeLeftPanel"] !== undefined && options["forestPlotSizeLeftPanel"] === undefined; }; from: "forestPlotRelativeSizeLeftPanel"; to: "forestPlotSizeLeftPanel" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeMiddlePanel"] !== undefined && options["forestPlotSizePlotArea"] === undefined; }; from: "forestPlotRelativeSizeMiddlePanel"; to: "forestPlotSizePlotArea" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeRightPanel"] !== undefined && options["forestPlotSizeRightPanel"] === undefined; }; from: "forestPlotRelativeSizeRightPanel"; to: "forestPlotSizeRightPanel" }
	}

	Upgrade
	{
		functionName:	"ClassicalMetaAnalysisMultilevelMultivariate"
		fromVersion:	"0.96.4"
		toVersion:		"0.96.5"

		ChangeCopy
		{
			from:	"diagnosticsCasewiseDiagnosticsExportToDataset"
			to:		"exportDiagnosticsCaseDiagnostics"
		}

		ChangeJS
		{
			name:		"exportDiagnosticsCaseDiagnostics"
			jsFunction:	function(options)
			{
				return options["diagnosticsCasewiseDiagnosticsExportToDataset"] === true;
			}
		}

		ChangeCopy
		{
			from:	"diagnosticsCasewiseDiagnosticsExportToDataset"
			to:		"exportDiagnosticsCoefficientInfluence"
		}

		ChangeJS
		{
			name:		"exportDiagnosticsCoefficientInfluence"
			jsFunction:	function(options)
			{
				return options["diagnosticsCasewiseDiagnosticsExportToDataset"] === true &&
					options["diagnosticsCasewiseDiagnosticsDifferenceInCoefficients"] === true;
			}
		}

		ChangeRename { condition: function(options) { return options["forestPlotAllignLeftPanel"] !== undefined && options["forestPlotAlignLeftPanel"] === undefined; }; from: "forestPlotAllignLeftPanel"; to: "forestPlotAlignLeftPanel" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeEstimates"] !== undefined && options["forestPlotSizeEstimates"] === undefined; }; from: "forestPlotRelativeSizeEstimates"; to: "forestPlotSizeEstimates" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeText"] !== undefined && options["forestPlotSizeText"] === undefined; }; from: "forestPlotRelativeSizeText"; to: "forestPlotSizeText" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeAxisLabels"] !== undefined && options["forestPlotSizeAxisLabels"] === undefined; }; from: "forestPlotRelativeSizeAxisLabels"; to: "forestPlotSizeAxisLabels" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeRow"] !== undefined && options["forestPlotSizeRow"] === undefined; }; from: "forestPlotRelativeSizeRow"; to: "forestPlotSizeRow" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeLeftPanel"] !== undefined && options["forestPlotSizeLeftPanel"] === undefined; }; from: "forestPlotRelativeSizeLeftPanel"; to: "forestPlotSizeLeftPanel" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeMiddlePanel"] !== undefined && options["forestPlotSizePlotArea"] === undefined; }; from: "forestPlotRelativeSizeMiddlePanel"; to: "forestPlotSizePlotArea" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeRightPanel"] !== undefined && options["forestPlotSizeRightPanel"] === undefined; }; from: "forestPlotRelativeSizeRightPanel"; to: "forestPlotSizeRightPanel" }
	}

	Upgrade
	{
		functionName:	"BayesianMetaAnalysis"
		fromVersion:	"0.96.4"
		toVersion:		"0.96.5"

		ChangeRename { condition: function(options) { return options["forestPlotAllignLeftPanel"] !== undefined && options["forestPlotAlignLeftPanel"] === undefined; }; from: "forestPlotAllignLeftPanel"; to: "forestPlotAlignLeftPanel" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeEstimates"] !== undefined && options["forestPlotSizeEstimates"] === undefined; }; from: "forestPlotRelativeSizeEstimates"; to: "forestPlotSizeEstimates" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeText"] !== undefined && options["forestPlotSizeText"] === undefined; }; from: "forestPlotRelativeSizeText"; to: "forestPlotSizeText" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeAxisLabels"] !== undefined && options["forestPlotSizeAxisLabels"] === undefined; }; from: "forestPlotRelativeSizeAxisLabels"; to: "forestPlotSizeAxisLabels" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeRow"] !== undefined && options["forestPlotSizeRow"] === undefined; }; from: "forestPlotRelativeSizeRow"; to: "forestPlotSizeRow" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeLeftPanel"] !== undefined && options["forestPlotSizeLeftPanel"] === undefined; }; from: "forestPlotRelativeSizeLeftPanel"; to: "forestPlotSizeLeftPanel" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeMiddlePanel"] !== undefined && options["forestPlotSizePlotArea"] === undefined; }; from: "forestPlotRelativeSizeMiddlePanel"; to: "forestPlotSizePlotArea" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeRightPanel"] !== undefined && options["forestPlotSizeRightPanel"] === undefined; }; from: "forestPlotRelativeSizeRightPanel"; to: "forestPlotSizeRightPanel" }
	}

	Upgrade
	{
		functionName:	"BayesianBinomialMetaAnalysis"
		fromVersion:	"0.96.4"
		toVersion:		"0.96.5"

		ChangeRename { condition: function(options) { return options["forestPlotAllignLeftPanel"] !== undefined && options["forestPlotAlignLeftPanel"] === undefined; }; from: "forestPlotAllignLeftPanel"; to: "forestPlotAlignLeftPanel" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeEstimates"] !== undefined && options["forestPlotSizeEstimates"] === undefined; }; from: "forestPlotRelativeSizeEstimates"; to: "forestPlotSizeEstimates" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeText"] !== undefined && options["forestPlotSizeText"] === undefined; }; from: "forestPlotRelativeSizeText"; to: "forestPlotSizeText" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeAxisLabels"] !== undefined && options["forestPlotSizeAxisLabels"] === undefined; }; from: "forestPlotRelativeSizeAxisLabels"; to: "forestPlotSizeAxisLabels" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeRow"] !== undefined && options["forestPlotSizeRow"] === undefined; }; from: "forestPlotRelativeSizeRow"; to: "forestPlotSizeRow" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeLeftPanel"] !== undefined && options["forestPlotSizeLeftPanel"] === undefined; }; from: "forestPlotRelativeSizeLeftPanel"; to: "forestPlotSizeLeftPanel" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeMiddlePanel"] !== undefined && options["forestPlotSizePlotArea"] === undefined; }; from: "forestPlotRelativeSizeMiddlePanel"; to: "forestPlotSizePlotArea" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeRightPanel"] !== undefined && options["forestPlotSizeRightPanel"] === undefined; }; from: "forestPlotRelativeSizeRightPanel"; to: "forestPlotSizeRightPanel" }
	}

	Upgrade
	{
		functionName:	"ClassicalMantelHaenszelPeto"
		fromVersion:	"0.96.4"
		toVersion:		"0.96.5"

		ChangeCopy
		{
			condition:	function(options) { return (options["method"] === "mantelHaenszelFrequencies" || options["method"] === "peto") && options["successesGroup1"] !== undefined && options["eventsGroup1"] === undefined; }
			from:	"successesGroup1"
			to:		"eventsGroup1"
		}
		ChangeCopy
		{
			condition:	function(options) { return (options["method"] === "mantelHaenszelFrequencies" || options["method"] === "peto") && options["successesGroup2"] !== undefined && options["eventsGroup2"] === undefined; }
			from:	"successesGroup2"
			to:		"eventsGroup2"
		}
		ChangeRemove { condition: function(options) { return options["successesGroup1"] !== undefined; }; name: "successesGroup1" }
		ChangeRemove { condition: function(options) { return options["successesGroup2"] !== undefined; }; name: "successesGroup2" }

		ChangeRename { condition: function(options) { return options["forestPlotAllignLeftPanel"] !== undefined && options["forestPlotAlignLeftPanel"] === undefined; }; from: "forestPlotAllignLeftPanel"; to: "forestPlotAlignLeftPanel" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeEstimates"] !== undefined && options["forestPlotSizeEstimates"] === undefined; }; from: "forestPlotRelativeSizeEstimates"; to: "forestPlotSizeEstimates" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeText"] !== undefined && options["forestPlotSizeText"] === undefined; }; from: "forestPlotRelativeSizeText"; to: "forestPlotSizeText" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeAxisLabels"] !== undefined && options["forestPlotSizeAxisLabels"] === undefined; }; from: "forestPlotRelativeSizeAxisLabels"; to: "forestPlotSizeAxisLabels" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeRow"] !== undefined && options["forestPlotSizeRow"] === undefined; }; from: "forestPlotRelativeSizeRow"; to: "forestPlotSizeRow" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeLeftPanel"] !== undefined && options["forestPlotSizeLeftPanel"] === undefined; }; from: "forestPlotRelativeSizeLeftPanel"; to: "forestPlotSizeLeftPanel" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeMiddlePanel"] !== undefined && options["forestPlotSizePlotArea"] === undefined; }; from: "forestPlotRelativeSizeMiddlePanel"; to: "forestPlotSizePlotArea" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeRightPanel"] !== undefined && options["forestPlotSizeRightPanel"] === undefined; }; from: "forestPlotRelativeSizeRightPanel"; to: "forestPlotSizeRightPanel" }
	}

	Upgrade
	{
		functionName:	"ForestPlot"
		fromVersion:	"0.96.4"
		toVersion:		"0.96.5"

		ChangeRename { condition: function(options) { return options["forestPlotAllignLeftPanel"] !== undefined && options["forestPlotAlignLeftPanel"] === undefined; }; from: "forestPlotAllignLeftPanel"; to: "forestPlotAlignLeftPanel" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeEstimates"] !== undefined && options["forestPlotSizeEstimates"] === undefined; }; from: "forestPlotRelativeSizeEstimates"; to: "forestPlotSizeEstimates" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeText"] !== undefined && options["forestPlotSizeText"] === undefined; }; from: "forestPlotRelativeSizeText"; to: "forestPlotSizeText" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeAxisLabels"] !== undefined && options["forestPlotSizeAxisLabels"] === undefined; }; from: "forestPlotRelativeSizeAxisLabels"; to: "forestPlotSizeAxisLabels" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeRow"] !== undefined && options["forestPlotSizeRow"] === undefined; }; from: "forestPlotRelativeSizeRow"; to: "forestPlotSizeRow" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeLeftPanel"] !== undefined && options["forestPlotSizeLeftPanel"] === undefined; }; from: "forestPlotRelativeSizeLeftPanel"; to: "forestPlotSizeLeftPanel" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeMiddlePanel"] !== undefined && options["forestPlotSizePlotArea"] === undefined; }; from: "forestPlotRelativeSizeMiddlePanel"; to: "forestPlotSizePlotArea" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeRightPanel"] !== undefined && options["forestPlotSizeRightPanel"] === undefined; }; from: "forestPlotRelativeSizeRightPanel"; to: "forestPlotSizeRightPanel" }
	}

	Upgrade
	{
		functionName:	"RobustBayesianMetaAnalysis"
		fromVersion:	"0.96.4"
		toVersion:		"0.96.5"

		ChangeRename { condition: function(options) { return options["forestPlotAllignLeftPanel"] !== undefined && options["forestPlotAlignLeftPanel"] === undefined; }; from: "forestPlotAllignLeftPanel"; to: "forestPlotAlignLeftPanel" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeEstimates"] !== undefined && options["forestPlotSizeEstimates"] === undefined; }; from: "forestPlotRelativeSizeEstimates"; to: "forestPlotSizeEstimates" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeText"] !== undefined && options["forestPlotSizeText"] === undefined; }; from: "forestPlotRelativeSizeText"; to: "forestPlotSizeText" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeAxisLabels"] !== undefined && options["forestPlotSizeAxisLabels"] === undefined; }; from: "forestPlotRelativeSizeAxisLabels"; to: "forestPlotSizeAxisLabels" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeRow"] !== undefined && options["forestPlotSizeRow"] === undefined; }; from: "forestPlotRelativeSizeRow"; to: "forestPlotSizeRow" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeLeftPanel"] !== undefined && options["forestPlotSizeLeftPanel"] === undefined; }; from: "forestPlotRelativeSizeLeftPanel"; to: "forestPlotSizeLeftPanel" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeMiddlePanel"] !== undefined && options["forestPlotSizePlotArea"] === undefined; }; from: "forestPlotRelativeSizeMiddlePanel"; to: "forestPlotSizePlotArea" }
		ChangeRename { condition: function(options) { return options["forestPlotRelativeSizeRightPanel"] !== undefined && options["forestPlotSizeRightPanel"] === undefined; }; from: "forestPlotRelativeSizeRightPanel"; to: "forestPlotSizeRightPanel" }
	}
}


