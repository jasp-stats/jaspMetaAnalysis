import QtQuick		2.12
import JASP.Module	1.0

Upgrades
{
	Upgrade
	{
		functionName:	"ClassicalMetaAnalysis"
		fromVersion:	"0.15"
		toVersion:		"0.16.4"

		// ClassicalMetaAnalysis.qml
		ChangeRename { from: "dependent"; to: "effectSize" }
		ChangeRename { from: "wlsWeights"; to: "effectSizeStandardError" }
		ChangeRename { from: "studyLabels"; to: "studyLabel" }
		ChangeRename { from: "includeConstant"; to: "includeIntercept" }

		// ClassicalMetaAnalysisStatistics.qml
		ChangeRename { from: "regressionCoefficientsEstimates"; to: "estimate" }
		ChangeRename { from: "regressionCoefficientsConfidenceIntervals"; to: "estimateCi" }
		ChangeRename { from: "regressionCoefficientsConfidenceIntervalsInterval"; to: "estimateCiLevel" }
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
		fromVersion:	"0.15"
		toVersion:		"0.16.4"

    // BayesianMetaAnalysis.qml
		ChangeRename { from: "standardError"; to: "effectSizeStandardError" }
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
		ChangeRename { from: "plotPosterior"; to: "priorAndPosterior" }
		ChangeRename { from: "addInfo"; to: "priorAndPosteriorInfo" }
		ChangeRename { from: "addLines"; to: "priorAndPosteriorFixedAndRandom" }
		ChangeRename { from: "shade"; to: "priorAndPosteriorShade" }
		ChangeRename { from: "plotSequential"; to: "sequentialPlotBayesFactor" }
		ChangeRename { from: "plotSeqPM"; to: "sequentialPlotModelProbability" }

		// BayesianMetaAnalysisPriors.qml
		ChangeRename { from: "priorES"; to: "priorEffectSize" }
		ChangeRename { from: "informativeCauchyLocation"; to: "cauchyLocation" }
		ChangeRename { from: "informativeCauchyScale"; to: "cauchyScale" }
		ChangeRename { from: "informativeNormalMean"; to: "normalMean" }
		ChangeRename { from: "informativeNormalStd"; to: "normalStd" }
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
		functionName:	"BayesianMetaAnalysis"
		fromVersion:	"0.15"
		toVersion:		"0.16.4"

        // BayesianMetaAnalysis.qml
		ChangeRename { from: "standardError"; to: "effectSizeStandardError" }
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
	}
}