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
        ChangeRename { from: "regressionCoefficientsEstimates"; to: "regressionCoefficientEstimate" }
        ChangeRename { from: "regressionCoefficientsConfidenceIntervals"; to: "regressionCoefficientEstimateCi" }
        ChangeRename { from: "regressionCoefficientsConfidenceIntervalsInterval"; to: "regressionCoefficientEstimateCiLevel" }
        ChangeRename { from: "test"; to: "regressionCoefficientEstimateTest" }
        ChangeRename { from: "regressionCoefficientsCovarianceMatrix"; to: "regressionCoefficientCovarianceMatrix" }
        ChangeRename { from: "modelFit"; to: "fitMeasure" }
        ChangeRename { from: "rSquaredChange"; to: "funnelPlotRankTestAsymmetry" }
        ChangeRename { from: "funnelPlotAsymmetryTest"; to: "funnelPlotRegressionAsymmetryTest" }
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
        ChangeRename { from: "checkForestPlot"; to: "forestPlot" }
        ChangeRename { from: "forestPlot"; to: "forestPlotEffect" }
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
        ChangeRename { from: "orderForest"; to: "forestPlotOrder" }
        ChangeJS
        {
            name:		"forestPlotOrder"
            jsFunction:	function(options)
            {
                switch(options["forestPlotOrder"])
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
	}
}