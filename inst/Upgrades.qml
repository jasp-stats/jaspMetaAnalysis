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
        ChangeRename { from: "includeConstant"; to: "includeIntercept" }

        // ClassicalMetaAnalysisStatistics.qml
        ChangeRename { from: "regressionCoefficientsEstimates"; to: "regressionCoefficientEstimate" }
        ChangeRename { from: "regressionCoefficientsConfidenceIntervals"; to: "regressionCoefficientEstimateCi" }
        ChangeRename { from: "regressionCoefficientsConfidenceIntervalsInterval"; to: "regressionCoefficientEstimateCiLevel" }
        ChangeRename { from: "test"; to: "regressionCoefficientEstimateTest" }
        ChangeRename { from: "regressionCoefficientsCovarianceMatrix"; to: "regressionCoefficientCovarianceMatrix" }
        ChangeRename { from: "modelFit"; to: "fitMeasure" }
        ChangeRename { from: "showLabels"; to: "forestPlotLabel" }
        ChangeRename { from: "forestPlotOrder"; to: "forestPlotOrdering" }
        ChangeRename { from: "rSquaredChange"; to: "funnelPlotRankTestAsymmetry" }
        ChangeRename { from: "funnelPlotAsymmetryTest"; to: "funnelPlotRegressionAsymmetryTest" }
        ChangeRename { from: "residualsParameters"; to: "residualParameter" }
	}
}