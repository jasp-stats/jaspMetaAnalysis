import QtQuick
import JASP.Module

Description
{
	name:			"jaspMetaAnalysis"
	title : 		qsTr("Meta-Analysis")
	description:	qsTr("Synthesize evidence across multiple studies")
	requiresData:	true
	preloadData:	true
	icon:			"meta-analysis.svg"
	version			: "0.20.0"
	author:			"JASP Team"
	maintainer:		"JASP Team <info@jasp-stats.org>"
	website:		"jasp-stats.org"
	license:		"GPL (>= 2)"


	GroupTitle
	{
		title:		qsTr("Miscellaneous")
		icon:		"meta-analysis.svg"
	}

	Analysis
	{
		menu:			qsTr("Effect Size Computation")
		title:			qsTr("Effect Size Computation")
		func:			"EffectSizeComputation"
	}

	Analysis
	{
		menu:			qsTr("Funnel Plot")
		title:			qsTr("Funnel Plot")
		func:			"FunnelPlot"
	}

	GroupTitle
	{
		title:		qsTr("Classical")
		icon:		"meta-analysis.svg"
	}

	Analysis
	{
		menu:			qsTr("Meta-Analysis")
		title:			qsTr("Classical Meta-Analysis")
		func:			"ClassicalMetaAnalysis"
	}

	Analysis
	{
		menu:			qsTr("Meta-Analysis (Multilevel/Multivariate)")
		title:			qsTr("Classical Meta-Analysis (Multilevel/Multivariate)")
		func:			"ClassicalMetaAnalysisMultilevelMultivariate"
	}

	Analysis
	{
		menu:			qsTr("Prediction Model Performance")
		title:			qsTr("Classical Prediction Model Performance")
		func:			"ClassicalPredictionPerformance"
	}
  
  	Analysis
	{
		title:			qsTr("WAAP-WLS")
		func:			"WaapWls"
	}

	Analysis
	{
		title:			qsTr("PET-PEESE")
		func:			"PetPeese"
	}
	
	Analysis
	{
		title:			qsTr("Selection Models")
		func:			"SelectionModels"
	}

	Analysis
	{
		title:			qsTr("Meta-Analytic SEM")
		func:			"MetaAnalyticSem"
		requiresData:	true
	}

	Analysis
	{
		title:			qsTr("SEM-Based Meta-Analysis")
		func:			"SemBasedMetaAnalysis"
		preloadData:	false
		requiresData:	true
	}

	GroupTitle
	{
		title:	 	qsTr("Bayesian")
		icon:		"meta-analysis-bayesian.svg"
	}

	Analysis
	{
		menu:			qsTr("Meta-Analysis")
		title:			qsTr("Bayesian Meta-Analysis")
		func:			"BayesianMetaAnalysis"
	}

	Analysis
	{
		menu:			qsTr("Meta-Analysis (Deprecated)")
		title:			qsTr("Bayesian Meta-Analysis (Deprecated)")
		func:			"BayesianMetaAnalysisDeprecated"
	}

	Analysis
	{
		menu:			qsTr("Binomial Meta-Analysis")
		title:			qsTr("Bayesian Binomial Meta-Analysis")
		func:			"BayesianBinomialMetaAnalysis"
	}

	Analysis
	{
		menu:			qsTr("Penalized Meta-Analysis")
		title:			qsTr("Penalized Meta-Analysis")
		func:			"PenalizedMetaAnalysis"
		preloadData:	false
	}
	
	Analysis
	{
		menu:			qsTr("Prediction Model Performance")
		title:			qsTr("Bayesian Prediction Model Performance")
		func:			"BayesianPredictionPerformance"
	}
	
	Analysis
	{
		title:			qsTr("Robust Bayesian Meta-Analysis")
		func:			"RobustBayesianMetaAnalysis"
	}
}
