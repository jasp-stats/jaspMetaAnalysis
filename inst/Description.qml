import QtQuick
import JASP.Module

Description
{
	name:			"jaspMetaAnalysis"
	title : 		qsTr("Meta-Analysis")
	description:	qsTr("Synthesize evidence across multiple studies")
	requiresData:	false
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
		requiresData:	true
	}

	Analysis
	{
		menu:			qsTr("Funnel Plot")
		title:			qsTr("Funnel Plot")
		func:			"FunnelPlot"
		requiresData:	true
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
		requiresData:	true
	}

	Analysis
	{
		menu:			qsTr("Meta-Analysis (Multilevel/Multivariate)")
		title:			qsTr("Classical Meta-Analysis (Multilevel/Multivariate)")
		func:			"ClassicalMetaAnalysisMultilevelMultivariate"
		requiresData:	true
	}

	Analysis
	{
		menu:			qsTr("Prediction Model Performance")
		title:			qsTr("Classical Prediction Model Performance")
		func:			"ClassicalPredictionPerformance"
    	requiresData:	true
	}
  
  	Analysis
	{
		title:			qsTr("WAAP-WLS")
		func:			"WaapWls"
		requiresData:	true
	}

	Analysis
	{
		title:			qsTr("PET-PEESE")
		func:			"PetPeese"
		requiresData:	true
	}
	
	Analysis
	{
		title:			qsTr("Selection Models")
		func:			"SelectionModels"
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
		requiresData:	true
		preloadData:	false
	}

	Analysis
	{
		menu:			qsTr("Binomial Meta-Analysis")
		title:			qsTr("Bayesian Binomial Meta-Analysis")
		func:			"BayesianBinomialMetaAnalysis"
		requiresData:	true
		preloadData:	false
	}

	Analysis
	{
		menu:			qsTr("Penalized Meta-Analysis")
		title:			qsTr("Penalized Meta-Analysis")
		func:			"PenalizedMetaAnalysis"
		requiresData:	true
		preloadData:	false
	}
	
	Analysis
	{
		menu:			qsTr("Prediction Model Performance")
		title:			qsTr("Bayesian Prediction Model Performance")
		func:			"BayesianPredictionPerformance"
		requiresData:	true
	}
	
	Analysis
	{
		title:			qsTr("Robust Bayesian Meta-Analysis")
		func:			"RobustBayesianMetaAnalysis"
		requiresData:	false
		preloadData:	false
	}
}
