import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name:			"jaspMetaAnalysis"
	title : 		qsTr("Meta-Analysis")
	description:	qsTr("Synthesize evidence across multiple studies")
	requiresData:	false
	icon:			"meta-analysis.svg"
	version:		"0.15"
	author:			"JASP Team"
	maintainer:		"JASP Team <info@jasp-stats.org>"
	website:		"jasp-stats.org"
	license:		"GPL (>= 2)"

	GroupTitle
	{
		title:	qsTr("Classical")
		icon:	"meta-analysis.svg"
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
		title:			qsTr("Selection Models")
		func:			"SelectionModels"
		requiresData:	true
	}

	GroupTitle
	{
		title: 	qsTr("Bayesian")
		icon:	"meta-analysis-bayesian.svg"
	}

	Analysis
	{
		menu:			qsTr("Meta-Analysis")
		title:			qsTr("Bayesian Meta-Analysis")
		func:			"BayesianMetaAnalysis"
		requiresData:	true
	}
	
	Analysis
	{
		title:			qsTr("Robust Bayesian Meta-Analysis")
		func:			"RobustBayesianMetaAnalysis"
		requiresData:	false
	}
}
