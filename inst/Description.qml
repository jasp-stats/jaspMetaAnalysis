import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name:			"jaspMetaAnalysis"
	title : 		qsTr("Meta-Analysis")
	description:	qsTr("Synthesize evidence across multiple studies")
	requiresData:	false
	icon:			"meta-analysis.svg"
	version:		"0.13"
	author:			"JASP Team"
	maintainer:		"JASP Team <info@jasp-stats.org>"
	website:		"jasp-stats.org"
	license:		"GPL (>= 2)"

	Analysis
	{
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

	Separator{}

	Analysis
	{
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
