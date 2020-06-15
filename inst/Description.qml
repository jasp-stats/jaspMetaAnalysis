import QtQuick 		2.12
import JASP.Module 	1.0

Description
{
	name:			"MetaAnalysis"
	title : 		qsTr("Meta-Analysis")
	description:	qsTr("Meta-Analysis module.")
	icon:			"meta-analysis.svg"
	version:		"0.13"
	author:			"JASP Team"
	maintainer:		"JASP Team <info@jasp-stats.org>"
	website:		"jasp-stats.org"
	license:		"GPL (>= 2)"

	Analysis
	{
		title:	qsTr("Classical Meta-Analysis")
		func:	"ClassicalMetaAnalysis"
	}

	Separator{}

	Analysis
	{
		title: 	qsTr("Bayesian Meta-Analysis")
		func:	"BayesianMetaAnalysis"
	}
	
	Analysis
	{
		title: 		qsTr("Robust Bayesian Meta-Analysis (beta)")
		function:		"RobustBayesianMetaAnalysis"
		requiresData:	false
	}
}
