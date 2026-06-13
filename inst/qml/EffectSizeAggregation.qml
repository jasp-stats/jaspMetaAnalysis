//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP
import "../qml/qml_components" as MA

Form
{
	info: qsTr("Effect Size Aggregation allows you to combine multiple effect sizes or outcomes from the same study into a single aggregate effect size." + 
	"This is useful when a study reports multiple outcomes that should be treated as a single unit. Note that multilevel and multivariate models available in the Classical Meta-Analysis (Multilevel/Multivariate) option are preferable for analyzing such structures.")
	infoBottom: "## " + qsTr("References") + "\n" +
	"- Bartoš F, Wagenmakers EJ, & Viechtbauer W (2025). “Meta-analysis with JASP, Part I: Classical approaches.” _ArXiv Preprint_. https://doi.org/10.48550/arXiv.2509.09845\n" + 
	"- Viechtbauer W (2010). “Conducting meta-analyses in R with the metafor package.” _Journal of Statistical Software, 36_(3), 1–48. https://doi.org/10.18637/jss.v036.i03\n" +
	"- Viechtbauer W (2025). _metafor: Meta-Analysis Package for R_. R package version 4.8-0 Available at: <https://CRAN.R-project.org/package=metafor>.\n" + 
	"## " + qsTr("R Packages") + "\n" +
	"- metafor"

	VariablesForm
	{
		preferredHeight:	200 * preferencesModel.uiScale

		AvailableVariablesList
		{
			name:	"allVariables"
		}

		AssignedVariablesList
		{
			name:				"effectSize"
			title:				qsTr("Effect Size")
			singleVariable:		true
			allowedColumns:		["scale"]
			info:				qsTr("Variable containing the observed effect sizes.")
		}

		AssignedVariablesList
		{
			name:				"effectSizeStandardError"
			title:				qsTr("Effect Size Standard Error")
			singleVariable:		true
			allowedColumns:		["scale"]
			info:				qsTr("Variable containing the standard errors of the effect sizes.")
		}

		AssignedVariablesList
		{
			name:				"cluster"
			title:				qsTr("Cluster")
			singleVariable:		true
			allowedColumns:		["nominal"]
			info:				qsTr("Variable identifying the cluster (e.g., study) to aggregate effect sizes within.")
		}
	}

	MA.ClassicalMetaAnalysisMultivariate
	{
		analysisType:	"effectSizeAggregation"
	}

	Section
	{
		title:	qsTr("Options")

		Group
		{

			CheckBox
			{
				name:		"showMetaforRCode"
				text:		qsTr("Show metafor R code")
				info:		qsTr("Display the underlying R code used by the metafor package to aggregate the effect sizes.")
			}

			CheckBox
			{
				id:			computeSamplingVariance
				name:		"computeSamplingVariance"
				text:		qsTr("Compute sampling variance")
				checked:	false
			}

			CheckBox
			{
				name:		"weighted"
				text:		qsTr("Inverse-variance weighting")
				checked:	true
				info:		qsTr("Use inverse-variance weighting when aggregating effect sizes.")
			}

			CheckBox
			{
				id:			addClusterSize
				name:		"addClusterSize"
				text:		qsTr("Add cluster size column")
				checked:	false
				info:		qsTr("Add a column with the number of effect sizes per cluster.")
			}
		}

		Group
		{
			title:	qsTr("Aggregated Column Names")

			TextField
			{
				name:			"aggregatedColumnNamesEffectSize"
				label:			qsTr("Effect size")
				defaultValue:	"aggregated effect size"
			}

			TextField
			{
				name:			"aggregatedColumnNamesStandardError"
				label:			qsTr("Standard error")
				defaultValue:	"aggregated standard error"
				visible:		!computeSamplingVariance.checked
			}

			TextField
			{
				name:			"aggregatedColumnNamesSamplingVariance"
				label:			qsTr("Sampling variance")
				defaultValue:	"aggregated sampling variance"
				visible:		computeSamplingVariance.checked
			}

			TextField
			{
				name:			"aggregatedColumnNamesClusterSize"
				label:			qsTr("Cluster size")
				defaultValue:	"cluster size"
				visible:		addClusterSize.checked
			}
		}
	}
}
