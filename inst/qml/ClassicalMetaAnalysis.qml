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
	info: qsTr("Classical meta-analysis allows you to conduct a meta-analysis using the classical approach. " + 
	"It provides options for fixed and random effects models, as well as meta-regression, location-scale models, and subgroup analysis. " +
	"Additional options include the ability to specify clustering for robust variance estimation, permutation tests, and generating the metafor package R code. " +
	"The results include estimates of effect sizes, heterogeneity, moderation, and various plots to visualize the results.\n\n" + 
	"See [this tutorial](https://doi.org/10.48550/arXiv.2509.09845) for a detailed introduction to the module.")
	infoBottom: "## " + qsTr("References") + "\n" +
	"- Bartoš F, Wagenmakers EJ, & Viechtbauer W (2025). “Meta-analysis with JASP, Part I: Classical approaches.” _ArXiv Preprint_. https://doi.org/10.48550/arXiv.2509.09845\n" + 
	"- Viechtbauer W (2010). “Conducting meta-analyses in R with the metafor package.” _Journal of Statistical Software, 36_(3), 1–48. https://doi.org/10.18637/jss.v036.i03\n" +
	"- Viechtbauer W, López-López JA, Sánchez-Meca J, Marín-Martínez F (2015). “A comparison of procedures to test for moderators in mixed-effects meta-regression models.” _Psychological Methods, 20_(3), 360–374. https://doi.org/10.1037/met0000023\n" +
	"- Viechtbauer W, López-López JA (2022). “Location-scale models for meta-analysis.” _Research Synthesis Methods, 13_(6), 697–715. https://doi.org/10.1002/jrsm.1562\n" +
	"- Viechtbauer W (2025). _metafor: Meta-Analysis Package for R_. R package version 4.8-0 Available at: <https://CRAN.R-project.org/package=metafor>.\n" + 
	"## " + qsTr("R Packages") + "\n" +
	"- metafor"

	VariablesForm
	{
		preferredHeight: 500 * preferencesModel.uiScale

		AvailableVariablesList
		{
			name:				"allVariables"
		}

		AssignedVariablesList
		{
			name:				"effectSize"
			id:					effectSize
			title:				qsTr("Effect Size")
			singleVariable:		true
			allowedColumns:		["scale"]
			info: qsTr("Variable containing the observed effect sizes.")
		}
		
		AssignedVariablesList
		{
			name:				"effectSizeStandardError"
			id:					effectSizeStandardError
			title:				qsTr("Effect Size Standard Error")
			singleVariable:		true
			allowedColumns:		["scale"]
			info: qsTr("Variable containing the standard errors corresponding to the effect sizes.")
		}

		DropDown
		{
			name:			"method"
			id:				method
			label:			qsTr("Method")
			startValue:		"restrictedML"
			info: qsTr("Method used to estimate heterogeneity (tau-squared) in the meta-analysis. The available methods depend on the inclusion of heterogeneity model terms.")
			values:			(function() {
				if (sectionModel.heterogeneityModelTermsCount == 0) {
					return [
						{ label: qsTr("Equal Effects")			, value: "equalEffects"		},
						{ label: qsTr("Fixed Effects")			, value: "fixedEffects"		},
						{ label: qsTr("Maximum Likelihood")		, value: "maximumLikelihood"},
						{ label: qsTr("Restricted ML")			, value: "restrictedML"		},
						{ label: qsTr("DerSimonian-Laird")		, value: "derSimonianLaird"	},
						{ label: qsTr("Hedges")					, value: "hedges"			},
						{ label: qsTr("Hunter-Schmidt")			, value: "hunterSchmidt"	},
						{ label: qsTr("Hunter-Schmidt (SSC)")	, value: "hunterSchmidtSsc"	},
						{ label: qsTr("Sidik-Jonkman")			, value: "sidikJonkman"		},
						{ label: qsTr("Empirical Bayes")		, value: "empiricalBayes"	},
						{ label: qsTr("Paule-Mandel")			, value: "pauleMandel"		},
						{ label: qsTr("Paule-Mandel (MU)")		, value: "pauleMandelMu"	},
						{ label: qsTr("Generalized Q-stat")		, value: "qeneralizedQStat"	},
						{ label: qsTr("Generalized Q-stat (MU)"), value: "qeneralizedQStatMu"}
					];
				} else {
					return [
						{ label: qsTr("Maximum Likelihood")		, value: "maximumLikelihood"},
						{ label: qsTr("Restricted ML")			, value: "restrictedML"		},
						{ label: qsTr("Empirical Bayes")		, value: "empiricalBayes"	}
					];
				}})()
		}

		DropDown
		{
			name:		"fixedEffectTest"
			label:		qsTr("Fixed effect test")
			startValue:	"knha"
			values:		[ "z", "t", "knha"]
			info: qsTr("Method for testing the model coefficients: 'z' uses standard normal approximation, 't' uses t-distribution, and 'knha' uses the Knapp and Hartung adjustment (default).")
		}

		AssignedVariablesList
		{
			name:				"predictors"
			id:					predictors
			title:				qsTr("Predictors")
			allowedColumns:		["nominal", "scale"]
			allowTypeChange:	true
			info: qsTr("Variables to include as predictors (moderators) in the meta-regression model. See the 'Model' section for the meta-regression specification details.")
		}

		AssignedVariablesList
		{
			name:				"clustering"
			id:					clustering
			title:				qsTr("Clustering")
			singleVariable:		true
			enabled:			!sectionAdvanced.permutationTestChecked
			allowedColumns:		["nominal"]
			info: qsTr("Variable indicating clustering of effect sizes for robust variance estimation. If the variable is specified, a cluster-robust tests and confidence intervals are provided. See 'Details' section for additional clustering options. This option is disabled when permutation tests are selected.")
		}

		AssignedVariablesList
		{
			name:				"studyLabels"
			title:				qsTr("Study Labels")
			singleVariable:		true
			allowedColumns:		["nominal"]
			info: qsTr("Variable containing labels for the studies. Used for labeling outputs and plots.")
		}

		AssignedVariablesList
		{
			name:				"subgroup"
			id:					subgroup
			title:				qsTr("Subgroup")
			singleVariable:		true
			allowedColumns:		["nominal"]
			info: qsTr("Variable indicating subgroup stratification. For each subgroup, an independent model is fitted to the corresponding data set subset.")
		}
	}

	MA.ClassicalMetaAnalysisModel
	{
		id:		sectionModel
	}

	MA.ClassicalMetaAnalysisStatistics {}

	MA.ClassicalMetaAnalysisEstimatedMarginalMeans {}

	MA.ForestPlot {}

	MA.BubblePlot {}

	MA.ClassicalMetaAnalysisDiagnostics {}

	MA.ClassicalMetaAnalysisAdvanced
	{
		id:		sectionAdvanced
	}
}
