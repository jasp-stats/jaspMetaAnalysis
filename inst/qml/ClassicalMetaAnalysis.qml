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

	MA.ClassicalMetaAnalysisForestPlot {}

	MA.ClassicalMetaAnalysisBubblePlot {}

	MA.ClassicalMetaAnalysisDiagnostics {}

	MA.ClassicalMetaAnalysisAdvanced
	{
		id:		sectionAdvanced
	}
}
