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
import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0
import "../qml/qml_components" as MA

Form
{
	VariablesForm
	{
		preferredHeight: 450 * preferencesModel.uiScale

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
		}
		AssignedVariablesList
		{
			name:				"effectSizeStandardError"
			id:					effectSizeStandardError
			title:				qsTr("Effect Size Standard Error")
			singleVariable:		true
			allowedColumns:		["scale"]
		}

		DropDown
		{
			name:			"method"
			id:				method
			label:			qsTr("Method")
			startValue:		"restrictedML"
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
		}

		AssignedVariablesList
		{
			name:				"predictors"
			id:					predictors
			title:				qsTr("Predictors")
			allowedColumns:		["nominal", "scale"]
		}

		AssignedVariablesList
		{
			name:				"clustering"
			id:					clustering
			title:				qsTr("Clustering")
			singleVariable:		true
			allowedColumns:		["nominal"]
		}

		AssignedVariablesList
		{
			name:				"studyLabels"
			title:				qsTr("Study Labels")
			singleVariable:		true
			allowedColumns:		["nominal"]
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

	MA.ClassicalMetaAnalysisAdvanced {}
}
