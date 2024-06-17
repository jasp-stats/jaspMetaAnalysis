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
		preferredHeight: 425 * preferencesModel.uiScale

		AvailableVariablesList
		{
			name:				"allVariables"
		}

		AssignedVariablesList
		{
			name:				"effectSize"
			title:				qsTr("Effect Size")
			singleVariable:		true
			allowedColumns:		["scale"]
		}
		AssignedVariablesList
		{	
			name:				"effectSizeStandardError"
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
			values: [
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
				{ label: qsTr("Generalized Q-stat (MU)"), value: "qeneralizedQStatMu"	}
			]
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
			title:				qsTr("Predictors")
			allowedColumns:		["nominal", "scale"]
		}

		AssignedVariablesList
		{
			name:				"clustering"
			title:				qsTr("Clustering")
			singleVariable:		true
			allowedColumns:		["nominal"]
		}

		AssignedVariablesList
		{
			name:				"studyLabel"
			title:				qsTr("Study Label")
			singleVariable:		true
			suggestedColumns:	["nominal"]
		}
	}

	Section
	{
		title:	qsTr("Model")

		Group
		{
			title: qsTr("Effect size model")

			VariablesForm
			{
				preferredHeight:	150 * preferencesModel.uiScale

				AvailableVariablesList
				{
					name:			"effectSizeModelAvailableComponents"
					title:			qsTr("Available Components")
					source:			["predictors"]
				}

				AssignedVariablesList
				{
					name:			"effectSizeModelTerms";
					title:			qsTr("Model Terms")
					listViewType:	JASP.Interaction
				}

				CheckBox
				{
					name:				"effectSizeModelIncludeIntercept";
					label:				qsTr("Include intercept")
					checked:			true
				}
			}
		}

		Group
		{
			title: qsTr("Heterogeneity model")

			VariablesForm
			{
				preferredHeight:	150 * preferencesModel.uiScale

				AvailableVariablesList
				{
					name:			"heterogeneityModelAvailableComponents"
					title:			qsTr("Available Components")
					source:			["predictors"]
				}

				// TODO: start empty
				AssignedVariablesList
				{
					name:			"heterogeneityModelTerms";
					title:			qsTr("Model Terms")
					listViewType:	JASP.Interaction
				}

				CheckBox
				{
					name:				"heterogeneityModelIncludeIntercept";
					label:				qsTr("Include intercept")
					checked:			true
				}
			}
		}
	}

	Section
	{
		title:	qsTr("Statistics")

		Group
		{
			title: qsTr("Meta-Regression")

			CheckBox
			{
				name:		"metaregressionTermsTests"
				text:		qsTr("Terms tests")
				checked:	true // TODO: Enable once at least one term is selected
			}

			CheckBox
			{
				name:		"metaregressionCoefficientEstimates"
				text:		qsTr("Coefficient estimates")
				checked:	true
			}

			CheckBox
			{
				name:		"metaregressionCoefficientCorrelationMatrix"
				text:		qsTr("Coefficient correlation matrix")
				checked:	false
			}
		}

		Group
		{
			title:		qsTr("Hetereogeneity")
			columns:	2
			enabled:	method.value != "fixedEffects" && method.value != "equalEffects"

			CheckBox
			{
				text:		qsTr("𝜏")
				name:		"heterogeneityTau"
				checked:	true
			}

			CheckBox
			{
				text:		qsTr("𝜏²")
				name:		"heterogeneityTau2"
				checked:	true
			}

			CheckBox
			{
				text:		qsTr("I²")
				name:		"heterogeneityI2"
				checked:	false
			}

			CheckBox
			{
				text:		qsTr("H²")
				name:		"heterogeneityH2"
				checked:	false
			}

			CheckBox
			{
				text:		qsTr("Prediction interval")
				name:		"heterogeneityPredictionInterval"
				checked:	false
			}
		}

		CheckBox
		{
			name:				"confidenceIntervals"
			text:				qsTr("Confidence intervals")
			checked:			true
			childrenOnSameRow:	true

			CIField
			{
				name:		"confidenceIntervalsLevel"
			}
		}

		CheckBox
		{
			name:		"fitMeasures"
			text:		qsTr("Fit measures")
		}
	}

	Section
	{
		title:	qsTr("Plots")

		CheckBox
		{
			id:			forestPlot
			name: 		"forestPlot"
			text: 		qsTr("Forest plot")
			
			CheckBox
			{
				name:		"forestPlotLabel"
				text:		qsTr("Show labels")
			}

			DropDown
			{
				name:			"forestPlotOrder"
				label:			qsTr("Ordering")
				currentIndex:	1
				values: [
					{ label: qsTr("Year (ascending)")			, value: "yearAscending"			},
					{ label: qsTr("Year (descending)")			, value: "yearDescending"			},
					{ label: qsTr("Effect size (ascending)")	, value: "effectSizeAscending"		},
					{ label: qsTr("Effect size (descending)")	, value: "effectSizeDescending"		}
				]
			}
		
		}
	}

	MA.ClassicalMetaAnalysisDiagnostics{}

	Section
	{
		title:	qsTr("Advanced")

		Group
		{
			title:	qsTr("Clustering")

			CheckBox
			{
				name:		"clusteringUseClubSandwich"
				text:		qsTr("Use clubSandwich")
				checked:	true
			}

			CheckBox
			{
				name:		"clusteringSmallSampleCorrection"
				text:		qsTr("Small sample correction")
				checked:	true
			}
		}

		Group
		{
			title:	qsTr("Fix Parameters")

			CheckBox // TODO: remove all items form heterogeneity model when specified
			{
				name:	"fixParametersTau2"
				text:	qsTr("𝜏²")
				childrenOnSameRow:	true

				FormulaField
				{
					label: 				""
					name: 				"fixParametersTau2Value"
					value:				"1"
					min: 				0
					inclusive: 			JASP.None
				}
			}

			CheckBox
			{
				name:	"fixParametersWeights"
				text:	qsTr("Weights")
				childrenOnSameRow:	true

				DropDown
				{
					label: 				""
					name: 				"fixParametersWeightsVariable"
					source:				"allVariables"
				}
			}
		}

		Group
		{
			title:	qsTr("Add Omibus Moderator Test")
			
			CheckBox
			{ // TODO: enable only if metaregression specified
				text:	qsTr("Effect size coefficients")
				name:	"addOmnibusModeratorTestEffectSizeCoefficients"
				childrenOnSameRow:	false

				// TODO: remove check for a number
				TextField
				{
					label: 				""
					name: 				"addOmnibusModeratorTestEffectSizeCoefficientsValues"
					value:				"c(1, 2)"
				}
			}

			CheckBox
			{
				text:	qsTr("Heterogeneity coefficients")
				name:	"addOmnibusModeratorTestHeterogeneityCoefficients"
				childrenOnSameRow:	false

				TextField
				{
					label: 				""
					name: 				"addOmnibusModeratorTestHeterogeneityCoefficientsValues"
					value:				"c(1, 2)"
				}
			}
		}
	}
}