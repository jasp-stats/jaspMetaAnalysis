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
		preferredHeight: 400 * preferencesModel.uiScale

		AvailableVariablesList
		{
			name:				"allVariables"
		}

		AssignedVariablesList
		{
			name:				"effectSize"
			title:				qsTr("Effect Size")
			singleVariable:		true
			suggestedColumns:	["scale"]
		}
		AssignedVariablesList
		{	
			name:				"effectSizeStandardError"
			title:				qsTr("Effect Size Standard Error")
			singleVariable:		true
			suggestedColumns:	["scale"]
		}
		
		MA.ClassicalMetaAnalysisMethod{visible: true}
		
		AssignedVariablesList
		{
			name:				"covariates"
			title:				qsTr("Covariates")
			suggestedColumns:	["scale"]
		}
		AssignedVariablesList
		{
			name:				"factors"
			title:				qsTr("Factors")
			suggestedColumns:	["nominal"]
		}

		AssignedVariablesList
		{
			name:				"clusters"
			title:				qsTr("Clusters")
			singleVariable:		true
			suggestedColumns:	["nominal"]
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
					name:			"effectSizeModelComponents"
					title:			qsTr("Components")
					source:			["covariates","factors"]
				}

				AssignedVariablesList
				{
					name:			"effectSizeModelTerms";
					title:			qsTr("Model Terms")
					listViewType:	JASP.Interaction
				}
			}

			CheckBox
			{
				name:				"effectSizeModelIncludeIntercept";
				label:				qsTr("Include intercept")
				checked:			true
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
					name:			"heterogeneityModelComponents"
					title:			qsTr("Components")
					source:			["covariates","factors"]
				}

				AssignedVariablesList
				{
					name:			"heterogeneityModelTerms";
					title:			qsTr("Model Terms")
					listViewType:	JASP.Interaction
				}
			}

			CheckBox
			{
				name:				"heterogeneityModelIncludeIntercept";
				label:				qsTr("Include intercept")
				checked:			true
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
				checked:	true
			}

			CheckBox
			{
				name:		"metaregressionCoefficientsEstimates"
				text:		qsTr("Coefficients estimates")
				checked:	true
			}

			DropDown
			{	
				name:		"metaregressionCoefficientsTest"
				label:		qsTr("Coefficients test")
				values:		[ "z", "t", "knha"]
			}

			CheckBox
			{
				name:		"metaregressionCoefficientsCovarianceMatrix"
				text:		qsTr("Coefficients covariance matrix")
				checked:	false
			}
		}

		Group
		{
			title: qsTr("Hetereogeneity")

			CheckBox
			{
				name:		"𝜏"
				text:		qsTr("heterogeneityTau")
				checked:	true
			}

			CheckBox
			{
				name:		"𝜏²"
				text:		qsTr("heterogeneityTau2")
				checked:	false
			}

			CheckBox
			{
				name:		"I²"
				text:		qsTr("heterogeneityI2")
				checked:	false
			}

			CheckBox
			{
				name:		"H²"
				text:		qsTr("heterogeneityH2")
				checked:	false
			}

			CheckBox
			{
				name:		"Prediction Interval"
				text:		qsTr("heterogeneityPredictionInterval")
				checked:	false
			}
		}

		CheckBox
		{
			name:				"cconfidenceIntervals"
			text:				qsTr("Confidence intervals")
			childrenOnSameRow:	true

			CIField
			{
				name:		"confidenceIntervalsLevel"
			}
		}

		CheckBox
		{
			name:				"modelFit"
			text:				qsTr("Pooled estimate")
			checked:			true
		}


		Group
		{
			title:	qsTr("Model Fit")

			CheckBox
			{
				name:		"fitMeasure"
				text:		qsTr("Fit measures")
			}
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
}