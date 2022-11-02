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
			name: "allVariables"
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
			name:				"effectSizeSe"
			title:				qsTr("Effect Size Standard Error")
			singleVariable:		true
			suggestedColumns:	["scale"]
		}

		DropDown
		{
			name:			"method"
			id:				method
			label:			qsTr("Method")
			currentIndex:	2

			values: [
				{ label: qsTr("Horseshoe"),	value: "horseshoe"		},
				{ label: qsTr("Lasso"),		value: "lasso"	}
			]
		}

/*		AssignedVariablesList
		{
			name:				"studyLabels"
			title:				qsTr("Study Labels")
			singleVariable:		true
			suggestedColumns:	["ordinal", "nominal"]
		}
*/
		AssignedVariablesList
		{
			id:					covariates
			name:				"covariates"
			title:				qsTr("Covariates")
			suggestedColumns:	["scale"]
		}

		AssignedVariablesList
		{
			name:				"factors"
			title:				qsTr("Factors")
			suggestedColumns:	["ordinal", "nominal"]
		}

		AssignedVariablesList
		{
			name:				"clustering"
			title:				qsTr("Clustering")
			singleVariable:		true
			suggestedColumns:	["ordinal", "nominal"]
		}	
	}

	Section
	{
		title: qsTr("Model")
		columns:	1

		VariablesForm
		{
			preferredHeight: 150 * preferencesModel.uiScale

			AvailableVariablesList
			{
				name:		"components"
				title:		qsTr("Components")
				source:		["covariates","factors"]
			}

			AssignedVariablesList
			{
				name:			"modelTerms"
				title:			qsTr("Model Terms")
				listViewType:	JASP.Interaction
			}
		}

		CheckBox
		{
			name:		"interceptTerm"
			label:		qsTr("Include intercept")
			checked:	true
		}

		CheckBox
		{
			name:		"scalePredictors"
			label:		qsTr("Scale predictors")
			checked:	true
		}

		/* Requires a different functionality of the pema interface 
		VariablesList
		{
			title:				qsTr("Rescale continuous predictors")
			id:					modelRescale
			name:				"modelRescale"
			source:				"covariates"
			listViewType:		JASP.AssignedVariables
			preferredHeight:	120 * preferencesModel.uiScale
			draggable:			false

			rowComponent:	CheckBox
			{
				name:		"covariate"
				checked:	true
			}
		}
		*/
	}

	Section
	{
		title: qsTr("Priors")

		Group
		{
			title:		qsTr("Horseshoe")
            visible:	method.currentValue == "horseshoe"
			
			DoubleField
			{
				name:			"horseshoePriorDf"
				label:			qsTr("Df")
				defaultValue:	1
				min:			1
				inclusive:		JASP.MinOnly
			}

			DoubleField
			{
				name:			"horseshoePriorScale"
				label:			qsTr("Scale")
				defaultValue:	1
				min:			0
				inclusive:		JASP.None
			}
		}

		Group
		{
			title:		qsTr("Lasso")
			visible:	method.currentValue == "lasso"
			
			DoubleField
			{
				name:			"lassoPriorDf"
				label:			qsTr("Df")
				defaultValue:	1
				min:			1
				inclusive:		JASP.MinOnly
			}

			DoubleField
			{
                name:			"lassoPriorDfGlobal"
				label:			qsTr("Df (global)")
				defaultValue:	1
				min:			1
				inclusive:		JASP.MinOnly
			}

			DoubleField
			{
                name:			"lassoPriorDfSlab"
				label:			qsTr("Df (slab)")
				defaultValue:	4
				min:			1
				inclusive:		JASP.MinOnly
			}

			DoubleField
			{
                name:			"lassoPriorScaleGlobal"
				label:			qsTr("Scale (global)")
				defaultValue:	1
				min:			0
				inclusive:		JASP.None
			}

			DoubleField
			{
                name:			"lassoPriorScaleSlab"
				label:			qsTr("Scale (slab)")
				defaultValue:	1
				min:			0
				inclusive:		JASP.None
			}
		}
	}

	Section
	{
		title:		qsTr("Inference")
		columns:	1

		Group
		{
			CheckBox
			{
				name:		"estimatesCoefficients"
				label:		qsTr("Estimates")
				checked:	true
			}

			CheckBox
			{
				name:		"estimatesTau"
				label:		qsTr("Heterogeneity")
				checked:	false

				CheckBox
				{
					name:		"estimatesI2"
					label:		qsTr("I²")
					checked:	false
				}
			}
		}

		Group
		{
			title:				qsTr("Posterior plots")
			
			VariablesForm
			{
				preferredHeight:	250 * preferencesModel.uiScale
				id:					availableModelComponentsForms
				property var alwaysAvailable:
					[
						{ label:	"Intercept",		value: "Intercept"},
						{ label:	"Heterogeneity",	value: "Heterogeneity"}
					]

				AvailableVariablesList
				{
					name:	"availableModelComponentsPlot"
					title:	qsTr("Model terms")
					source:	["modelTerms", {values: availableModelComponentsForms.alwaysAvailable}]
				}

				AssignedVariablesList
				{
					name:	"plotPosterior"
					title:	qsTr("Plotted term")
				}
			}
		}

	}


	Section
	{
		title:		qsTr("MCMC diagnostics")

		VariablesForm
		{
			preferredHeight: 200 * preferencesModel.uiScale
			id:				diagnosticsForms
			property var alwaysAvailable:
				[
					{ label:	"Intercept",		value: "Intercept"},
					{ label:	"Heterogeneity",	value: "Heterogeneity"}
				]

			AvailableVariablesList
			{
				name:	"diagnosticsVariables"
				title:	qsTr("Model terms")
				
				source:	["modelTerms", {values: diagnosticsForms.alwaysAvailable}]
			}

			AssignedVariablesList
			{
				singleVariable:	true
                name:			"scatterVariableX"
                title:			samplingPlot.currentValue == "scatter" ? qsTr("Horizontal axis") : qsTr("Plotted term")
			}

			AssignedVariablesList
			{
				singleVariable:	true
                name:			"scatterVariableY"
				title:			qsTr("Vertical axis")
                visible:		samplingPlot.currentValue == "scatter"
				onVisibleChanged: if (!visible && count > 0) itemDoubleClicked(0)
			}
		}

		DropDown
		{
			name:	"diagnosticsType"
			id:		samplingPlot
			label:	qsTr("Plot type")
			values:
			[
				{ label: qsTr("Traceplot"),			value: "trace"},
				{ label: qsTr("Scatterplot"),		value: "scatter"},
				{ label: qsTr("Histogram"),			value: "histogram"},
				{ label: qsTr("Density"),			value: "density"},
				{ label: qsTr("Autocorrelations"),	value: "autocorrelation"}
			]
		}
	}


	Section
	{
		columns: 	2
		title: 		qsTr("Advanced")
		
		Group
		{
			title: 		qsTr("Estimation settings (MCMC)")
			columns: 	1

			IntegerField
			{
				label: 			qsTr("Warmup")
				name: 			"mcmcBurnin"
				defaultValue: 	2000
				min:			100
				max: 			1000000
				fieldWidth: 	100
			}

			IntegerField
			{
				label: 			qsTr("Iterations")
				name: 			"mcmcSamples"
				defaultValue: 	2000
				min:			100
				max: 			1000000
				fieldWidth: 	100
			}

			IntegerField
			{
				label: 			qsTr("Chains")
				name: 			"mcmcChains"
				defaultValue: 	4
				min:			1
				max: 			10
				fieldWidth: 	50
			}

			DoubleField
			{
				label: 			qsTr("Adapt delta")
				name: 			"mcmcAdaptDelta"
				defaultValue: 	0.80
				min:			0.05
				max: 			1
				fieldWidth: 	50
			}

			IntegerField
			{
				label: 			qsTr("Maximum treedepth")
				name: 			"mcmcMaxTreedepth"
				defaultValue: 	15
				min:			5
				max: 			100
				fieldWidth: 	50
			}
		}

		SetSeed { }
	}
}
