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

Section
{
	title:							qsTr("Bubble Plot")
	property string analysisType:	"metaAnalysis"
	info: qsTr("Options for visualizing the estimated effect sizes at different levels of the predictors with the observed estimates visualized as bubbles. Continuous predictors can be split into three bins with cutoffs at ±x standard deviations from the mean. Predictors that are not specified for either the x-axis, lines, or plots are averaged over.")

	VariablesForm
	{
		preferredHeight:	200 * preferencesModel.uiScale

		AvailableVariablesList
		{
			name:			"bubblePlotModelVariables"
			title:			qsTr("Model Variables")
			source:			[{ name: "effectSizeModelTerms", use: "noInteraction" }]
		}

		AssignedVariablesList
		{
			name:			"bubblePlotSelectedVariable"
			id:				bubblePlotSelectedVariable
			title:			qsTr("Selected Variable")
			singleVariable:	true
			allowTypeChange:false
			info: qsTr("Variable to be visualized on the x-axis.")
		}

		AssignedVariablesList
		{
			name:			"bubblePlotSeparateLines"
			id:				bubblePlotSeparateLines
			title:			qsTr("Separate Lines")
			allowTypeChange:false
			info: qsTr("Variable(s) according to which predictions are split across different lines.")
		}

		AssignedVariablesList
		{
			name:			"bubblePlotSeparatePlots"
			id:				bubblePlotSeparatePlots
			title:			qsTr("Separate Plots")
			allowTypeChange:false
			info: qsTr("Variable(s) according to which predictions are split across different plots.")
		}
	}

	Group
	{
		columns:	2

		Group
		{
			DoubleField
			{
				name:			"bubblePlotSdFactorCovariates"
				label:			qsTr("SD factor covariates")
				defaultValue: 	1
				min:			0
				enabled:		bubblePlotSeparateLines.columnsTypes.includes("scale") || bubblePlotSeparatePlots.columnsTypes.includes("scale")
				Layout.preferredWidth: 300 * jaspTheme.uiScale
				info: qsTr("Standard deviation cutoff used for binning continuous covariates.")
			}

			Group
			{
				title:		qsTr("Bubbles")

				DropDown
				{
					name:		"bubblePlotBubblesSize"
					label:		qsTr("Size")
					info: qsTr("Options for determining the size of the observed estimates.")
					values:
					if (analysisType === "metaAnalysis" || analysisType === "metaAnalysisMultilevelMultivariate")
						[
							{ label: qsTr("Weight")				, value: "weight"},
							{ label: qsTr("Inverse variance")	, value: "inverseVariance"	},
							{ label: qsTr("Equal")				, value: "equal"	}
						]
					else if (analysisType === "BiBMA")
						[
							{ label: qsTr("Sample size")		, value: "sampleSize"	},
							{ label: qsTr("Inverse variance")	, value: "inverseVariance"	},
							{ label: qsTr("Equal")				, value: "equal"	}
						]
					else
						[
							{ label: qsTr("Inverse variance")	, value: "inverseVariance"	},
							{ label: qsTr("Equal")				, value: "equal"	}
						]
				}

				DoubleField
				{
					name:			"bubblePlotBubblesRelativeSize"
					label:			qsTr("Relative size")
					defaultValue:	1
					min:			0
					inclusive: 		JASP.None
					info: qsTr("Set the relative size of the observed estimates.")
				}

				DoubleField
				{
					name:			"bubblePlotBubblesTransparency"
					label:			qsTr("Transparency")
					defaultValue:	0.90
					min:			0
					max:			1
					inclusive: 		JASP.None
					info: qsTr("Set the transparency of the observed estimates.")
				}

				DoubleField
				{
					enabled:		bubblePlotSelectedVariable.columnsTypes.includes("nominal")
					name:			"bubblePlotBubblesJitter"
					label:			qsTr("Jitter")
					defaultValue:	1
					min:			0
					info: qsTr("Set the degree of x-coordinate jitter of the observed estimates. Available when the x-axis variable is nominal.")
				}
			}
		}

		Group
		{
			CheckBox
			{
				name:		"bubblePlotConfidenceIntervals"
				label:		qsTr("Confidence intervals")
				checked:	true
				info: qsTr("Include confidence intervals of the estimated effect sizes.")

				DoubleField
				{
					name:			"bubblePlotConfidenceIntervalsTransparency"
					label:			qsTr("Transparency")
					defaultValue:	0.30
					min:			0
					max:			1
					inclusive: 		JASP.None
				}
			}

			CheckBox
			{
				name:		"bubblePlotPredictionIntervals"
				label:		qsTr("Prediction intervals")
				checked:	true
				info: qsTr("Include prediction intervals of the estimated effect sizes.")
				visible:	analysisType === "metaAnalysis" || analysisType === "metaAnalysisMultilevelMultivariate"

				DoubleField
				{
					name:			"bubblePlotPredictionIntervalsTransparency"
					label:			qsTr("Transparency")
					defaultValue:	0.10
					min:			0
					max:			1
					inclusive: 		JASP.None
				}
			}
		}

		Group
		{

			ColorPalette{}

			DropDown
			{
				name:			"bubblePlotTheme"
				id:				bubblePlotTheme
				label:			qsTr("Theme")
				startValue:		"jasp"
				info: qsTr("Set the theme of the bubble plot.")
				values:
				[
					{ label: "JASP",					value: "jasp"},
					{ label: qsTr("White background"),	value: "whiteBackground"},
					{ label: qsTr("Light"),				value: "light"},
					{ label: qsTr("Minimal")	,		value: "minimal"},
					{ label: "APA",						value: "apa"},
					{ label: "pubr",					value: "pubr"}
				]
			}

			DoubleField
			{
				enabled:		bubblePlotTheme.value != "jasp"
				name:			"bubblePlotRelativeSizeText"
				label:			qsTr("Relative text size")
				defaultValue:	1.5
				min:			0
				inclusive: 		JASP.None
				info: qsTr("Adjust the text size of the bubble plot.")
			}

			DropDown
			{
				name:			"bubblePlotLegendPosition"
				label:			qsTr("Legend position")
				info: qsTr("Set the legend position of the bubble plot.")
				startValue:		"right"
				values:
				[
					{ label: qsTr("None"),			value: "none"},
					{ label: qsTr("Bottom"),		value: "bottom"},
					{ label: qsTr("Right"),			value: "right"},
					{ label: qsTr("Top"),			value: "top"},
					{ label: qsTr("Left"), 			value: "left"}
				]
			}
		}
	}

}
