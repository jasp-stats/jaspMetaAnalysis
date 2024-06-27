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

Section
{
	title:						qsTr("Bubble Plot")
	property string module:		"metaAnalysis"

	VariablesForm
	{
		preferredHeight:	200 * preferencesModel.uiScale

		AvailableVariablesList
		{
			name:			"bubblePlotModelVariables"
			title:			qsTr("Model variables")
			source:			[{ name: "effectSizeModelTerms", use: "noInteraction" }]
		}

		AssignedVariablesList
		{
			name:			"bubblePlotSelectedVariable"
			id:				bubblePlotSelectedVariable
			title:			qsTr("Selected variable")
			singleVariable:	true
			allowTypeChange:false
		}

		AssignedVariablesList
		{
			name:			"bubblePlotSeparateLines"
			id:				bubblePlotSeparateLines
			title:			qsTr("Separate Lines")
			allowTypeChange:false
		}

		AssignedVariablesList
		{
			name:			"bubblePlotSeparatePlots"
			id:				bubblePlotSeparatePlots
			title:			qsTr("Separate Plots")
			allowTypeChange:false
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
			}

			Group
			{
				title:		qsTr("Bubles")

				DropDown
				{
					name:		"bubblePlotBubblesSize"
					label:		qsTr("Size")
					values:		[
						{ label: qsTr("Weight")				, value: "weight"},
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
				}

				DoubleField
				{
					name:			"bubblePlotBubblesTransparency"
					label:			qsTr("Transparency")
					defaultValue:	0.90
					min:			0
					max:			1
					inclusive: 		JASP.None
				}

				DoubleField
				{
					enabled:		bubblePlotSelectedVariable.columnsTypes.includes("nominal")
					name:			"bubblePlotBubblesJitter"
					label:			qsTr("Jitter")
					defaultValue:	1
					min:			0
				}
			}
		}

		Group
		{
			CheckBox
			{
				name:		"bubblePlotCondifenceIntervals"
				label:		qsTr("Condifence intervals")
				checked:	true

				DoubleField
				{
					name:			"bubblePlotCondifenceIntervalsTransparency"
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
			}

			DropDown
			{
				name:			"bubblePlotLegendPosition"
				label:			qsTr("Legend position")
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
