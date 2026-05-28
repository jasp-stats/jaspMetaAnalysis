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

Form
{
	info: qsTr("Create risk-of-bias visualizations from systematic review quality assessments. " +
		"Supports RoB 2, ROBINS-I, and QUADAS-2.")
	infoBottom: "## " + qsTr("References") + "\n" +
		"- McGuinness LA, Higgins JPT (2021). “Risk-of-bias VISualization (robvis): An R package and Shiny web app for visualizing risk-of-bias assessments.” _Research Synthesis Methods, 12_(1), 55-61. https://doi.org/10.1002/jrsm.1411\n" +
		"- McGuinness LA (2019). “robvis: An R package and web application for visualising risk-of-bias assessments.” https://github.com/mcguinlu/robvis\n" +
		"## " + qsTr("R Packages") + "\n" +
		"- robvis"

	DropDown
	{
		name:		"robTool"
		id:			robTool
		label:		qsTr("Assessment tool")
		info:		qsTr("Select the risk-of-bias assessment tool used for the systematic review. " +
			"Each tool expects a specific number of domain columns with specific judgment values.")
		values:
		[
			{ label: qsTr("RoB 2"),		value: "rob2"		},
			{ label: qsTr("ROBINS-I"),	value: "robinsI"	},
			{ label: qsTr("QUADAS-2"),	value: "quadas2"	}
		]
	}

	VariablesForm
	{
		preferredHeight: 500 * preferencesModel.uiScale
		removeInvisibles: true

		AvailableVariablesList
		{
			name:	"allVariables"
		}

		AssignedVariablesList
		{
			name:				"studyLabel"
			title:				qsTr("Study Label")
			singleVariable:		true
			allowedColumns:		["nominal"]
			info:				qsTr("Variable containing study identifiers.")
		}

		// ── ROB2: 5 domains ─────────────────────────────────────────────

		AssignedVariablesList
		{
			name:				"rob2D1"
			title:				qsTr("D1: Randomisation")
			singleVariable:		true
			allowedColumns:		["nominal"]
			visible:			robTool.currentValue === "rob2"
			property bool active: robTool.currentValue === "rob2"
			onActiveChanged:	if (!active && count > 0) itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name:				"rob2D2"
			title:				qsTr("D2: Deviations from interventions")
			singleVariable:		true
			allowedColumns:		["nominal"]
			visible:			robTool.currentValue === "rob2"
			property bool active: robTool.currentValue === "rob2"
			onActiveChanged:	if (!active && count > 0) itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name:				"rob2D3"
			title:				qsTr("D3: Missing outcome data")
			singleVariable:		true
			allowedColumns:		["nominal"]
			visible:			robTool.currentValue === "rob2"
			property bool active: robTool.currentValue === "rob2"
			onActiveChanged:	if (!active && count > 0) itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name:				"rob2D4"
			title:				qsTr("D4: Measurement of outcome")
			singleVariable:		true
			allowedColumns:		["nominal"]
			visible:			robTool.currentValue === "rob2"
			property bool active: robTool.currentValue === "rob2"
			onActiveChanged:	if (!active && count > 0) itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name:				"rob2D5"
			title:				qsTr("D5: Selection of reported result")
			singleVariable:		true
			allowedColumns:		["nominal"]
			visible:			robTool.currentValue === "rob2"
			property bool active: robTool.currentValue === "rob2"
			onActiveChanged:	if (!active && count > 0) itemDoubleClicked(0)
		}

		// ── ROBINS-I: 7 domains ─────────────────────────────────────────

		AssignedVariablesList
		{
			name:				"robinsID1"
			title:				qsTr("D1: Confounding")
			singleVariable:		true
			allowedColumns:		["nominal"]
			visible:			robTool.currentValue === "robinsI"
			property bool active: robTool.currentValue === "robinsI"
			onActiveChanged:	if (!active && count > 0) itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name:				"robinsID2"
			title:				qsTr("D2: Selection of participants")
			singleVariable:		true
			allowedColumns:		["nominal"]
			visible:			robTool.currentValue === "robinsI"
			property bool active: robTool.currentValue === "robinsI"
			onActiveChanged:	if (!active && count > 0) itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name:				"robinsID3"
			title:				qsTr("D3: Classification of interventions")
			singleVariable:		true
			allowedColumns:		["nominal"]
			visible:			robTool.currentValue === "robinsI"
			property bool active: robTool.currentValue === "robinsI"
			onActiveChanged:	if (!active && count > 0) itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name:				"robinsID4"
			title:				qsTr("D4: Deviations from interventions")
			singleVariable:		true
			allowedColumns:		["nominal"]
			visible:			robTool.currentValue === "robinsI"
			property bool active: robTool.currentValue === "robinsI"
			onActiveChanged:	if (!active && count > 0) itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name:				"robinsID5"
			title:				qsTr("D5: Missing data")
			singleVariable:		true
			allowedColumns:		["nominal"]
			visible:			robTool.currentValue === "robinsI"
			property bool active: robTool.currentValue === "robinsI"
			onActiveChanged:	if (!active && count > 0) itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name:				"robinsID6"
			title:				qsTr("D6: Measurement of outcomes")
			singleVariable:		true
			allowedColumns:		["nominal"]
			visible:			robTool.currentValue === "robinsI"
			property bool active: robTool.currentValue === "robinsI"
			onActiveChanged:	if (!active && count > 0) itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name:				"robinsID7"
			title:				qsTr("D7: Selection of reported result")
			singleVariable:		true
			allowedColumns:		["nominal"]
			visible:			robTool.currentValue === "robinsI"
			property bool active: robTool.currentValue === "robinsI"
			onActiveChanged:	if (!active && count > 0) itemDoubleClicked(0)
		}

		// ── QUADAS-2: 4 domains ─────────────────────────────────────────

		AssignedVariablesList
		{
			name:				"quadas2D1"
			title:				qsTr("D1: Patient selection")
			singleVariable:		true
			allowedColumns:		["nominal"]
			visible:			robTool.currentValue === "quadas2"
			property bool active: robTool.currentValue === "quadas2"
			onActiveChanged:	if (!active && count > 0) itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name:				"quadas2D2"
			title:				qsTr("D2: Index test")
			singleVariable:		true
			allowedColumns:		["nominal"]
			visible:			robTool.currentValue === "quadas2"
			property bool active: robTool.currentValue === "quadas2"
			onActiveChanged:	if (!active && count > 0) itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name:				"quadas2D3"
			title:				qsTr("D3: Reference standard")
			singleVariable:		true
			allowedColumns:		["nominal"]
			visible:			robTool.currentValue === "quadas2"
			property bool active: robTool.currentValue === "quadas2"
			onActiveChanged:	if (!active && count > 0) itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name:				"quadas2D4"
			title:				qsTr("D4: Flow & timing")
			singleVariable:		true
			allowedColumns:		["nominal"]
			visible:			robTool.currentValue === "quadas2"
			property bool active: robTool.currentValue === "quadas2"
			onActiveChanged:	if (!active && count > 0) itemDoubleClicked(0)
		}

		// ── Shared: Overall & Weights ───────────────────────────────────

		AssignedVariablesList
		{
			name:				"overallJudgment"
			id:					overallJudgment
			title:				qsTr("Overall Judgment")
			singleVariable:		true
			allowedColumns:		["nominal"]
			info:				qsTr("Variable containing the overall risk-of-bias judgment for each study.")
		}

		AssignedVariablesList
		{
			name:				"studyWeights"
			id:					studyWeights
			title:				qsTr("Weights")
			singleVariable:		true
			allowedColumns:		["scale"]
			info:				qsTr("Variable containing study weights for weighted summary plots (e.g., inverse-variance weights).")
		}
	}

	Group
	{
		title:	qsTr("Plots")

		CheckBox
		{
			name:		"summaryPlot"
			label:		qsTr("Summary plot")
			checked:	true
			info:		qsTr("Display a bar chart showing the distribution of risk-of-bias judgments across domains.")

			CheckBox
			{
				name:		"summaryPlotWeighted"
				label:		qsTr("Weighted by study precision")
				enabled:	studyWeights.count > 0
				info:		qsTr("Weight the summary bars by study precision. Requires a weights variable.")
			}
		}

		CheckBox
		{
			name:		"trafficLightPlot"
			label:		qsTr("Traffic light plot")
			checked:	true
			info:		qsTr("Display a matrix of colored circles showing per-study, per-domain risk-of-bias judgments.")

			IntegerField
			{
				name:			"trafficLightPlotPointSize"
				label:			qsTr("Point size")
				defaultValue:	10
				min:			1
				max:			30
				info:			qsTr("Size of the traffic light circles.")
			}
		}
	}

	Group
	{
		title:	qsTr("Appearance")

		DropDown
		{
			name:		"colorScheme"
			label:		qsTr("Color scheme")
			info:		qsTr("Select the color palette for risk-of-bias judgments.")
			values:
			[
				{ label: qsTr("Cochrane"),		value: "cochrane"		},
				{ label: qsTr("Colourblind"),	value: "colourblind"	}
			]
		}

	}
}
