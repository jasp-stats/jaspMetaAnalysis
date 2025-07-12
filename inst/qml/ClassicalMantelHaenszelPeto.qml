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
	info: qsTr("Mantel-Haenszel / Peto meta-analysis allows you to conduct a meta-analysis of frequencies / time to event outcomes.")
	infoBottom: "## " + qsTr("References") + "\n" +
	"- Viechtbauer W (2010). “Conducting meta-analyses in R with the metafor package.” _Journal of Statistical Software, 36_(3), 1–48. https://doi.org/10.18637/jss.v036.i03\n" +
	"- Viechtbauer W (2025). _metafor: Meta-Analysis Package for R_. R package version 4.8-0 Available at: <https://CRAN.R-project.org/package=metafor>.\n" +
	"## " + qsTr("R Packages") + "\n" +
	"- metafor"

	VariablesForm
	{
		preferredHeight: 400 * preferencesModel.uiScale
		removeInvisibles:	true

		AvailableVariablesList
		{
			name:				"allVariables"
		}

		AssignedVariablesList
		{
			name: 			"successesGroup1"
			title: 			qsTr("Successes Group 1")
			singleVariable: true
			allowedColumns: ["scale"]
			visible: 		method.value === "mantelHaenszelFrequencies" || method.value === "peto"
			enabled: 		method.value === "mantelHaenszelFrequencies" || method.value === "peto"
			onEnabledChanged:	itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name: 			"successesGroup2"
			title: 			qsTr("Successes Group 2")
			singleVariable: true
			allowedColumns: ["scale"]
			visible: 		method.value === "mantelHaenszelFrequencies" || method.value === "peto"
			enabled: 		method.value === "mantelHaenszelFrequencies" || method.value === "peto"
			onEnabledChanged:	itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name: 			"sampleSizeGroup1"
			title: 			qsTr("Sample Size Group 1")
			singleVariable: true
			allowedColumns: ["scale"]
			visible: 		method.value === "mantelHaenszelFrequencies" || method.value === "peto"
			enabled: 		method.value === "mantelHaenszelFrequencies" || method.value === "peto"
			onEnabledChanged:	itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name: 			"sampleSizeGroup2"
			title: 			qsTr("Sample Size Group 2")
			singleVariable: true
			allowedColumns: ["scale"]
			visible: 		method.value === "mantelHaenszelFrequencies" || method.value === "peto"
			enabled: 		method.value === "mantelHaenszelFrequencies" || method.value === "peto"
			onEnabledChanged:	itemDoubleClicked(0)
		}

		AssignedVariablesList
		{ 
			name:			"eventsGroup1"
			title:			qsTr("Events Group 1")
			singleVariable: true
			allowedColumns:	["scale"]
			visible: 		method.value === "mantelHaenszelEvents"
			enabled: 		method.value === "mantelHaenszelEvents"
			onEnabledChanged:	itemDoubleClicked(0)
		}

		AssignedVariablesList
		{ 
			name:			"eventsGroup2"
			title:			qsTr("Events Group 2")
			singleVariable: true
			allowedColumns:	["scale"]
			visible: 		method.value === "mantelHaenszelEvents"
			enabled: 		method.value === "mantelHaenszelEvents"
			onEnabledChanged:	itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name:			"personTimeGroup1"
			title:			qsTr("Person-Time Group 1")
			singleVariable: true
			allowedColumns:	["scale"]
			visible: 		method.value === "mantelHaenszelEvents"
			enabled: 		method.value === "mantelHaenszelEvents"
			onEnabledChanged:	itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name:			"personTimeGroup2"
			title:			qsTr("Person-Time Group 2")
			singleVariable: true
			allowedColumns:	["scale"]
			visible: 		method.value === "mantelHaenszelEvents"
			enabled: 		method.value === "mantelHaenszelEvents"
			onEnabledChanged:	itemDoubleClicked(0)
		}

		DropDown
		{
			name:			"method"
			id:				method
			label:			qsTr("Method")
			startValue:		"mantelHaenszelFrequencies"
			info: qsTr("Select the method to be used for the meta-analysis.")
			values:			[
						{ label: qsTr("Mantel-Haenszel (Frequencies)")		, value: "mantelHaenszelFrequencies"},
						{ label: qsTr("Mantel-Haenszel (Events)")			, value: "mantelHaenszelEvents"},
						{ label: qsTr("Peto")								, value: "peto"	}
					]
		}

		DropDown
		{
			id:			effectSizeMeasure
			name:		"effectSizeMeasure"
			label:		qsTr("Effect size measure")
			info: qsTr("Select the effect size measure to be used in the meta-analysis.")
			values: (function() {
				if (method.value === "mantelHaenszelFrequencies" || method.value === "peto") {
					return [
						{ label: qsTr("Log odds ratio"),				value: "OR",			info: qsTr("Log odds ratio")},
						{ label: qsTr("Log risk ratio"),				value: "RR",			info: qsTr("Log risk ratio")},
						{ label: qsTr("Risk difference"),				value: "RD",			info: qsTr("Risk difference")}
					];
				} else {
					return [
						{ label: qsTr("Log incidence rate ratio")		, value: "IRR",			info: qsTr("Log incidence rate ratio")},
						{ label: qsTr("Incidence rate difference")		, value: "IRD",			info: qsTr("Incidence rate difference")}
					];
				}})()
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

	MA.ClassicalMetaAnalysisStatistics
	{
		analysisType:	"ClassicalMantelHaenszelPeto"
	}

	MA.ForestPlot
	{
		analysisType:	"ClassicalMantelHaenszelPeto"
	}

	MA.ClassicalMetaAnalysisDiagnostics
	{
		analysisType:	"ClassicalMantelHaenszelPeto"
	}

	MA.ClassicalMantelHaenszelPetoAdvanced {}
}
