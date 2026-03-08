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
		preferredHeight:	200 * preferencesModel.uiScale

		AvailableVariablesList
		{
			name:	"allVariables"
		}

		AssignedVariablesList
		{
			name:				"effectSize"
			title:				qsTr("Effect Size")
			singleVariable:		true
			allowedColumns:		["scale"]
			info:				qsTr("Variable containing the observed effect sizes.")
		}

		AssignedVariablesList
		{
			name:				"effectSizeStandardError"
			title:				qsTr("Effect Size Standard Error")
			singleVariable:		true
			allowedColumns:		["scale"]
			info:				qsTr("Variable containing the standard errors of the effect sizes.")
		}

		AssignedVariablesList
		{
			name:				"cluster"
			title:				qsTr("Cluster")
			singleVariable:		true
			allowedColumns:		["nominal"]
			info:				qsTr("Variable identifying the cluster (e.g., study) to aggregate effect sizes within.")
		}
	}

	MA.ClassicalMetaAnalysisMultivariate
	{
		analysisType:	"effectSizeAggregation"
	}

	Section
	{
		title:	qsTr("Options")

		Group
		{

			CheckBox
			{
				name:		"weighted"
				text:		qsTr("Inverse-variance weighting")
				checked:	true
				info:		qsTr("Use inverse-variance weighting when aggregating effect sizes.")
			}

			CheckBox
			{
				id:			addClusterSize
				name:		"addClusterSize"
				text:		qsTr("Add cluster size column")
				checked:	false
				info:		qsTr("Add a column with the number of effect sizes per cluster.")
			}
		}

		Group
		{
			title:	qsTr("Aggregated Column Names")

			TextField
			{
				name:			"aggregatedColumnNamesEffectSize"
				label:			qsTr("Effect size")
				defaultValue:	"aggregated effect size"
			}

			TextField
			{
				name:			"aggregatedColumnNamesStandardError"
				label:			qsTr("Standard error")
				defaultValue:	"aggregated standard error"
			}

			TextField
			{
				name:			"aggregatedColumnNamesClusterSize"
				label:			qsTr("Cluster size")
				defaultValue:	"cluster size"
				visible:		addClusterSize.checked
			}
		}
	}
}
