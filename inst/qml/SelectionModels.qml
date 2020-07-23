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

Form
{
	VariablesForm
	{
		preferredHeight: 200 * preferencesModel.uiScale

		AvailableVariablesList
		{
			name: "allVariables"
		}

		AssignedVariablesList
		{
			name:			"input_ES"
			title:			qsTr("Effect Size")
			singleVariable:	true
			allowedColumns:	["scale"]
		}

		AssignedVariablesList
		{
			name:			"input_SE"
			title:			qsTr("Effect Size Standard Error")
			singleVariable:	true
			allowedColumns:	["scale"]
		}

		AssignedVariablesList
		{
			name:			"input_p"
			title:			qsTr("P-values (one-sided)")
			singleVariable:	true
			allowedColumns:	["scale"]
		}
	}

	Section
	{
		title: qsTr("Model")

		TextField
		{
			name:		"cutoffs_p"
			text:		qsTr("P-value cutoffs")
			value:		"(.05, .10)"
			fieldWidth:	150
		}

		CheckBox
		{
			name:		"selection_twosided"
			text:		qsTr("Two-sided selection")
			checked:	true
		}

		CheckBox
		{
			name:	"p_table"
			text:	qsTr("P-values frequency")
		}
		
		CheckBox
		{
			name:		"auto_reduce"
			text:		qsTr("Automatically join p-values intervals")
			checked:	true
		}

		CheckBox
		{
			name:		"negative_direction"
			text:		qsTr("Expected direction is negative")
		}
	}

	Section
	{
		title: qsTr("Inference")

		CheckBox
		{
			name:	"FE_estimates"
			text:	qsTr("Fixed effects estimates")
			checked: true

			CheckBox
			{
				name:	"FE_weights"
				text:	qsTr("Estimated weights")
			}
		}
		
		CheckBox
		{
			name:	"RE_estimates"
			text:	qsTr("Random effects estimates")
			checked: true

			CheckBox
			{
				name:	"RE_heterogeneity"
				text:	qsTr("Estimated heterogeneity")
			}

			CheckBox
			{
				name:	"RE_weights"
				text:	qsTr("Estimated weights")
			}
		}

	}
	Section
	{
		title: qsTr("Plots")

		Group
		{
			title: qsTr("Weight function")
			CheckBox
			{
				name:	"FE_weightfunction"
				text:	qsTr("Fixed effects")
			}

			CheckBox
			{
				name:	"RE_weightfunction"
				text:	qsTr("Random effects")
			}
		}

		CheckBox
		{
			name: "plot_models"
			text:	qsTr("Mean model estimates")
		}

	}
}
