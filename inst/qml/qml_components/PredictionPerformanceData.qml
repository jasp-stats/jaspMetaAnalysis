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
import QtQuick			2.12
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0
import QtQuick.Layouts	1.3

Section
{
	title:		qsTr("Data")
	expanded:	true

	property alias measure:		measure.value

	RadioButtonGroup
	{
		title:					qsTr("Measure")
		name:					"measure"
		id:						measure
		Layout.columnSpan:		2
		radioButtonsOnSameRow:	true
		columns:				2
		onValueChanged:
		{
			inputMeasure.itemDoubleClicked(0)
			inputSE.itemDoubleClicked(0)
			inputCI.itemDoubleClicked(0)
			inputCI.itemDoubleClicked(1)
		}

		RadioButton
		{
			label:		qsTr("O:E ratio")
			value:		"OE"
			id:			measuresOE
		}

		RadioButton
		{
			label:		qsTr("C statistic")
			value:		"cstat"
			id:			measuresC
		}
	}

	VariablesForm
	{
		preferredHeight: 360 * preferencesModel.uiScale
		
		AvailableVariablesList
		{
			name: "allVariables"
		}
		
		AssignedVariablesList
		{
			name:				"inputMeasure"
			id:					inputMeasure
			title:				if (measuresOE.checked){
				qsTr("O:E Ratio")
			} else if (measuresC.checked) {
				qsTr("C-statistic")
			}
			singleVariable:		true
			allowedColumns: 	["scale"]
		}

		AssignedVariablesList
		{
			name:				"inputSE"
			id:					inputSE
			title:				qsTr("Standard Error")
			singleVariable:		true
			allowedColumns: 	["scale"]
		}

		AssignedPairsVariablesList
		{
			name: 				"inputCI"
			id: 				inputCI
			title: 				qsTr("95% CI Lower and Upper Bound")
			singleVariable: 	true
			allowedColumns: 	["scale"]
		}

		AssignedVariablesList
		{
			name:				"inputN"
			title:				qsTr("Number of Participants")
			singleVariable:		true
			allowedColumns: 	["scale", "ordinal"]
		}

		AssignedVariablesList
		{
			name:				"inputO"
			title:				qsTr("Number of Observed Events")
			singleVariable:		true
			allowedColumns: 	["scale", "ordinal"]
		}

		AssignedVariablesList
		{
			name:				"inputE"
			title:				qsTr("Number of Expected Events")
			singleVariable:		true
			allowedColumns: 	["scale", "ordinal"]
		}

		AssignedVariablesList
		{
			name: 				"inputLabels"
			title: 				qsTr("Study Labels")
			singleVariable:		true
			allowedColumns:		["nominal","nominalText"]
		}
	}

}
