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
			name:			"inputES"
			title:
			{
				if (measuresCorrelation.checked)
					qsTr("Correlation")
				else
					qsTr("Effect Size")
			}
			singleVariable:	true
			allowedColumns:	["scale"]
		}

		AssignedVariablesList
		{
			id:				inputSE
			name:			"inputSE"
			title:			qsTr("Effect Size Standard Error")
			singleVariable:	true
			allowedColumns:	["scale"]
			visible:		 measuresGeneral.checked
		}

		AssignedVariablesList
		{
			id:				inputN
			name: 			"inputN"
			title: 			qsTr("N")
			singleVariable: true
			allowedColumns: ["scale", "ordinal"]
			visible:		 measuresCorrelation.checked
		}

		DropDown
		{
			visible:	measuresCorrelation.checked
			label:		qsTr("Transform correlations to")
			name:		"muTransform"
			values:
			[
				{ label: qsTr("Fisher's z"),	value: "fishersZ"},
				{ label: qsTr("Cohen's d"),		value: "cohensD"}
			]
		}
	}

	RadioButtonGroup
	{
		name:					"measures"
		title:					qsTr("Measure")

		RadioButton
		{
			label: qsTr("Effect sizes & SE")
			value: "general"
			id: 	measuresGeneral
			checked:true
			onCheckedChanged: if (!checked && inputSE.count > 0) inputSE.itemDoubleClicked(0);
		}

		RadioButton
		{
			label: qsTr("Correlations & N")
			value: "correlation"
			id: 	measuresCorrelation
			onCheckedChanged: if (!checked && inputN.count > 0) inputN.itemDoubleClicked(0);
		}
	}

	Section
	{
		title: qsTr("Inference")
		
		Group
		{
			CheckBox
			{
				text:		qsTr("Mean estimates")
				name:		"estimatesMean"
				checked:	true
			}
						
			CheckBox
			{
				text:		qsTr("Regression estimates")
				name:		"estimatesPetPeese"
				checked:	false
			}

			CheckBox
			{
				text:		qsTr("Multiplicative heterogeneity estimates")
				name:		"estimatesSigma"
				checked:	false
			}
		}
	}

	Section
	{
		title: qsTr("Plots")

		Group
		{
			title: qsTr("Meta-Regression Estimate")

			CheckBox
			{
				text:	qsTr("PET")
				name:	"regressionPet"
			}

			CheckBox
			{
				text:	qsTr("PEESE")
				name:	"regressionPeese"
			}
		}

		CheckBox
		{
			text:	qsTr("Mean model estimates")
			name:	"plotModels"
		}

	}
}
