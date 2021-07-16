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

ColumnLayout
{
	spacing: 						0
	property string componentType:	"Default type"

	Label
	{
		text:					
		{
			if (componentType == "omega")
				qsTr("Publication bias: selection models")
			else if (componentType == "omegaNull")
				qsTr("Publication bias: selection models (null)")
		}
		Layout.preferredHeight:	20 * preferencesModel.uiScale
	}

	RowLayout
	{

		Label { text: qsTr("Weight function");	Layout.preferredWidth: 140 * preferencesModel.uiScale; Layout.leftMargin: 5 * preferencesModel.uiScale}
		Label { text: qsTr("Cut-points");		Layout.preferredWidth: 155 * preferencesModel.uiScale }
		Label { text: qsTr("Parameters");		Layout.preferredWidth: 150 * preferencesModel.uiScale }
		Label { text: qsTr("Prior weights")}
	}

	ComponentsList
	{
		name:					componentType
		optionKey:				"name"
		defaultValues:
		{
			if (componentType == "omega")
				[
					{"type": "two-sided",	"parCuts": "(.05)",				"parAlpha": "(1,1)",	"priorWeights": "1/12"},
					{"type": "two-sided",	"parCuts": "(.05, .10)",		"parAlpha": "(1,1,1)",	"priorWeights": "1/12"},
					{"type": "one-sided",	"parCuts": "(.05)",				"parAlpha": "(1,1)",	"priorWeights": "1/12"},
					{"type": "one-sided",	"parCuts": "(.025, .05)",		"parAlpha": "(1,1,1)",	"priorWeights": "1/12"},
					{"type": "one-sided",	"parCuts": "(.05, .50)",		"parAlpha": "(1,1,1)",	"priorWeights": "1/12"},
					{"type": "one-sided",	"parCuts": "(.025, .05, .10)",	"parAlpha": "(1,1,1,1)","priorWeights": "1/12"}
				]
			else if (componentType == "omegaNull")
				[{"type": "none"}]
		}
		rowComponent: 			RowLayout
		{
			Row
			{
				spacing:				4 * preferencesModel.uiScale
				Layout.preferredWidth:	140 * preferencesModel.uiScale

				DropDown
				{
					id:					typeItem
					name:				"type"
					useExternalBorder:	true
					values:
					[
						{ label: qsTr("Two-sided"),			value: "two-sided"},
						{ label: qsTr("One-sided"),			value: "one-sided"},
						{ label: qsTr("Two-sided (fixed)"),	value: "two-sided-fixed"},
						{ label: qsTr("One-sided (fixed)"),	value: "one-sided-fixed"},
						{ label: qsTr("None"),				value: "none"}
					]
					onCurrentValueChanged: 
					{
						if (currentValue == "two-sided" || currentValue == "one-sided")
						{
							parCuts.value  = "(.05, .10)";
							parAlpha.value = "(1,1,1)";
							parCuts.editingFinished();
							parAlpha.editingFinished();
						}
						if (currentValue == "two-sided-fixed" || currentValue == "one-sided-fixed")
						{
							parCuts.value  = "(.05, .10)";
							parOmega.value = "(1, 0.5, 0.1)";
							parCuts.editingFinished();
							parAlpha.editingFinished();
						}
					}
				}
			}

			Row
			{
				spacing:				4 * preferencesModel.uiScale
				Layout.preferredWidth:	155 * preferencesModel.uiScale

				TextField
				{
					label:				qsTr("p-values")
					id:					parCuts
					name:				"parCuts"
					visible:			typeItem.currentValue === "two-sided"		||
										typeItem.currentValue === "one-sided"       ||
										typeItem.currentValue === "two-sided-fixed" ||
										typeItem.currentValue === "one-sided-fixed"
					value:				"(.05, .10)"
					fieldWidth: 		100 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
			}

			Row
			{
				spacing:				4 * preferencesModel.uiScale
				Layout.preferredWidth:	150 * preferencesModel.uiScale

				TextField
				{
					label:				"α"
					id:					parAlpha
					name:				"parAlpha"
					visible:			typeItem.currentValue === "two-sided" ||
										typeItem.currentValue === "one-sided"
					value:				"(1,1,1)"
					fieldWidth: 		120 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				TextField
				{
					label:				"ω"
					id:					parOmega
					name:				"parOmega"
					visible:			typeItem.currentValue === "two-sided-fixed" ||
										typeItem.currentValue === "one-sided-fixed"
					value:				"(1, 0.5, 0.1)"
					fieldWidth: 		70 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
			}

			FormulaField
			{
				label: 				qsTr("Weight")
				name: 				"priorWeight"
				value:				"1"
				min: 				0
				inclusive: 			JASP.None
				fieldWidth:			40 * preferencesModel.uiScale
				useExternalBorder:	false
				showBorder:			true
			}
		}
	}
}
