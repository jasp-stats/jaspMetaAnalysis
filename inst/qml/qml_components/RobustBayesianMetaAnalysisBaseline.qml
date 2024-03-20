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
		text:	switch (componentType) 
		{
			case "modelsBaseline":				qsTr("Baseline"); break;
			case "modelsBaselineNull":			qsTr("Baseline (null)"); break;
		}
		Layout.preferredHeight:	20 * preferencesModel.uiScale
	}

	RowLayout
	{
		Label { text: qsTr("Distribution"); Layout.preferredWidth: 140 * preferencesModel.uiScale; Layout.leftMargin: 5 * preferencesModel.uiScale}
		Label { text: qsTr("Parameters");	Layout.preferredWidth: 155 * preferencesModel.uiScale }
		Label { text: qsTr("Truncation");	Layout.preferredWidth: 150 * preferencesModel.uiScale }
		Label { text: qsTr("Prior weights") }
	}

	ComponentsList
	{
		name:					componentType
		optionKey:				"name"
		defaultValues:			switch (componentType) 		
		{
			case "modelsBaseline":				[]; break;
			case "modelsBaselineNull":			[{"type": "beta", "alpha": "1", "beta": "1"}]; break;
		}
		rowComponent: 			RowLayout
		{
			Row
			{
				spacing:				4 * preferencesModel.uiScale
				Layout.preferredWidth:	140 * preferencesModel.uiScale

				DropDown
				{
					id: typeItem
					name: "type"
					useExternalBorder: true
					values:
					[
						{ label: qsTr("Beta(α,β)"),				value: "beta"}
					]
				}
			}

			Row
			{
				spacing:				4 * preferencesModel.uiScale
				Layout.preferredWidth:	155 * preferencesModel.uiScale

				FormulaField
				{
					label:				"α "
					name:				"alpha"
					value:				"1"
					min:				0
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"β"
					name:				"beta"
					value:				"1"
					min:				0
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
			}

			Row
			{
				spacing:				4 * preferencesModel.uiScale
				Layout.preferredWidth:	150 * preferencesModel.uiScale

				FormulaField
				{
					id:					truncationLower
					label: 				qsTr("lower")
					name: 				"truncationLower"
					value:				0
					min:				0
					max: 				truncationUpper.value
					inclusive: 			JASP.MinOnly
					fieldWidth:			40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder:			true
				}
				FormulaField
				{
					id:					truncationUpper
					label: 				qsTr("upper")
					name: 				"truncationUpper"
					value:				1
					max:				1
					min: 				truncationLower ? truncationLower.value : 0
					inclusive: 			JASP.MaxOnly
					fieldWidth:			40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder:			true
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
