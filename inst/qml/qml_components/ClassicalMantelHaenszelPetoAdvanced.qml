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
	title:							qsTr("Advanced")
	property string analysisType:	"metaAnalysis"
	columns:						1

	info: qsTr("Advanced options for the meta-analysis.")

	Group
	{
		columns:	2

		Group
		{
			CheckBox
			{
				name:		"showMetaforRCode"
				text:		qsTr("Show metafor R code")
				Layout.preferredWidth: 300 * jaspTheme.uiScale
				info: qsTr("Display the underlying R code used by the metafor package to fit the model.")
			}

			CheckBox
			{
				name:		"includeFullDatasetInSubgroupAnalysis"
				text:		qsTr("Include full dataset in subgroup analysis")
				enabled:	subgroup.count == 1
				checked:	false
				info: qsTr("Include the full dataset output in the subgroup analysis. This option is only available when the subgroup analysis is selected.")
			}
		}

		Group
		{
			title:		qsTr("Handling of zero frequencies")
			enabled:	method.value === "mantelHaenszelFrequencies" || method.value === "peto"
			info: qsTr("Options for handling zero frequencies in the data.")

			DoubleField
			{
				label: qsTr("Add")
				name: "advancedAdd"
				enabled: advancedTo.value != "none"
				defaultValue: 0.5
			}

			DropDown
			{
				name: "advancedTo"
				id: advancedTo
				label: qsTr("To")
				startValue: "onlyZero"
				values: [
					{ label: qsTr("All"), value: "all" },
					{ label: qsTr("Only zero"), value: "onlyZero" },
					{ label: qsTr("If any zero"), value: "ifAnyZero" },
					{ label: qsTr("None"), value: "none" }
				]
			}

			RadioButtonGroup
			{
				name: "advancedDropStudiesWithNoCasesOrEvents"
				title: qsTr("Drop studies with no cases/events")
				columns: 2

				RadioButton
				{
					value: "yes"
					label: qsTr("Yes")
				}

				RadioButton
				{
					value: "no"
					label: qsTr("No")
					checked: true
				}
			}
		}

		CheckBox
		{
			name:		"advancedContinuityCorrection"
			text:		qsTr("Continuity correction")
			enabled:	method.value === "mantelHaenszelFrequencies" && effectSizeMeasure.value === "OR"
			checked:	true
			info: qsTr("Apply continuity correction to Cochran-Mantel-Haenszel test statistic.")
		}
	}

}
