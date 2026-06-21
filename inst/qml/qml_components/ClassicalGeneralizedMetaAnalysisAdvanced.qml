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
	title:		qsTr("Advanced")
	columns:	2
	info: qsTr("Advanced options for the generalized (GLMM) meta-analysis.")

	Group
	{
		CheckBox
		{
			name:		"showMetaforRCode"
			text:		qsTr("Show metafor R code")
			info: qsTr("Display the underlying metafor R code used for the analysis.")
		}
	}

	Group
	{
		title:		qsTr("Subgroup Analysis")

		CheckBox
		{
			name:		"includeFullDatasetInSubgroupAnalysis"
			text:		qsTr("Include full dataset")
			checked:	true
			enabled:	subgroup.count > 0
			info: qsTr("Include the full dataset estimate alongside the subgroup estimates.")
		}
	}

	Group
	{
		title:		qsTr("GLMM Settings")
		info: qsTr("Settings specific to the generalized linear mixed model.")

		DropDown
		{
			name:			"glmmCoding"
			label:			qsTr("Group coding")
			startValue:		"0.5"
			visible:		glmmModel.value === "UM.FS" || glmmModel.value === "UM.RS"
			info: qsTr("Coding for the group variable in unconditional models. Contrast (plus/minus 1/2) centers the intercept at the average log odds; Treatment (0/1) sets the intercept to the reference group.")
			values:			[
				{ label: qsTr("Contrast (\u00B1\u00BD)"),				value: "0.5"	},
				{ label: qsTr("Treatment (0/1, group 1 = 1)"),			value: "1"		},
				{ label: qsTr("Treatment (0/1, group 2 = 1)"),			value: "0"		}
			]
		}

		CheckBox
		{
			name:		"glmmCorrelatedEffects"
			label:		qsTr("Correlated random effects")
			checked:	false
			visible:	glmmModel.value === "UM.RS"
			info: qsTr("Allow correlated random effects in the UM.RS model (random study effects and random treatment effects can be correlated).")
		}

		IntegerField
		{
			name:			"glmmQuadraturePoints"
			label:			qsTr("Quadrature points (nAGQ)")
			defaultValue:	7
			min:			1
			max:			100
			info: qsTr("Number of quadrature points for the adaptive Gauss-Hermite approximation of the log-likelihood. Higher values increase accuracy but slow down estimation.")
		}
	}

	Group
	{
		title:		qsTr("Handling of Zero Frequencies")
		info: qsTr("Options for handling zero cell frequencies in the data. These affect the computation of observed effect sizes displayed in plots but do not affect the GLMM model fit itself.")

		DoubleField
		{
			name:			"advancedAdd"
			label:			qsTr("Add to zero cells")
			defaultValue:	0.5
			min:			0
			decimals:		2
			info: qsTr("Value to add to zero cells when computing observed effect sizes for plots.")
		}

		DropDown
		{
			name:		"advancedTo"
			label:		qsTr("Add to")
			info: qsTr("When to apply the continuity correction for zero cells.")
			values:		[
				{ label: qsTr("All studies"),				value: "all"		},
				{ label: qsTr("Only zero-cell studies"),		value: "onlyZero"	},
				{ label: qsTr("If any zero-cell study"),	value: "ifAnyZero"	},
				{ label: qsTr("None"),						value: "none"		}
			]
		}

		RadioButtonGroup
		{
			name:		"advancedDropStudiesWithNoCasesOrEvents"
			title:		qsTr("Drop studies with no cases or events")
			info: qsTr("Whether to drop studies that have no cases/events in both groups from the analysis.")

			RadioButton
			{
				value:		"yes"
				label:		qsTr("Yes")
				checked:	true
			}

			RadioButton
			{
				value:		"no"
				label:		qsTr("No")
			}
		}
	}

	Group
	{
		title:	qsTr("Extend Metafor Call")

		CheckBox
		{
			name:		"advancedExtendMetaforCall"
			label:		qsTr("Extend metafor call")
			info: qsTr("Extend the metafor function call with custom R code. Arguments are passed as additional named arguments to rma.glmm().")

			TextArea
			{
				name:		"advancedExtendMetaforCallCode"
				textType:	JASP.TextTypeSource
				text:		""
				info: qsTr("Custom R code to extend the metafor call (e.g., 'control = list(maxiter = 1000)').")
			}
		}
	}
}
