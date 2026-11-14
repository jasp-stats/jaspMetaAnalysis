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
	columns:	1
	info: qsTr("Advanced options for the network meta-analysis.")

	Group
	{
		columns:	2

		Group
		{
			CheckBox
			{
				name:		"showNetmetaRCode"
				text:		qsTr("Show netmeta R code")
				Layout.preferredWidth: 300 * jaspTheme.uiScale
				info: qsTr("Display the R code that fits the model(s) of the analysis with the meta and netmeta packages (the code reproduces the fitted models, not the tables and plots of the selected outputs).")
			}

			CheckBox
			{
				name:		"includeFullDatasetInSubgroupAnalysis"
				text:		qsTr("Include full dataset in subgroup analysis")
				enabled:	subgroup.count == 1
				checked:	false
				info: qsTr("Include the full dataset output in the subgroup analysis. This option is only available when the subgroup analysis is selected.")
			}

			DoubleField
			{
				name:			"advancedMultiArmTolerance"
				label:			qsTr("Multi-arm consistency tolerance")
				defaultValue:	0.001
				min:			0
				decimals:		4
				fieldWidth:		70 * preferencesModel.uiScale
				info: qsTr("Tolerance for the consistency check of the treatment effects of multi-arm studies specified by all of their pairwise comparisons (contrast-based data); only the consistency of the effect sizes is checked.")
			}

			IntegerField
			{
				name:			"rankingNumberOfSimulations"
				label:			qsTr("Number of ranking simulations")
				defaultValue:	1000
				min:			100
				max:			100000
				fieldWidth:		70 * preferencesModel.uiScale
				enabled:		!poolingMethod.countBased && networkModel.value === "standard"
				info: qsTr("Number of simulations used to resample the treatment effects for the SUCRA, probability of being best, mean and median rank, and the rankogram (reproducible with the 'Set seed' option).")
			}

			SetSeed {}
		}

		Group
		{
			title:		qsTr("Fix Parameters")
			enabled:	method.value !== "equalEffects" && !poolingMethod.noHeterogeneity
			info: qsTr("Fix model parameters instead of estimating them. Only available for random-effects models.")

			CheckBox
			{
				name:				"fixParametersTau2"
				text:				qsTr("𝜏²")
				childrenOnSameRow:	true
				info: qsTr("Fix the value of 𝜏² (the common between-study variance) instead of estimating it. Only available for random-effects models.")

				FormulaField
				{
					label: 				""
					name: 				"fixParametersTau2Value"
					value:				"1"
					min: 				0
					inclusive: 			JASP.None
					info: qsTr("Value of 𝜏² (the common between-study variance) used in the model.")
				}
			}
		}

		Group
		{
			title:		qsTr("Continuity Correction")
			visible:	dataFormat.value === "armBased" && (outcomeType.value === "binary" || outcomeType.value === "incidenceRate")
			info: qsTr("Options for handling zero frequencies in arm-based binary and incidence rate data. The continuity correction is applied to the pairwise effect sizes computed from the arm-level data, which the inverse variance analysis pools. The Mantel-Haenszel, non-central hypergeometric, and penalized logistic regression estimates are computed from the event counts without a continuity correction.")

			DoubleField
			{
				label:			qsTr("Add")
				name:			"advancedAdd"
				enabled:		advancedTo.value != "none"
				defaultValue:	0.5
				min:			0
				info: qsTr("Value added to the frequencies for the continuity correction.")
			}

			DropDown
			{
				name:		"advancedTo"
				id:			advancedTo
				label:		qsTr("To")
				startValue:	"onlyZero"
				info: qsTr("Select to which studies the continuity correction is applied.")
				values:		[
					{ label: qsTr("All")			, value: "all"			, info: qsTr("Add the value to all studies.")},
					{ label: qsTr("Only zero")		, value: "onlyZero"		, info: qsTr("Add the value to studies with a zero frequency.")},
					{ label: qsTr("If any zero")	, value: "ifAnyZero"	, info: qsTr("Add the value to all studies if any study has a zero frequency.")},
					{ label: qsTr("None")			, value: "none"			, info: qsTr("Do not apply a continuity correction.")}
				]
			}

			RadioButtonGroup
			{
				name:		"advancedDropStudiesWithNoCasesOrEvents"
				title:		qsTr("Drop studies with no cases/events")
				columns:	2
				enabled:	(effectSizeMeasure.value === "OR" || effectSizeMeasure.value === "RR") && !poolingMethod.countBased
				info: qsTr("Specify whether studies with zero events (or non-events) in all arms are dropped from the analysis. This applies to the inverse variance analysis of the odds ratio and the risk ratio (the other measures keep these studies). The group is disabled for the Mantel-Haenszel, non-central hypergeometric, and penalized logistic regression methods, which do not use it: the Mantel-Haenszel and non-central hypergeometric methods always exclude the studies without events (and the designs with zero cells), and penalized logistic regression keeps all studies.")

				RadioButton
				{
					value:		"yes"
					label:		qsTr("Yes")
					checked:	true
					info: qsTr("Drop studies without cases/events in all arms.")
				}

				RadioButton
				{
					value:		"no"
					label:		qsTr("No")
					info: qsTr("Keep studies without cases/events in all arms.")
				}
			}
		}
	}
}
