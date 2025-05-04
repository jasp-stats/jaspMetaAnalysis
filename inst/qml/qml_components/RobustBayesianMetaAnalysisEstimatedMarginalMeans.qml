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
	title:						qsTr("Estimated Marginal Means")
	columns:					2
	info: qsTr("Options to compute estimated marginal means, allowing examination of predicted values at specific levels of moderators and their differences from 0.")

	property string analysisType: "RoBMA"
	// RoBMA: Robust Bayesian Meta-Analsis
	// BiBMA: Binomial Bayesian Meta-Analysis
	// NoBMA: Normal Bayesian Meta-Analysis

	Group
	{
		title:		qsTr("Effect size")
		enabled:	sectionModel.effectSizeModelTermsCount > 0

		VariablesForm
		{
			preferredHeight:	250  * preferencesModel.uiScale

			AvailableVariablesList
			{
				name:			"estimatedMarginalMeansEffectSizeModelVariables"
				title:			qsTr("Model variables")
				source:			"effectSizeModelTerms"
			}

			AssignedVariablesList
			{
				id:				estimatedMarginalMeansEffectSizeSelectedVariables
				name:			"estimatedMarginalMeansEffectSizeSelectedVariables"
				title:			qsTr("Selected variables")
				allowTypeChange:false
				info: qsTr("Variables selected for computing estimated marginal means in the effect size model.")
			}
		}

		Group
		{
			columns:	2

			CheckBox
			{
				label:		qsTr("Estimated Marginal Means")
				name: 		"estimatedMarginalMeansEffectSize"
				checked:	true
				Layout.preferredWidth: 250 * jaspTheme.uiScale

				CheckBox
				{
					name:		"estimatedMarginalMeansEffectSizeAddAdjustedEstimate"
					label:		qsTr("Add adjusted estimate")
					info: qsTr("Include the adjusted effect estimate, which accounts for the moderators in the meta-regression model. This provides the effect size adjusted for the influence of moderators, as opposed to the pooled effect which combines the estimates across all studies.")
				}

				CheckBox
				{
					name:				"estimatedMarginalMeansEffectSizeTestAgainst0"
					label:				qsTr("Test against 0")
					childrenOnSameRow:	true
					info: qsTr("Option to test the estimated marginal means against 0.")
				}
			}
		}
	}
}
