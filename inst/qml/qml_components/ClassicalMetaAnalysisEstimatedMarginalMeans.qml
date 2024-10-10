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

Section
{
	title:						qsTr("Estimated Marginal Means")
	columns:					1
	property string module:		"metaAnalysis"

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
				source:			[{ name: "effectSizeModelTerms", use: "noInteraction" }]
			}

			AssignedVariablesList
			{
				id:				estimatedMarginalMeansEffectSizeSelectedVariables
				name:			"estimatedMarginalMeansEffectSizeSelectedVariables"
				title:			qsTr("Selected variables")
				allowTypeChange:false
			}
		}

		Group
		{
			columns:	2

			DoubleField
			{
				name:			"estimatedMarginalMeansEffectSizeSdFactorCovariates"
				label:			qsTr("SD factor covariates")
				defaultValue: 	1
				min:			0
				enabled:		estimatedMarginalMeansEffectSizeSelectedVariables.columnsTypes.includes("scale")
				Layout.preferredWidth: 350 * jaspTheme.uiScale
			}

			CheckBox
			{
				name:		"estimatedMarginalMeansEffectSizeAddAdjustedEstimate"
				label:		qsTr("Add adjusted estimate")
			}

			CheckBox
			{
				name:				"estimatedMarginalMeansEffectSizeTestAgainst"
				label:				qsTr("Test against")
				childrenOnSameRow:	true

				DoubleField
				{
					name:			"estimatedMarginalMeansEffectSizeTestAgainstValue"
					defaultValue:	0
				}
			}
		}
	}

	Group
	{
		title:		qsTr("Heterogeneity")
		enabled:	sectionModel.heterogeneityModelTerms.count > 0
		visible:	module == "metaAnalysis"

		VariablesForm
		{
			preferredHeight:	250 * preferencesModel.uiScale

			AvailableVariablesList
			{
				name:			"estimatedMarginalHeterogeneityModelVariables"
				title:			qsTr("Model variables")
				source:			[{ name: "heterogeneityModelTerms", use: "noInteraction" }]
			}

			AssignedVariablesList
			{
				id:				estimatedMarginalMeansHeterogeneitySelectedVariables
				name:			"estimatedMarginalMeansHeterogeneitySelectedVariables"
				title:			qsTr("Selected variables")
				allowTypeChange:false
			}
		}

		Group
		{
			columns:	2

			DoubleField
			{
				name:			"estimatedMarginalMeansHeterogeneitySdFactorCovariates"
				label:			qsTr("SD factor covariates")
				defaultValue: 	1
				min:			0
				enabled:		estimatedMarginalMeansHeterogeneitySelectedVariables.columnsTypes.includes("scale")
				Layout.preferredWidth: 350 * jaspTheme.uiScale
			}

			CheckBox
			{
				name:		"estimatedMarginalMeansHeterogeneityAddAdjustedEstimate"
				label:		qsTr("Add adjusted estimate")
			}

			DropDown
			{
				name:			"estimatedMarginalMeansHeterogeneityTransformation"
				label:			qsTr("Heterogeneity transformation")
				values:			[
						{ label: qsTr("𝜏")		, value: "tau"	},
						{ label: qsTr("𝜏²")	, value: "tau2"	}
					]
			}
		}
	}
}