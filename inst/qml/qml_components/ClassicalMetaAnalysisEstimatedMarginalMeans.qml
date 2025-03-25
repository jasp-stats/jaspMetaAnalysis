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
	title:						qsTr("Estimated Marginal Means and Contrasts")
	columns:					2
	property string module:		"metaAnalysis"
	info: qsTr("Options to compute estimated marginal means (EMMs) and coefficient contrasts for the effect size and heterogeneity models, allowing examination of predicted values at specific levels of moderators and their differences.")

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

				DoubleField
				{
					name:			"estimatedMarginalMeansEffectSizeSdFactorCovariates"
					label:			qsTr("SD factor covariates")
					defaultValue: 	1
					min:			0
					enabled:		estimatedMarginalMeansEffectSizeSelectedVariables.columnsTypes.includes("scale")
					info: qsTr("Standard deviation factor for covariates when computing estimated marginal means; applies to scale variables.")
				}

				CheckBox
				{
					name:				"estimatedMarginalMeansEffectSizeTestAgainst"
					label:				qsTr("Test against")
					childrenOnSameRow:	true
					info: qsTr("Option to test the estimated marginal means against a specific value.")

					DoubleField
					{
						name:			"estimatedMarginalMeansEffectSizeTestAgainstValue"
						defaultValue:	0
					}
				}
			}

			CheckBox
			{	
				enabled:	estimatedMarginalMeansEffectSizeSelectedVariables.columnsTypes.includes("nominal")
				name:		"contrastsEffectSize"
				label:		qsTr("Contrasts")

				DropDown
				{
					name:	"contrastsEffectSizePValueAdjustment"
					label:	qsTr("P-value adjustment")
					values:
					[
						{ label: qsTr("None"),				value: "none"},
						{ label: "Bonferroni",				value: "bonferroni"},
						{ label: "Holm",					value: "holm"},
						{ label: "Hochberg",				value: "hochberg"},
						{ label: "Hommel",					value: "hommel"},
						{ label: "Benjamini & Hochberg",	value: "benjaminiHochberg"},
						{ label: "Benjamini & Yekutieli",	value: "benjaminiYekutieli"}
					]
				}
			}
		}
	}

	Group
	{
		title:		qsTr("Heterogeneity")
		enabled:	sectionModel.heterogeneityModelTermsCount > 0
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
				info: qsTr("Variables selected for computing estimated marginal means in the heterogeneity model. Unvailable when performing multilevel/multivariate meta-analysis.")
			}
		}

		Group
		{
			columns:	2

			CheckBox
			{
				label:		qsTr("Estimated Marginal Means")
				name: 		"estimatedMarginalMeansHeterogeneity"
				checked:	true
				Layout.preferredWidth: 250 * jaspTheme.uiScale

				CheckBox
				{
					name:		"estimatedMarginalMeansHeterogeneityAddAdjustedEstimate"
					label:		qsTr("Add adjusted estimate")
					info: qsTr("Include the adjusted heterogeneity estimate, which accounts for the moderators in the heterogeneity meta-regression model. This provides the heterogeneity estimate adjusted for the influence of moderators, as opposed to the pooled heterogeneity estimate which combines the heterogeneity estimates across all studies.")
				}

				DoubleField
				{
					name:			"estimatedMarginalMeansHeterogeneitySdFactorCovariates"
					label:			qsTr("SD factor covariates")
					defaultValue: 	1
					min:			0
					enabled:		estimatedMarginalMeansHeterogeneitySelectedVariables.columnsTypes.includes("scale")
					Layout.preferredWidth: 350 * jaspTheme.uiScale
					info: qsTr("Standard deviation factor for covariates when computing estimated marginal means; applies to scale variables.")
				}

				DropDown
				{
					name:			"estimatedMarginalMeansHeterogeneityTransformation"
					label:			qsTr("Heterogeneity transformation")
					info: qsTr("Transformation to apply to the heterogeneity estimate: tau (𝜏) or tau-squared (𝜏²).")
					values:			[
							{ label: qsTr("𝜏")		, value: "tau"	},
							{ label: qsTr("𝜏²")	, value: "tau2"	}
						]
				}
			}

			CheckBox
			{
				enabled:	estimatedMarginalMeansHeterogeneitySelectedVariables.columnsTypes.includes("nominal")
				name:		"contrastsHeterogeneity"
				label:		qsTr("Contrasts")

				DropDown
				{
					name:	"contrastsHeterogeneityPValueAdjustment"
					label:	qsTr("P-value adjustment")
					values:
					[
						{ label: qsTr("None"),				value: "none"},
						{ label: "Bonferroni",				value: "bonferroni"},
						{ label: "Holm",					value: "holm"},
						{ label: "Hochberg",				value: "hochberg"},
						{ label: "Hommel",					value: "hommel"},
						{ label: "Benjamini & Hochberg",	value: "benjaminiHochberg"},
						{ label: "Benjamini & Yekutieli",	value: "benjaminiYekutieli"}
					]
				}
			}
		}
	}
}
