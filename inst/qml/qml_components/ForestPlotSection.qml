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
	title:							qsTr("Forest Plot")
	property string analysisType:	"metaAnalysis"
	property string transformEffectSizeValue: "none"
	columns:						1
	info: qsTr("Options for visualizing study-level information, estimated marginal means, and the model information in an all encompassing forest plot. Different sections of the forest plot can be individually enabled/disabled.")

	// analysis type helpers
	readonly property bool isClassical:			analysisType === "metaAnalysis" || analysisType === "multilevelMultivariateMetaAnalysis" || analysisType === "generalizedMetaAnalysis" || analysisType === "mantelHaenszelPeto"
	readonly property bool isStandardClassical:	analysisType === "metaAnalysis" || analysisType === "multilevelMultivariateMetaAnalysis"
	readonly property bool isBayesian:			analysisType === "RoBMA" || analysisType === "NoBMA" || analysisType === "BiBMA"
	readonly property bool isMetaRegression:	(analysisType !== "mantelHaenszelPeto") && sectionModel.effectSizeModelTermsCount > 0
	readonly property bool isScaleRegression:	analysisType == "metaAnalysis" && sectionModel.heterogeneityModelTermsCount == 0

	CheckBox
	{
		id:			forestPlotStudyInformation
		name: 		"forestPlotStudyInformation"
		text: 		qsTr("Study information")
		info: qsTr("Add study-level information panel to the forest plot. There are three sections of the study-level information panel: a) the left section with study labels, names, and etc designed via the 'Selected Variables' option, b) the middle section visualizing the estimates and confidence intervals based on the meta-analytic input, c) the right section textually summarizing the estimates and confidence intervals based on the meta-analytic input.")
	}

	ForestPlotStudyInformation
	{
		panelEnabled:			forestPlotStudyInformation.checked
		predictedEffectsEnabled:	effectSize.count == 1 && effectSizeStandardError.count == 1
		showPredictedEffects:	isStandardClassical
		showStudyWeights:		isStandardClassical
		showSecondaryCI:		true
		showAggregate:			true
	}


	Group
	{
		visible:	analysisType !== "mantelHaenszelPeto"
		enabled:	analysisType !== "mantelHaenszelPeto"

		Divider { }

		CheckBox
		{
			name:		"forestPlotEstimatedMarginalMeans"
			id:			forestPlotEstimatedMarginalMeans
			text:		qsTr("Estimated marginal means")
			enabled:	isMetaRegression
			info: qsTr("Add estimated marginal means information to the forest plot. Available when effect size meta-regression is specified.")
		}

		VariablesForm
		{
			preferredHeight:	150 * preferencesModel.uiScale
			enabled:			forestPlotEstimatedMarginalMeans.checked

			AvailableVariablesList
			{
				name:			"forestPlotEstimatedMarginalMeansModelVariables"
				title:			qsTr("Model Variables")
				source:			analysisType === "mantelHaenszelPeto" ? undefined : [{ name: "effectSizeModelTerms", use: "noInteraction" }]
			}

			AssignedVariablesList
			{
				id:				forestPlotEstimatedMarginalMeansSelectedVariables
				name:			"forestPlotEstimatedMarginalMeansSelectedVariables"
				title:			qsTr("Selected Variables")
				allowTypeChange:false
				info: qsTr("Select variables for which the estimated marginal means are visualized.")
			}
		}

		Group
		{
			columns:	2

			Group
			{
				enabled:	forestPlotEstimatedMarginalMeans.checked && (isClassical || (bayesianModelAveragingEffectSize.checked || bayesianModelAveragingModerations.checked))

				CheckBox
				{
					name:		"forestPlotEstimatedMarginalMeansTermTests"
					id:			forestPlotEstimatedMarginalMeansTermTests
					label:		qsTr("Term tests")
					checked:	true
					Layout.preferredWidth: 350 * jaspTheme.uiScale
					info: qsTr("Include the omnibus term test of variables included in the estimated marginal means. The null hypothesis states that the effect size at all levels of the categorical variable are equal or that there is no linear association between the effect size and the continuous variable.")
				}

				CheckBox
				{
					name:		"forestPlotEstimatedMarginalMeansCoefficientTests"
					id:			forestPlotEstimatedMarginalMeansCoefficientTests
					visible:	isClassical
					label:		qsTr("Coefficient tests")
					checked:	true
					info: qsTr("Include coefficient tests of variables included in the estimated marginal means. The null hypothesis states that the estimated marginal mean for a given level equals the tested value.")

					DoubleField
					{
						name:			"forestPlotEstimatedMarginalMeansCoefficientTestsAgainst"
						text:			qsTr("Against")
						defaultValue:	0
						info: qsTr("Specify the test value for the coefficient tests.")
					}
				}

				CheckBox
				{
					name:		"forestPlotEstimatedMarginalMeansCoefficientTestsAgainst0"
					visible:	!isClassical
					label:		qsTr("Coefficient tests against 0")
					checked:	true
					info: qsTr("Include coefficient tests of variables included in the estimated marginal means against 0. The null hypothesis states that the estimated marginal mean for a given level equals the tested value.")
				}

			}

			CheckBox
			{
				name:		"forestPlotEstimatedMarginalMeansAdjustedEffectSizeEstimate"
				label:		qsTr("Adjusted effect size estimate")
				enabled:	forestPlotEstimatedMarginalMeans.checked
				info: qsTr("Include the adjusted effect size estimate in the estimated marginal means section.")
			}
		}
	}


	Divider { }

	CheckBox
	{
		name:		"forestPlotModelInformation"
		id:			forestPlotModelInformation
		enabled:	(effectSize.count == 1 && effectSizeStandardError.count == 1) || analysisType === "generalizedMetaAnalysis" || analysisType === "mantelHaenszelPeto"
		text:		qsTr("Model information")
		info: qsTr("Add meta-analytic model information to the forest plot.")
	}

	Group
	{
		enabled:	forestPlotModelInformation.checked
		columns:	2

		Group
		{
			title:		qsTr("Heterogeneity")
			Layout.preferredWidth: 300 * jaspTheme.uiScale

			CheckBox
			{
				name:		"forestPlotHeterogeneityTest"
				text:		qsTr("Test")
				visible:	(isClassical || analysisType !== "generalizedMetaAnalysis") || (isBayesian && bayesianModelAveragingHeterogeneity.checked)
				checked:	(isClassical || analysisType !== "generalizedMetaAnalysis") || (isBayesian && bayesianModelAveragingHeterogeneity.checked)
				info: qsTr("Include the test of the residual heterogeneity in the model information section.")
			}

			CheckBox
			{
				name:		"forestPlotHeterogeneityTestWald"
				text:		qsTr("Wald test")
				visible:	analysisType === "generalizedMetaAnalysis"
				checked:	analysisType === "generalizedMetaAnalysis"
				info: qsTr("Include the Wald-type test of the residual heterogeneity in the model information section.")
			}

			CheckBox
			{
				name:		"forestPlotHeterogeneityTestLRT"
				text:		qsTr("Likelihood ratio test")
				visible:	analysisType === "generalizedMetaAnalysis"
				checked:	analysisType === "generalizedMetaAnalysis"
				info: qsTr("Include the likelihood ratio test of the residual heterogeneity in the model information section.")
			}

			Group
			{
				title:		qsTr("Estimate")
				columns:	2
				enabled:	(method.value != "fixedEffects" || method.value != "equalEffects")

				CheckBox
				{
					text:		qsTr("\uD835\uDF0F")
					name:		"forestPlotHeterogeneityEstimateTau"
					visible:	analysisType !== "mantelHaenszelPeto"
					checked:	analysisType !== "mantelHaenszelPeto"
					info: qsTr("Include the meta-analytic \uD835\uDF0F, the square root of the estimated between-study variance in the model information section. Not available for multilevel/multivariate meta-analysis.")
				}

				CheckBox
				{
					text:		qsTr("\uD835\uDF0F\u00B2")
					name:		"forestPlotHeterogeneityEstimateTau2"
					visible:	analysisType !== "mantelHaenszelPeto"
					checked:	false
					info: qsTr("Include the meta-analytic \uD835\uDF0F\u00B2, the estimated between-study variance in the model information section. Not available for multilevel/multivariate meta-analysis.")
				}

				CheckBox
				{
					text:		qsTr("I\u00B2")
					name:		"forestPlotHeterogeneityEstimateI2"
					enabled:	analysisType !== "mantelHaenszelPeto" && !isScaleRegression
					checked:	false
					info: qsTr("Include the meta-analytic I\u00B2, the percentage of total variation across studies due to heterogeneity in the model information section. Not available for multilevel/multivariate and binomial meta-analysis.")
				}

				CheckBox
				{
					text:		qsTr("H\u00B2")
					name:		"forestPlotHeterogeneityEstimateH2"
					enabled:	analysisType !== "mantelHaenszelPeto" && !isScaleRegression
					checked:	false
					info: qsTr("Include the meta-analytic H\u00B2, an index indicating the ratio of total variability to sampling variability in the model information section. Not available for multilevel/multivariate and binomial meta-analysis.")
				}
			}

			CheckBox
			{
				name:		"forestPlotHeterogeneityModerationTest"
				text:		qsTr("Moderation test")
				enabled:	isScaleRegression
				visible:	analysisType === "metaAnalysis"
				checked:	analysisType === "metaAnalysis"
				info: qsTr("Include the omnibus heterogeneity moderation test in the model information section. Available when heterogeneity meta-regression is specified.")
			}
		}

		Group
		{
			title:		qsTr("Effect Size")

			CheckBox
			{
				name:		"forestPlotEffectSizeFixedEffectEstimate"
				text:		qsTr("Fixed effect estimate")
				id:			forestPlotEffectSizeFixedEffectEstimate
				checked:	false
				visible:	isStandardClassical
				enabled:	isStandardClassical && !(method.value === "fixedEffects" || method.value === "equalEffects")
				info: qsTr("Include a fixed effect meta-analytic effect size estimate in the model information section. Not available if the model was already fitted with fixed effects or the model contains heterogeneity meta-regression.")
			}

			CheckBox
			{
				name:		"forestPlotEffectSizeFixedEffectTest"
				text:		qsTr("Fixed effect estimate test")
				checked:	true
				visible:	isStandardClassical 
				enabled:	isStandardClassical && !(method.value === "fixedEffects" || method.value === "equalEffects") && forestPlotEffectSizeFixedEffectEstimate.checked
				info: qsTr("Include the test of the fixed effect meta-analytic effect size estimate in the model information section.")
			}

			CheckBox
			{
				name:		"forestPlotEffectSizePooledEstimate"
				text:		qsTr("Pooled estimate")
				id:			forestPlotEffectSizePooledEstimate
				checked:	true
				Layout.preferredWidth: 300 * jaspTheme.uiScale
				info: qsTr("Include the overall meta-analytic effect size estimate in the model information section.")
			}

			CheckBox
			{
				name:		"forestPlotEffectSizePooledEstimateTest"
				text:		qsTr("Pooled estimate test")
				enabled:	forestPlotEffectSizePooledEstimate.checked && (isClassical || bayesianModelAveragingEffectSize.checked)
				checked:	true
				info:
					if (isClassical && isMetaRegression)
						qsTr("Include the test of the overall meta-analytic effect size estimate in the model information section.")
					else
						qsTr("Include the test of the overall meta-analytic effect size estimate in the model information section. If a meta-regression model is fitted, the test corresponds to either the adjusted effect or the meta-regression intercept term and the corresponding estimate is also displayed. The test is available only if Bayesian model averaging for the effect is selected.")
			}

			CheckBox
			{
				name:		"forestPlotEffectSizeModerationTest"
				text:		qsTr("Moderation test")
				enabled:	isClassical && analysisType !== "mantelHaenszelPeto"
				visible: 	isClassical && analysisType !== "mantelHaenszelPeto"
				checked:	isClassical && analysisType !== "mantelHaenszelPeto"
				info: qsTr("Include the omnibus effect size moderation test in the model information section. Available when effect size meta-regression is specified.")
			}
		}

		Group
		{
			title:		qsTr("Publication Bias")
			visible: 	analysisType === "RoBMA"

			CheckBox
			{
				name:		"forestPlotPublicationBiasTest"
				text:		qsTr("Test")
				enabled:	publicationBiasAdjustment.value != "none"
				checked:	publicationBiasAdjustment.value != "none"
				info: qsTr("Include the publication bias test in the model information section.")
			}
		}
	}


	Divider {}

	Text
	{
		text:	qsTr("Settings")
	}

	ForestPlotSettings
	{
		settingsEnabled:			forestPlotStudyInformation.checked || forestPlotEstimatedMarginalMeans.checked || forestPlotModelInformation.checked
		showPredictionIntervals:	(isClassical && analysisType !== "mantelHaenszelPeto") || isBayesian
		showConditionalEstimates:	isBayesian
		showTestsInRightPanel:		true
		showTestsInformation:		isClassical
		enableSubgroupSettings:		subgroup.count > 0
		transformEffectSizeValue:	transformEffectSizeValue
	}
}
