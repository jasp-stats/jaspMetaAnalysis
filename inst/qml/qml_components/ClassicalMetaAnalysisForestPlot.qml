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
	title:						qsTr("Forest Plot")
	property string module:		"metaAnalysis"
	columns:					1
	info: qsTr("Options for visualizing study-level information, estimated marginal means, and the model information in an all encompassing forest plot. Different sections of the forest plot can be individually enabled/disabled.")

	CheckBox
	{
		id:			forestPlotStudyInformation
		name: 		"forestPlotStudyInformation"
		text: 		qsTr("Study information")
		info: qsTr("Add study-level information panel to the forest plot. There are three sections of the study-level information panel: a) the left section with study labels, names, and etc designed via the 'Selected Variables' option, b) the middle section visualizing the estimates and confidence intervals based on the meta-analytic input, c) the right section textually summarizing the estimates and confidence intervals based on the meta-analytic input.")
	}

	VariablesForm
	{
		preferredHeight: 	150 * preferencesModel.uiScale
		enabled:			forestPlotStudyInformation.checked

		AvailableVariablesList
		{			
			name:				"forestPlotStudyInformationAllVariables"
		}

		AssignedVariablesList
		{
			name:				"forestPlotStudyInformationSelectedVariables"
			id:					forestPlotStudyInformationSelectedVariables
			title:				qsTr("Selected Variables")
			allowedColumns:		["nominal"]
			info: qsTr("Select variables containing study-level information to be printed in the left section of the panel. Each variable creates a new column in the panel.")
		}
	}

	ComponentsList
	{
		name:				"forestPlotStudyInformationSelectedVariablesSettings"
		source:				"forestPlotStudyInformationSelectedVariables"
		enabled:			forestPlotStudyInformation.checked
		visible:			forestPlotStudyInformationSelectedVariables.count > 0
		headerLabels:		[qsTr("Title"), qsTr("Width"), qsTr("Alignment")]
		info: qsTr("Adjust the Title, Width, and Alignment of each column in the left section of the study-level information panel.")

		rowComponent: 			RowLayout
		{
			Text
			{
				text:					rowValue
				Layout.preferredWidth:	100 * preferencesModel.uiScale
				elide:					Text.ElideRight
			}

			TextField
			{
				label:				""
				name:				"title"
				value:				""
				fieldWidth: 		120 * preferencesModel.uiScale
				useExternalBorder:	false
				showBorder: 		true
			}

			DoubleField
			{
				label: 				""
				name: 				"width"
				value:				"1"
				min: 				0
				inclusive: 			JASP.None
				fieldWidth:			40 * preferencesModel.uiScale
				useExternalBorder:	false
				showBorder:			true
			}

			DropDown
			{
				label: 				""
				name: 				"alignment"
				values:				[
						{ label: qsTr("Left")		, value: "left"		},
						{ label: qsTr("Middle")		, value: "middle"	},
						{ label: qsTr("Right")		, value: "right"	}
					]
				fieldWidth:			40 * preferencesModel.uiScale
				useExternalBorder:	false
				showBorder:			true
			}
		}
	}

	Group
	{
		enabled:	forestPlotStudyInformation.checked
		columns:	2

		Group
		{
			CheckBox
			{
				name:		"forestPlotStudyInformationPredictedEffects"
				id:			forestPlotStudyInformationPredictedEffects
				text:		qsTr("Predicted effects")
				enabled:	effectSize.count == 1 && effectSizeStandardError.count == 1
				visible:	module === "metaAnalysis" || module === "metaAnalysisMultilevelMultivariate"
				checked:	false
				Layout.preferredWidth: 300 * jaspTheme.uiScale
				info: qsTr("Include predicted effect sizes in the middle section of the study-level information panel.")
			}

			CheckBox
			{
				name:			"forestPlotStudyInformationStudyWeights"
				text:			qsTr("Study weights")
				enabled:		forestPlotStudyInformation.checked
				visible:		module === "metaAnalysis" || module === "metaAnalysisMultilevelMultivariate"
				checked:		false
				info: qsTr("Include the study weights in the right section of the study-level information panel.")
			}

			CheckBox
			{
				name:			"forestPlotStudyInformationSecondaryConfidenceInterval"
				text:			qsTr("Secondary confidence interval")
				enabled:		forestPlotStudyInformation.checked
				Layout.preferredWidth: 300 * jaspTheme.uiScale
				childrenOnSameRow:	true
				info: qsTr("Include secondary confidence interval for effect sizes in the middle section of the study-level information panel.")

				CIField
				{ 
					name:			"forestPlotStudyInformationSecondaryConfidenceIntervalLevel"
					text:			""
					defaultValue:	89
				}
			}
		}

		Group
		{

			Group
			{
				title:		qsTr("Order")
				info: qsTr("Order the study-level information panel by a variable.")

				DropDown
				{
					name:			"forestPlotStudyInformationOrderBy"
					label:			qsTr("By")
					addEmptyValue:	true
					fieldWidth:		125 * preferencesModel.uiScale
				}

				CheckBox
				{
					name:		"forestPlotStudyInformationOrderAscending"
					text:		qsTr("Ascending")
				}
			}

			Group
			{
				title:		qsTr("Aggregate")
				info: qsTr("Aggregate the study-level information panel by a variable. If selected, `Predicted effects` and all `Study information` in the tight panel is supressed.")

				DropDown
				{
					name:			"forestPlotStudyInformationAggregateBy"
					label:			qsTr("By")
					addEmptyValue:	true
					fieldWidth:		125 * preferencesModel.uiScale
				}

				RadioButtonGroup
				{
					name:			"forestPlotStudyInformationAggregateMethod"
					info: qsTr("Select the method for aggregating the estimates of the study-level information panel.")

					RadioButton
					{
						name: 		"boxplot";
						label: 		qsTr("Boxplot")
						checked: 	true
					}

					RadioButton
					{
						name: 				"bubbles"
						label: 				qsTr("Bubbles")
						childrenOnSameRow: 	true

						DoubleField
						{
							name:			"forestPlotStudyInformationAggregateMethodBubbleRelativeSize"
							label:			qsTr("Relative size")
							defaultValue:	1
							min:			0
							inclusive: 		JASP.None
							info: qsTr("Set the relative size of the observed estimates.")
						}
					}
				}
			}
		}
	}



	Divider { }

	CheckBox
	{
		name:		"forestPlotEstimatedMarginalMeans"
		id:			forestPlotEstimatedMarginalMeans
		text:		qsTr("Estimated marginal means")
		enabled:	sectionModel.effectSizeModelTermsCount > 0
		info: qsTr("Add estimated marginal means information to the forest plot. Available when effect size meta-regression is specified.")
	}

	VariablesForm
	{
		preferredHeight:	250 * preferencesModel.uiScale
		enabled:			forestPlotEstimatedMarginalMeans.checked

		AvailableVariablesList
		{
			name:			"forestPlotEstimatedMarginalMeansModelVariables"
			title:			qsTr("Model Variables")
			source:			[{ name: "effectSizeModelTerms", use: "noInteraction" }]
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
			CheckBox
			{
				name:		"forestPlotEstimatedMarginalMeansTermTests"
				id:			forestPlotEstimatedMarginalMeansTermTests
				enabled:	forestPlotEstimatedMarginalMeans.checked
				label:		qsTr("Term tests")
				checked:	true
				Layout.preferredWidth: 350 * jaspTheme.uiScale
				info: qsTr("Include the omnibus term test of variables included in the estimated marginal means. The null hypothesis states that the effect size at all levels of the categorical variable are equal or that there is no linear association between the effect size and the continuous variable.")
			}

			CheckBox
			{
				name:		"forestPlotEstimatedMarginalMeansCoefficientTests"
				id:			forestPlotEstimatedMarginalMeansCoefficientTests
				enabled:	forestPlotEstimatedMarginalMeans.checked
				visible:		module === "metaAnalysis" || module === "metaAnalysisMultilevelMultivariate"
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
				enabled:	forestPlotEstimatedMarginalMeans.checked
				visible:	module === "RoBMA" || module === "NoBMA"
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


	Divider { }

	CheckBox
	{
		name:		"forestPlotModelInformation"
		id:			forestPlotModelInformation
		enabled:	effectSize.count == 1 && effectSizeStandardError.count == 1
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
				name:		"forestPlotResidualHeterogeneityTest"
				text:		qsTr("Test")
				checked:	true
				info: qsTr("Include the test of the residual heterogeneity in the model information section.")
			}

			Group
			{
				title:		qsTr("Estimate")
				columns:	2
				visible:	module === "metaAnalysis" || module === "RoBMA"  || module === "NoBMA"
				enabled:	(method.value != "fixedEffects" || method.value != "equalEffects")

				CheckBox
				{
					text:		qsTr("𝜏")
					name:		"forestPlotHeterogeneityEstimateTau"
					checked:	module === "metaAnalysis" || module === "RoBMA"  || module === "NoBMA"
					info: qsTr("Include the meta-analytic 𝜏, the square root of the estimated between-study variance in the model information section. Not available for multilevel/multivariate meta-analysis.")
				}

				CheckBox
				{
					text:		qsTr("𝜏²")
					name:		"forestPlotHeterogeneityEstimateTau2"
					checked:	false
					info: qsTr("Include the meta-analytic 𝜏², the estimated between-study variance in the model information section. Not available for multilevel/multivariate meta-analysis.")
				}

				CheckBox
				{
					text:		qsTr("I²")
					name:		"forestPlotHeterogeneityEstimateI2"
					enabled:	(module === "RoBMA"  || module === "NoBMA") || sectionModel.heterogeneityModelTermsCount == 0
					checked:	false
					info: qsTr("Include the meta-analytic I², the percentage of total variation across studies due to heterogeneity in the model information section. Not available for multilevel/multivariate meta-analysis.")
				}

				CheckBox
				{
					text:		qsTr("H²")
					name:		"forestPlotHeterogeneityEstimateH2"	
					enabled:	(module === "RoBMA"  || module === "NoBMA") || sectionModel.heterogeneityModelTermsCount == 0
					checked:	false
					info: qsTr("Include the meta-analytic H², an index indicating the ratio of total variability to sampling variability in the model information section. Not available for multilevel/multivariate meta-analysis.")
				}
			}
			
			CheckBox
			{
				name:		"forestPlotHeterogeneityModerationTest"
				text:		qsTr("Moderation test")
				enabled:	sectionModel.heterogeneityModelTermsCount > 0
				visible:	module === "metaAnalysis"
				checked:	module === "metaAnalysis"
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
				visible:	module === "metaAnalysis" || module === "metaAnalysisMultilevelMultivariate"
				enabled:	(module === "metaAnalysis" || module === "metaAnalysisMultilevelMultivariate") && !(method.value == "fixedEffects" || method.value == "equalEffects")
				info: qsTr("Include a fixed effect meta-analytic effect size estimate in the model information section. Not available if the model was already fitted with fixed effects or the model contains heterogeneity meta-regression.")
			}

			CheckBox
			{
				name:		"forestPlotEffectSizeFixedEffectTest"
				text:		qsTr("Fixed effect estimate test")
				checked:	module === "metaAnalysis" || module === "metaAnalysisMultilevelMultivariate"
				visible:	module === "metaAnalysis" || module === "metaAnalysisMultilevelMultivariate"
				enabled:	forestPlotEffectSizeFixedEffectEstimate.checked
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
				enabled:	forestPlotEffectSizePooledEstimate.checked
				checked:	true
				info: qsTr("Include the test of the overall meta-analytic effect size estimate in the model information section.")
			}

			CheckBox
			{
				name:		"forestPlotEffectSizeModerationTest"
				text:		qsTr("Moderation test")
				enabled:	sectionModel.effectSizeModelTermsCount > 0
				visible: 	module === "metaAnalysis" || module === "metaAnalysisMultilevelMultivariate"
				checked:	module === "metaAnalysis" || module === "metaAnalysisMultilevelMultivariate"
				info: qsTr("Include the omnibus effect size moderation test in the model information section. Available when effect size meta-regression is specified.")
			}
		}

		Group
		{
			title:		qsTr("Publication Bias")
			visible: 	module === "RoBMA"

			CheckBox
			{
				name:		"forestPlotPublicationBiasTest"
				text:		qsTr("Test")
				enabled:	module === "RoBMA" && publicationBiasAdjustment.value != "none"
				checked:	module === "RoBMA" && publicationBiasAdjustment.value != "none"
				info: qsTr("Include the publication bias test in the model information section.")
			}
		}
	}


	Divider {}

	Text
	{
		text:	qsTr("Settings")
	}

	Group
	{
		columns:	2


		Group
		{
			Group
			{
				title:		qsTr("General")

				CheckBox
				{
					name:		"forestPlotPredictionIntervals"
					text:		qsTr("Prediction intervals")
					checked:	true
					Layout.preferredWidth: 300 * jaspTheme.uiScale
					info: qsTr("Include prediction intervals of the estimated marginal means and the model information output.")
				}

				CheckBox
				{
					name:			"forestPlotEstimatesAndConfidenceIntervals"
					text:			qsTr("Estimates and confidence intervals")
					checked:		true
					info: qsTr("Include effect size estimates and confidence intervals summary text in the right panel of the forest plot.")
				}

				CheckBox
				{
					name:		"forestPlotConditionalEstimates"
					text:		qsTr("Conditional estimates")
					visible:	module === "RoBMA" || module === "NoBMA"
					checked:	false
					info: qsTr("Display the conditional effect, heterogeneity, and estimated marginal means estimates.")
				}

				CheckBox
				{
					name:			"forestPlotTestsInRightPanel"
					text:			qsTr("Tests in right panel")
					checked:		false
					info: qsTr("Move test results text to the right panel.")
				}

				DropDown
				{
					name:			"forestPlotAllignLeftPanel"
					label:			qsTr("Align left panel")
					enabled:		forestPlotModelInformation.checked || forestPlotEstimatedMarginalMeans.checked
					startValue:		"right"
					info: qsTr("Align text of the `Estimated marginal means` and  `Model Information` sections.")
					values:		[
						{ label: qsTr("Left")		, value: "left"		},
						{ label: qsTr("Middle")		, value: "middle"	},
						{ label: qsTr("Right")		, value: "right"	}
					]
				}
			}

			Group
			{
				title:		qsTr("Subgroup")
				enabled:	subgroup.count == 1
				info:		qsTr("Specify the forest plot behavior in the case of a subgroup analysis. These options is only available when the subgroup analysis is selected.")

				CheckBox
				{
					name:			"forestPlotSubgroupPanelsWithinSubgroup"
					text:			qsTr("Panels within subgroup")
					checked:		true
					info: qsTr("Group the output panels within their subgroup membership, i.e., the output panels are presented for each subgroup in a sequential order (i.e., Study information (Subgroup 1), Estimated marginal means (Subgroup 1), Model information (Subgroup 1), Study information (Subgroup 2), Estimated marginal means (Subgroup 2), Model information (Subgroup 2), .... If unchecked, the output is group by the panel membership first, i.e., Study information (Subgroup 1), Study information (Subgroup 2), Esimated marginal means (Subgroup 1), Estimated marginal means (Subgroup 2), Model information (Subgroup 1), Model information (Subgroup 2)...")
				}

				CheckBox
				{
					name:			"forestPlotSubgroupFullDatasetEstimatedMarginalMeans"
					text:			qsTr("Full dataset estimated marginal means")
					checked:		true
					info: qsTr("Include the full dataset estimated marginal means in the forest plot if subgroups are specified. This option overrides the `Include full dataset in subgroup analysis` setting in the `Advanced` section.")
				}

				CheckBox
				{
					name:			"forestPlotSubgroupFullDatasetModelInformation"
					text:			qsTr("Full dataset model information")
					checked:		true
					info: qsTr("Include the full dataset model information in the forest plot if subgroups are specified. This option overrides the `Include full dataset in subgroup analysis` setting in the `Advanced` section.")
				}
			}
		}


		Group
		{
			title:		qsTr("Auxiliary")

			IntegerField
			{
				name:			"forestPlotAuxiliaryDigits"
				text:			qsTr("Digits")
				min:			1
				value:			2
				inclusive: 		JASP.None
				info: qsTr("Number of digits printed for the effect size and confidence intervals summary text.")
			}

			DropDown
			{
				label:		qsTr("Tests information")
				name:		"forestPlotAuxiliaryTestsInformation"				
				visible:	module === "metaAnalysis" || module === "metaAnalysisMultilevelMultivariate"
				values:		[
						{ label: qsTr("Statistic and p-value")		, value: "statisticAndPValue"	},
						{ label: qsTr("P-value")					, value: "pValue"				}
				]
			}

			DropDown
			{
				name:			"forestPlotAuxiliaryPlotColor"
				enabled:		forestPlotMappingColor.value == ""
				label:			qsTr("Color")
				info: qsTr("Change color of the plotted objects. Only available if no color mapping is selected.")
				values:			[
						{ label: qsTr("Black")		, value: "black"},
						{ label: qsTr("Blue")		, value: "blue"	},
						{ label: qsTr("Red")		, value: "red"	}
					]
			}

			CheckBox
			{
				name:				"forestPlotAuxiliaryAddVerticalLine"
				text:				qsTr("Add vertical line")
				childrenOnSameRow:	true
				info: qsTr("Add a solid vertical line in the forest plot.")

				DoubleField
				{
					name:			"forestPlotAuxiliaryAddVerticalLineValue"
					defaultValue:	0
					negativeValues:	true
				}
			}

			CheckBox
			{
				name:				"forestPlotAuxiliaryAddVerticalLine2"
				text:				qsTr("Add vertical line (2)")
				childrenOnSameRow:	true
				info: qsTr("Add a dotted vertical line in the forest plot.")

				DoubleField
				{
					name:			"forestPlotAuxiliaryAddVerticalLineValue2"
					defaultValue:	0
					negativeValues:	true
				}
			}

			TextField
			{
				name:			"forestPlotAuxiliaryEffectLabel"
				text:			qsTr("X-axis label")
				value:			"Effect Size"
				info: qsTr("Change the x-axis label of the forest plot (the default 'Effect Size' changes in accordance with the selected effect size transformation).")
			}

			CheckBox
			{
				name:			"forestPlotAuxiliarySetXAxisLimit"
				text:			qsTr("X-axis limits")
				childrenOnSameRow:	false
				info: qsTr("Change the default x-axis limits.")

				DoubleField
				{
					name:			"forestPlotAuxiliarySetXAxisLimitLower"
					id:				forestPlotAuxiliarySetXAxisLimitLower
					text:			qsTr("Lower")
					defaultValue:	-1
					negativeValues:	true
					max:			forestPlotAuxiliarySetXAxisLimitUpper
					inclusive: 		JASP.None
				}

				DoubleField
				{
					name:			"forestPlotAuxiliarySetXAxisLimitUpper"
					id:				forestPlotAuxiliarySetXAxisLimitUpper
					text:			qsTr("Upper")
					defaultValue:	1
					min:			forestPlotAuxiliarySetXAxisLimitLower			
					inclusive: 		JASP.None
				}
			}

		}


		Group
		{
			title:		qsTr("Relative Size")
			info: qsTr("Adjust the relative size of the forest plot components.")

			DoubleField
			{
				name:			"forestPlotRelativeSizeEstimates"
				text:			qsTr("Estimates")
				defaultValue:	1
				min:			0
				inclusive: 		JASP.None
			}

			DoubleField
			{
				name:			"forestPlotRelativeSizeText"
				text:			qsTr("Text")
				defaultValue:	1
				min:			0
				inclusive: 		JASP.None
			}

			DoubleField
			{
				name:			"forestPlotRelativeSizeAxisLabels"
				text:			qsTr("Axis labels")
				defaultValue:	1
				min:			0
				inclusive: 		JASP.None
			}

			DoubleField
			{
				name:			"forestPlotRelativeSizeRow"
				text:			qsTr("Row")
				defaultValue:	1
				min:			0
				inclusive: 		JASP.None
			}

			DoubleField
			{
				name:			"forestPlotRelativeSizeLeftPanel"
				text:			qsTr("Left panel")
				defaultValue:	0.5
				min:			0
				inclusive: 		JASP.None
			}

			DoubleField
			{
				name:			"forestPlotRelativeSizeMiddlePanel"
				text:			qsTr("Middle panel")
				defaultValue:	1
				min:			0
				inclusive: 		JASP.None
			}

			DoubleField
			{
				name:			"forestPlotRelativeSizeRightPanel"
				text:			qsTr("Right panel")
				defaultValue:	0.5
				min:			0
				inclusive: 		JASP.None
			}

			CheckBox
			{
				name:			"forestPlotAuxiliaryAdjustWidthBasedOnText"
				text:			qsTr("Adjust width based on text")
				checked:		true
				info: qsTr("Turn off the automatic width adjustment of the individual components.")
			}
		}

		Group
		{
			title:	qsTr("Mapping")
			info: qsTr("Select a variable for encoding the color or shape of the study information and the estimated marginal means output.")

			DropDown
			{
				name:			"forestPlotMappingColor"
				id:				forestPlotMappingColor
				label:			qsTr("Color")
				addEmptyValue:	true
				allowedColumns:	["nominal"]
				fieldWidth:		125 * preferencesModel.uiScale
			}

			DropDown
			{
				name:			"forestPlotMappingShape"
				label:			qsTr("Shape")
				addEmptyValue:	true
				allowedColumns:	["nominal"]
				fieldWidth:		125 * preferencesModel.uiScale
			}
		}
	}



}
