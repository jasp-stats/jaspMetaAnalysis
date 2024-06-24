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
	title:						qsTr("Forest Plot")
	property string module:		"metaAnalysis"
	columns:					1

	CheckBox
	{
		id:			forestPlotStudyInformation
		name: 		"forestPlotStudyInformation"
		text: 		qsTr("Study information")
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
		}
	}

	ComponentsList
	{
		name:				"forestPlotStudyInformationSelectedVariablesSettings"
		source:				"forestPlotStudyInformationSelectedVariables"
		enabled:			forestPlotStudyInformation.checked
		visible:			forestPlotStudyInformationSelectedVariables.count > 0
		headerLabels:		[qsTr("Title"), qsTr("Width"), qsTr("Alignment")]

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
				text:		qsTr("Predicted effects")
				enabled:	effectSize.count == 1 && effectSizeStandardError.count == 1
				checked:	false
				Layout.preferredWidth: 300 * jaspTheme.uiScale
			}

			CheckBox
			{
				name:			"forestPlotStudyInformationStudyWeights"
				text:			qsTr("Study weights")
				enabled:		forestPlotStudyInformation.checked
			}
		}

		Group
		{
			title:		qsTr("Order")
			DropDown
			{
				name:			"forestPlotStudyInformationOrderBy"
				label:			qsTr("By")
				addEmptyValue:	true
			}

			CheckBox
			{
				name:		"forestPlotStudyInformationOrderAscending"
				text:		qsTr("Ascending")
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
	}

	VariablesForm
	{
		preferredHeight:	250 * preferencesModel.uiScale
		enabled:			forestPlotEstimatedMarginalMeans.checked

		AvailableVariablesList
		{
			name:			"forestPlotEstimatedMarginalMeansModelVariables"
			title:			qsTr("Model variables")
			source:			[{ name: "effectSizeModelTerms", use: "noInteraction" }]
		}

		AssignedVariablesList
		{
			id:				forestPlotEstimatedMarginalMeansSelectedVariables
			name:			"forestPlotEstimatedMarginalMeansSelectedVariables"
			title:			qsTr("Selected variables")
			allowTypeChange:false
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
			}

			CheckBox
			{
				name:		"forestPlotEstimatedMarginalMeansCoefficientTests"
				id:			forestPlotEstimatedMarginalMeansCoefficientTests
				enabled:	forestPlotEstimatedMarginalMeans.checked
				label:		qsTr("Coefficient tests")
				checked:	true

				DoubleField
				{
					name:			"forestPlotEstimatedMarginalMeansCoefficientTestsAgainst"
					text:			qsTr("Against")
					defaultValue:	0
				}
			}

		}

		CheckBox
		{
			name:		"forestPlotEstimatedMarginalMeansAdjustedEffectSizeEstimate"
			label:		qsTr("Adjusted effect size estimate")
			enabled:	forestPlotEstimatedMarginalMeans.checked
		}
	}


	Divider { }

	CheckBox
	{
		name:		"forestPlotModelInformation"
		id:			forestPlotModelInformation
		enabled:	effectSize.count == 1 && effectSizeStandardError.count == 1
		text:		qsTr("Model information")
	}

	Group
	{
		enabled:	forestPlotModelInformation.checked
		columns:	2

		CheckBox
		{
			name:		"forestPlotPooledEffectSizeEstimate"
			text:		qsTr("Pooled effect size estimate")
			checked:	true				
			Layout.preferredWidth: 300 * jaspTheme.uiScale
		}

		CheckBox
		{
			name:		"forestPlotPooledEffectSizeTest"
			text:		qsTr("Pooled effect size test")
			checked:	true
		}

		CheckBox
		{
			name:		"forestPlotResidualHeterogeneityTest"
			text:		qsTr("Residual heterogeneity test")
			checked:	true
		}

		CheckBox
		{
			name:		"forestPlotResidualHeterogeneityEstimate"
			text:		qsTr("Residual heterogeneity estimate")
			enabled:	(method.value != "fixedEffects" || method.value != "equalEffects")
			checked:	true
		}

		CheckBox
		{
			name:		"forestPlotEffectSizeModerationTest"
			text:		qsTr("Effect size moderation test")
			enabled:	sectionModel.effectSizeModelTermsCount > 0
			checked:	true
		}

		CheckBox
		{
			name:		"forestPlotHeterogeneityModerationTest"
			text:		qsTr("Heterogeneity moderation test")
			enabled:	sectionModel.heterogeneityModelTermsCount > 0
			checked:	true
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
			CheckBox
			{
				name:		"forestPlotPredictionIntervals"
				text:		qsTr("Prediction intervals")
				checked:	true
				Layout.preferredWidth: 300 * jaspTheme.uiScale
			}

			CheckBox
			{
				name:			"forestPlotEstimatesAndConfidenceIntervals"
				text:			qsTr("Estimates and confidence intervals")
				checked:		true
			}

			CheckBox
			{
				name:			"forestPlotTestsInRightPanel"
				text:			qsTr("Tests in right panel")
				checked:		false
			}
		}

		Group
		{
			title:	qsTr("Mapping")

			DropDown
			{
				name:			"forestPlotMappingColor"
				label:			qsTr("Color")
				addEmptyValue:	true
			}

			DropDown
			{
				name:			"forestPlotMappingShape"
				label:			qsTr("Shape")
				addEmptyValue:	true
			}
		}
	


		Group
		{
			title:		qsTr("Relative Size")

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
			}

			DropDown
			{
				label:		qsTr("Tests information")
				name:		"forestPlotAuxiliaryTestsInformation"
				values:		[
						{ label: qsTr("Statistic and p-value")		, value: "statisticAndPValue"	},
						{ label: qsTr("P-value")					, value: "pValue"				}
				]
			}

			DropDown
			{
				name:			"forestPlotAuxiliaryPlotColor"
				label:			qsTr("Color")
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
			}

			CheckBox
			{
				name:			"forestPlotAuxiliarySetXAxisLimit"
				text:			qsTr("X-axis limits")
				childrenOnSameRow:	true

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

	}


}