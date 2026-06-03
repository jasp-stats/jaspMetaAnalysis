import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Group
{
	property bool showPredictionIntervals:		false
	property bool showConditionalEstimates:		false
	property bool showTestsInRightPanel:		false
	property bool showTestsInformation:			false
	property bool enableSubgroupSettings:			false
	property bool settingsEnabled:				true
	property string transformEffectSizeValue:	"none"

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
				visible:	showPredictionIntervals
				Layout.preferredWidth: 300 * jaspTheme.uiScale
				info: qsTr("Include prediction intervals of the estimated marginal means and the model information output.")
			}

			CheckBox
			{
				name:			"forestPlotEstimatesAndConfidenceIntervals"
				text:			qsTr("Estimates and confidence intervals")
				checked:		true
				info: qsTr("Include effect size estimates and confidence intervals summary text in the right panel.")
			}

			CheckBox
			{
				name:		"forestPlotConditionalEstimates"
				text:		qsTr("Conditional estimates")
				visible:	showConditionalEstimates
				checked:	false
				info: qsTr("Display the conditional effect, heterogeneity, and estimated marginal means estimates.")
			}

			CheckBox
			{
				name:			"forestPlotTestsInRightPanel"
				text:			qsTr("Tests in right panel")
				checked:		false
				visible:		showTestsInRightPanel
				info: qsTr("Move test results text to the right panel.")
			}

			DropDown
			{
				name:			"forestPlotAllignLeftPanel"
				label:			qsTr("Align left panel")
				startValue:		"right"
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
			enabled:	enableSubgroupSettings
			info: qsTr("Specify the forest plot behavior for subgroup analysis.")

			CheckBox
			{
				name:			"forestPlotSubgroupShowTitles"
				text:			qsTr("Show subgroup titles")
				checked:		true
				info: qsTr("Show the subgroup heading labels in the forest plot.")
			}

			CheckBox
			{
				name:			"forestPlotSubgroupPanelsWithinSubgroup"
				text:			qsTr("Panels within subgroup")
				checked:		false
				info: qsTr("Group the output panels within their subgroup membership.")
			}

			CheckBox
			{
				name:			"forestPlotSubgroupFullDatasetEstimatedMarginalMeans"
				text:			qsTr("Full dataset estimated marginal means")
				checked:		true
				info: qsTr("Include the full dataset estimated marginal means in the forest plot if subgroups are specified.")
			}

			CheckBox
			{
				name:			"forestPlotSubgroupFullDatasetModelInformation"
				text:			qsTr("Full dataset model information")
				checked:		true
				info: qsTr("Include the full dataset model information in the forest plot if subgroups are specified.")
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
			defaultValue:	2
			inclusive: 		JASP.None
			info: qsTr("Number of digits printed for the effect size and confidence intervals summary text.")
		}

		DropDown
		{
			label:		qsTr("Tests information")
			name:		"forestPlotAuxiliaryTestsInformation"
			visible:	showTestsInformation
			values:		[
					{ label: qsTr("Statistic and p-value")		, value: "statisticAndPValue"	},
					{ label: qsTr("P-value")					, value: "pValue"				}
			]
		}

		DropDown
		{
			name:			"forestPlotAuxiliaryPlotColor"
			enabled:		forestPlotMappingColor.value === ""
			label:			qsTr("Color")
			info: qsTr("Change color of the plotted objects.")
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

		Group
		{
			title:		qsTr("X-axis")

			TextField
			{
				name:			"forestPlotAuxiliaryEffectLabel"
				text:			qsTr("Label")
				value:			"Effect Size"
				info: qsTr("Change the x-axis label of the forest plot.")
			}

			CheckBox
			{
				name:			"forestPlotAuxiliarySetXAxisLimit"
				text:			qsTr("Limits")
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
					negativeValues:	true
					min:			forestPlotAuxiliarySetXAxisLimitLower
					inclusive: 		JASP.None
				}
			}

			CheckBox
			{
				name:				"forestPlotAuxiliarySetXAxisTicks"
				text:				qsTr("Ticks")
				childrenOnSameRow:	true
				info: qsTr("Specify x-axis ticks manually as comma-separated numbers or fractions.")

				TextField
				{
					label:			""
					name:			"forestPlotAuxiliarySetXAxisTicksValues"
					value:			"-1, -0.5, 0, 0.5, 1"
					fieldWidth:		200 * preferencesModel.uiScale
				}
			}

			CheckBox
			{
				name:		"forestPlotAuxiliaryXAxisTransformLabelsOnly"
				text:		qsTr("Transform labels only")
				checked:	true
				enabled:	transformEffectSizeValue !== "none"
				info: qsTr("Apply the selected effect size transformation only to the x-axis labels.")
			}
		}
	}


	Group
	{
		title:		qsTr("Size")
		info: qsTr("Adjust the size of the forest plot elements.")

		DoubleField
		{
			name:			"forestPlotSizeEstimates"
			text:			qsTr("Estimates")
			defaultValue:	1
			min:			0
			inclusive: 		JASP.None
		}

		DoubleField
		{
			name:			"forestPlotSizeText"
			text:			qsTr("Text")
			defaultValue:	1
			min:			0
			inclusive: 		JASP.None
		}

		DoubleField
		{
			name:			"forestPlotSizeAxisLabels"
			text:			qsTr("Axis labels")
			defaultValue:	1
			min:			0
			inclusive: 		JASP.None
		}

		DoubleField
		{
			name:			"forestPlotSizeRow"
			text:			qsTr("Row")
			defaultValue:	1
			min:			0
			inclusive: 		JASP.None
		}

		DoubleField
		{
			name:			"forestPlotSizeLeftPanel"
			text:			qsTr("Left panel")
			defaultValue:	1
			min:			0
			inclusive: 		JASP.None
		}

		DoubleField
		{
			name:			"forestPlotSizePlotArea"
			text:			qsTr("Plot area")
			defaultValue:	1
			min:			0
			inclusive: 		JASP.None
		}

		DoubleField
		{
			name:			"forestPlotSizeRightPanel"
			text:			qsTr("Right panel")
			defaultValue:	1
			min:			0
			inclusive: 		JASP.None
		}
	}

	Group
	{
		title:	qsTr("Mapping")
		info: qsTr("Select a variable for encoding the color or shape of the plotted objects.")

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
