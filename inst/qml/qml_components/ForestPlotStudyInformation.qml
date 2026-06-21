import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Group
{
	property bool showPredictedEffects:		false
	property bool showStudyWeights:			false
	property bool showSecondaryCI:			false
	property bool showAggregate:			false
	property bool panelEnabled:				true
	property bool predictedEffectsEnabled:	true
	property bool showWeightDisplayOptions:	false
	property bool weightDisplayOptionsEnabled:
		panelEnabled &&
		showStudyWeights

	columns: 1

	VariablesForm
	{
		preferredHeight:	150 * preferencesModel.uiScale
		enabled:			panelEnabled

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
			info: qsTr("Select variables containing study-level information to be printed in the left section of the panel.")
		}
	}

	ComponentsList
	{
		name:				"forestPlotStudyInformationSelectedVariablesSettings"
		source:				"forestPlotStudyInformationSelectedVariables"
		enabled:			panelEnabled
		visible:			forestPlotStudyInformationSelectedVariables.count > 0
		headerLabels:		[qsTr("Title"), qsTr("Width"), qsTr("Alignment")]
		info: qsTr("Adjust the Title, Width, and Alignment of each column in the left section.")

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
		enabled:	panelEnabled
		columns:	2

		Group
		{
			CheckBox
			{
				name:		"forestPlotStudyInformationPredictedEffects"
				id:			forestPlotStudyInformationPredictedEffects
				text:		qsTr("Predicted effects")
				enabled:	predictedEffectsEnabled
				visible:	showPredictedEffects
				checked:	false
				info: qsTr("Include predicted effect sizes in the middle section of the study-level information panel.")
				Layout.preferredWidth: 300 * jaspTheme.uiScale
			}

			CheckBox
			{
				name:			"forestPlotStudyInformationStudyWeights"
				text:			qsTr("Study weights")
				enabled:		panelEnabled && showStudyWeights
				checked:		false
				info: qsTr("Include study weights in the right section of the study-level information panel. For multilevel/multivariate meta-analysis, diagonal model weights are shown, matching the default forest plot point-size weights.")
				Layout.preferredWidth: 300 * jaspTheme.uiScale

				CheckBox
				{
					name:		"forestPlotStudyInformationBoxplotWeightsPercentage"
					text:		qsTr("Percentage")
					checked:	true
					visible:	showWeightDisplayOptions
					enabled:	weightDisplayOptionsEnabled
					info: qsTr("Display user-supplied weights as percentages.")
				}

				CheckBox
				{
					name:		"forestPlotStudyInformationBoxplotWeightsNormalized"
					text:		qsTr("Normalized")
					checked:	true
					visible:	showWeightDisplayOptions
					enabled:	weightDisplayOptionsEnabled
					info: qsTr("Normalize displayed user-supplied weights to sum to 100.")
				}
			}

			CheckBox
			{
				name:				"forestPlotStudyInformationSecondaryConfidenceInterval"
				text:				qsTr("Secondary confidence interval")
				enabled:			panelEnabled
				visible:			showSecondaryCI
				childrenOnSameRow:	true
				info: qsTr("Include secondary confidence interval for effect sizes in the middle section of the study-level information panel.")
				Layout.preferredWidth: 300 * jaspTheme.uiScale

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
					info: qsTr("Select the variable by which the study-level information panel is ordered.")
				}

				CheckBox
				{
					name:		"forestPlotStudyInformationOrderAscending"
					text:		qsTr("Ascending")
					info: qsTr("Order the study-level information panel in ascending order.")
				}
			}

			Group
			{
				title:		qsTr("Aggregate")
				visible:	showAggregate
				info: qsTr("Aggregate the study-level information panel by a variable.")

				DropDown
				{
					name:			"forestPlotStudyInformationAggregateBy"
					label:			qsTr("By")
					addEmptyValue:	true
					fieldWidth:		125 * preferencesModel.uiScale
					info: qsTr("Select the variable by which the study-level information panel is aggregated.")
				}

				RadioButtonGroup
				{
					name:			"forestPlotStudyInformationAggregateMethod"
					info: qsTr("Select the method for aggregating the estimates.")

					RadioButton
					{
						name: 		"boxplot";
						label: 		qsTr("Boxplot")
						checked: 	true
						info: qsTr("Aggregate the estimates by boxplot.")
					}

					RadioButton
					{
						name: 				"bubbles"
						label: 				qsTr("Bubbles")
						childrenOnSameRow: 	true
						info: qsTr("Aggregate the estimates by bubbles.")

						DoubleField
						{
							name:			"forestPlotStudyInformationAggregateMethodBubbleRelativeSize"
							label:			qsTr("Relative size")
							defaultValue:	1
							min:			0
							inclusive: 		JASP.None
						}
					}
				}
			}
		}
	}
}
