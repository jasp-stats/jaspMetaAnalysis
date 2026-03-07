import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Group
{
	columns: 1

	VariablesForm
	{
		preferredHeight:	150 * preferencesModel.uiScale

		AvailableVariablesList
		{
			name:				"forestPlotEstimateInformationAllVariables"
		}

		AssignedVariablesList
		{
			name:				"forestPlotEstimateInformationSelectedVariables"
			id:					forestPlotEstimateInformationSelectedVariables
			title:				qsTr("Selected Variables")
			allowedColumns:		["nominal"]
			info: qsTr("Select variables containing estimate-level information to be printed in the left section of the panel for estimate rows.")
		}
	}

	ComponentsList
	{
		name:				"forestPlotEstimateInformationSelectedVariablesSettings"
		source:				"forestPlotEstimateInformationSelectedVariables"
		visible:			forestPlotEstimateInformationSelectedVariables.count > 0
		headerLabels:		[qsTr("Title"), qsTr("Width"), qsTr("Alignment")]
		info: qsTr("Adjust the Title, Width, and Alignment of each column in the left section for estimate rows.")

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
}
