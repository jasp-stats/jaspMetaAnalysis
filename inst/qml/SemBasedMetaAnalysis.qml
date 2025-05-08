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

Form
{

	TabView
	{
		name:			"models"
		maximumItems:	9
		newItemName:	qsTr("Model 1")
		content: Column
		{
			spacing: 5
			TextArea
			{
				name:		"syntax"
				id:			syntax
				textType:	JASP.TextTypeLavaan
				info:		qsTr("Specify model using a lavaan style syntax.")
			}

			Group
			{
				columns: 2

				ComponentsList
				{
					Layout.columnSpan:	1
					preferredWidth:		syntax.width / 2
					preferredHeight:	150
					title: 				qsTr("Observed Variables")
					name:				"observedVariableList"
					source: 			"syntax"
					info:				qsTr("Specify observed variables.")
					rowComponent: 		CheckBox
					{
						label:		rowValue
						name:		"observedVariable"
						checked:	true
					}
				}

				Group
				{
					title: qsTr("Model Settings")

					CheckBox
					{
						text:		qsTr("Replace constraints")
						name:		"replaceConstraints"
						checked:	false
						info:		qsTr("Replace constraints in the model.")
					}

					CheckBox
					{
						text:		qsTr("Fix latent variance to 1")
						name:		"fixLatentVarianceTo1"
						checked:	false
						info:		qsTr("Fix the variance of latent variables to 1.")
					}
				}
			}
		}

	}
	

	CheckBox
	{
		text:		qsTr("Model summary")
		name:		"modelSummary"
		checked:	false
		info:		qsTr("Show a summary of the model coefficients and computed estimates.")

		DropDown
		{
			name:		"modelSummaryConfidenceIntervalType"
			label:		qsTr("Confidence interval type")
			values:
			[
				{ label: qsTr("Standard errors")	, value: "standardErrors"	},
				{ label: qsTr("Likelihood based")   , value: "likelihoodBased"	}
			]
			info:		qsTr("Method for computing confidence interval.")
		}
	}

	CheckBox
	{
		text:		qsTr("Path diagram")
		name:		"pathDiagram"
		checked:	false
		info:		qsTr("Show a path diagram of the model.")

		DropDown
		{
			name:	"pathDiagramLayout"
			label:	qsTr("Layout")
			values:
			[
				{ label: qsTr("Tree"),		value: "tree" },
				{ label: qsTr("Circle"),	value: "circle" },
				{ label: qsTr("Spring"),	value: "spring" },
				{ label: qsTr("Tree2"),		value: "tree2" },
				{ label: qsTr("Circle2"),	value: "circle2" }
			]
			info:	qsTr("Layout of the path diagram.")
		}

		CheckBox
		{
			name:		"pathDiagramShowParameterNames"
			label:		qsTr("Show parameters names")
			checked:	false
			info:		qsTr("Show parameter names instead of the estimates in the path diagram.")
		}

		Group
		{
			DoubleField
			{
				name: "pathDiagramManifestNodeWidth"
				label: qsTr("Manifest node width")
				value: 6
				info: qsTr("Width of manifest nodes.")
			}
			
			DoubleField 
			{
				name: "pathDiagramLatentNodeWidth"
				label: qsTr("Latent node width")
				value: 8
				info: qsTr("Width of latent nodes.")
			}
			
			DoubleField
			{
				name: "pathDiagramUnitVectorNodeWidth"
				label: qsTr("Unit vector node width")
				value: 8
				info: qsTr("Width of unit vector nodes.")
			}
			
			DoubleField
			{
				name: "pathDiagramLabelSize"
				label: qsTr("Label size")
				value: 1.3
				info: qsTr("Size of the labels.")
			}
			
			DoubleField
			{
				name: "pathDiagramEdgeLabelSize"
				label: qsTr("Edge label size")
				value: 0.9
				info: qsTr("Size of the edge labels.")
			}

			IntegerField
			{
				name: "pathDiagramNumberOfDigits"
				label: qsTr("Number of digits")
				value: 4
				info: qsTr("Number of digits for rounding estimates.")
			}

		}

	}

}
