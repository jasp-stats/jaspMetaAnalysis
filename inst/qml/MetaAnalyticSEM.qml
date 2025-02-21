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
		id:				models
		name:			"models"
		maximumItems:	9
		newItemName:	qsTr("Model 1")
		optionKey:		"name"

		content: Group
		{
			TextArea
			{
				name:		"syntax"
				width:		models.width
				textType:	JASP.TextTypeLavaan
			}

			Group
			{
				CheckBox
				{
					text:		qsTr("Replace constraints")
					name:		"replaceConstraints"
					checked:	false
				}

				CheckBox
				{
					text:		qsTr("Fix latent variance to 1")
					name:		"fixLatentVarianceTo1"
					checked:	false
				}
			}
		}
	}
	

	CheckBox
	{
		text:		qsTr("Model summary")
		name:		"modelSummary"
		checked:	false

		DropDown
		{
			name:		"modelSummaryConfidenceIntervalType"
			label:		qsTr("Confidence interval type")
			values:
			[
				{ label: qsTr("Standard errors")	, value: "standardErrors"	},
				{ label: qsTr("Likelihood based")   , value: "likelihoodBased"	}
			]
		}
	}

	CheckBox
	{
		text:		qsTr("Path diagram")
		name:		"pathDiagram"
		checked:	false

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
		}

		CheckBox
		{
			name:		"pathDiagramShowParameters"
			label:		qsTr("Show parameters")
			checked:	false
		}

		Group
		{
			DoubleField
			{
				name: "pathDiagramManifestNodeWidth"
				label: qsTr("Manifest node width")
				value: 6
			}
			
			DoubleField 
			{
				name: "pathDiagramLatentNodeWidth"
				label: qsTr("Latent node width")
				value: 8
			}
			
			DoubleField
			{
				name: "pathDiagramUnitVectorNodeWidth"
				label: qsTr("Unit vector node width")
				value: 8
			}
			
			DoubleField
			{
				name: "pathDiagramLabelSize"
				label: qsTr("Label size")
				value: 1.3
			}
			
			DoubleField
			{
				name: "pathDiagramEdgeLabelSize"
				label: qsTr("Edge label size")
				value: 0.9
			}

			IntegerField
			{
				name: "pathDiagramNumberOfDigits"
				label: qsTr("Number of digits")
				value: 4
			}

		}

	}

}
