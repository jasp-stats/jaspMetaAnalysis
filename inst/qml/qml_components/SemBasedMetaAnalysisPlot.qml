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
		label:		qsTr("Show parameter names")
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
