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
	title:		qsTr("Network Plot")
	columns:	2
	info: qsTr("Options for visualizing the structure of the treatment network.")

	CheckBox
	{
		id:			networkPlot
		name:		"networkPlot"
		text:		qsTr("Network plot")
		checked:	true
		info: qsTr("Include a plot of the treatment network. Treatments are shown as nodes, and the direct comparisons as edges.")

		DropDown
		{
			name:		"networkPlotLayout"
			label:		qsTr("Layout")
			startValue:	"circle"
			info: qsTr("Arrangement of the treatments in the network plot. The network plot of the component model of a disconnected network and the network plot of a network of two treatments always use the circle layout.")
			values:		[
				{ label: qsTr("Circle")							, value: "circle"	, info: qsTr("Treatments arranged on a circle in the order of the network.")},
				{ label: qsTr("Circle (minimal crossings)")		, value: "optimal"	, info: qsTr("Treatments arranged on a circle in an order that reduces the crossing of the edges.")},
				{ label: qsTr("Stress majorization")			, value: "stress"	, info: qsTr("Layout in which the distances between the treatments reflect the network structure.")}
			]
		}

		DropDown
		{
			name:		"networkPlotEdgeWidth"
			label:		qsTr("Edge width")
			startValue:	"numberOfStudies"
			info: qsTr("Quantity determining the width of the edges.")
			values:		[
				{ label: qsTr("Number of studies")	, value: "numberOfStudies"			, info: qsTr("Edges are wider for comparisons with more studies.")},
				{ label: qsTr("Precision")			, value: "inverseStandardError"		, info: qsTr("Edges are wider for more precise direct estimates (inverse standard error). The component model of a disconnected network has no direct estimates: the edge widths show the number of studies then.")},
				{ label: qsTr("Equal")				, value: "equal"					, info: qsTr("All edges have the same width.")}
			]
		}

		DropDown
		{
			name:		"networkPlotNodeSize"
			label:		qsTr("Node size")
			startValue:	"equal"
			info: qsTr("Quantity determining the size of the nodes. The sample size is available for arm-based binary and continuous data.")
			values:		(function() {
				var sizes = [
					{ label: qsTr("Equal")				, value: "equal"			, info: qsTr("All nodes have the same size.")},
					{ label: qsTr("Number of studies")	, value: "numberOfStudies"	, info: qsTr("Nodes are larger for treatments evaluated in more studies.")}
				];
				if (dataFormat.value === "armBased" && (outcomeType.value === "binary" || outcomeType.value === "continuous"))
					sizes.push({ label: qsTr("Sample size")	, value: "sampleSize"	, info: qsTr("Nodes are larger for treatments with more participants.")});
				return sizes;
			})()
		}

		CheckBox
		{
			name:		"networkPlotNumberOfStudies"
			text:		qsTr("Number of studies on edges")
			checked:	false
			info: qsTr("Show the number of studies of each comparison on the edges.")
		}

		CheckBox
		{
			name:		"networkPlotMultiArmStudies"
			text:		qsTr("Highlight multi-arm studies")
			checked:	false
			info: qsTr("Highlight the treatments of multi-arm studies by colored areas.")
		}
	}

	Group
	{
		enabled:	networkPlot.checked

		Group
		{
			title:		qsTr("Size")
			info: qsTr("Adjust the size of the network plot elements (relative to the default size).")

			DoubleField
			{
				name:			"networkPlotSizeNodes"
				text:			qsTr("Nodes")
				defaultValue:	1
				min:			0
				inclusive: 		JASP.None
				info: qsTr("Relative size of the nodes (treatments).")
			}

			DoubleField
			{
				name:			"networkPlotSizeEdges"
				text:			qsTr("Edges")
				defaultValue:	1
				min:			0
				inclusive: 		JASP.None
				info: qsTr("Relative width of the edges (direct comparisons).")
			}

			DoubleField
			{
				name:			"networkPlotSizeText"
				text:			qsTr("Text")
				defaultValue:	1
				min:			0
				inclusive: 		JASP.None
				info: qsTr("Relative size of the treatment labels and the numbers of studies.")
			}
		}
	}
}
