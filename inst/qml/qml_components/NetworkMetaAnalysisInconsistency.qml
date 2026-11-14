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
	title:		qsTr("Inconsistency")
	columns:	2
	enabled:	networkModel.value === "standard"
	info: qsTr("Options for assessing the inconsistency between direct and indirect evidence in the network. Available for the standard model.")

	property alias nodeSplittingForestPlotChecked:	nodeSplittingForestPlot.checked

	Group
	{
		CheckBox
		{
			name:		"designDecomposition"
			text:		qsTr("Design decomposition")
			checked:	false
			enabled:	!poolingMethod.countBased
			info: qsTr("Include the decomposition of the Cochran's Q statistic (common-effect model) into the within-design heterogeneity (per design) and the between-design inconsistency after detaching single designs. The test of the full design-by-treatment interaction model of the random-effects model is shown in the table of the network meta-analytic tests. Not available for the Mantel-Haenszel, non-central hypergeometric, and penalized logistic regression pooling methods.")
		}

		CheckBox
		{
			name:		"netHeatPlot"
			text:		qsTr("Net heat plot")
			checked:	false
			enabled:	!poolingMethod.countBased
			info: qsTr("Include a net heat plot visualizing the inconsistency between designs and its drivers. Available for networks with more than two designs and degrees of freedom for the between-design inconsistency. Not available for the Mantel-Haenszel, non-central hypergeometric, and penalized logistic regression pooling methods.")
		}
	}

	Group
	{
		CheckBox
		{
			name:		"nodeSplitting"
			text:		qsTr("Direct and indirect evidence")
			checked:	false
			enabled:	!poolingMethod.dispersionModel
			info: qsTr("Include a table comparing the direct and indirect evidence for each treatment comparison (node-splitting) with a z-test of the disagreement between the direct and the indirect evidence. The difference is the direct minus the indirect estimate (under the exponential transformation, the ratio of the direct to the indirect estimate). For the Mantel-Haenszel and non-central hypergeometric pooling methods only SIDDE is available, which is slow (the computation takes several minutes); not available for penalized logistic regression. The test of the difference uses normal-theory (z) inference.")

			DropDown
			{
				name:		"nodeSplittingMethod"
				label:		qsTr("Method")
				startValue:	"backCalculation"
				// (the Mantel-Haenszel and non-central hypergeometric fits always use SIDDE)
				enabled:	!poolingMethod.countBased
				info: qsTr("Method used to split the direct and indirect evidence. Not available for the Mantel-Haenszel and non-central hypergeometric pooling methods, which always use SIDDE (slow: several minutes).")
				values:		[
					{ label: qsTr("Back-calculation")	, value: "backCalculation"	, info: qsTr("Back-calculation method: the indirect evidence is derived from the network and direct estimates.")},
					{ label: qsTr("SIDDE")				, value: "sidde"			, info: qsTr("Separate indirect from direct design evidence (SIDDE). Slow: the computation can take several minutes.")}
				]
			}

			DropDown
			{
				name:		"nodeSplittingShow"
				label:		qsTr("Show")
				startValue:	"all"
				info: qsTr("Select the comparisons included in the table.")
				values:		[
					{ label: qsTr("All comparisons")					, value: "all"			, info: qsTr("Show all comparisons.")},
					{ label: qsTr("With direct and indirect evidence")	, value: "both"			, info: qsTr("Show only comparisons with both direct and indirect evidence.")},
					{ label: qsTr("With direct evidence")				, value: "withDirect"	, info: qsTr("Show only comparisons with direct evidence.")}
				]
			}

			CheckBox
			{
				id:			nodeSplittingForestPlot
				name:		"nodeSplittingForestPlot"
				text:		qsTr("Forest plot")
				checked:	false
				info: qsTr("Visualize the direct, indirect, and network estimates of each comparison in a forest plot.")
			}
		}
	}
}
