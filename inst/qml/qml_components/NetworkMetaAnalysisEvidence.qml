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
	title:		qsTr("Evidence Structure")
	columns:	2
	enabled:	networkModel.value === "standard"
	info: qsTr("Options for describing how the evidence of the network contributes to the network estimates. Available for the standard model.")

	Group
	{
		CheckBox
		{
			name:		"directEvidence"
			text:		qsTr("Direct evidence proportion")
			checked:	false
			info: qsTr("Include a table with the proportion of direct evidence contributing to each network estimate, the mean path length, and the minimal parallelism (the last two describe the indirect evidence; König et al., 2013). The mean path length and the minimal parallelism are not available for the Mantel-Haenszel, non-central hypergeometric, and penalized logistic regression pooling methods. The minimal parallelism is not defined for a network of two treatments.")
		}

		CheckBox
		{
			name:		"contributionMatrix"
			text:		qsTr("Contribution matrix")
			checked:	false
			enabled:	!poolingMethod.countBased
			info: qsTr("Include a matrix with the contribution of each direct comparison to each network estimate. Not available for the Mantel-Haenszel, non-central hypergeometric, and penalized logistic regression pooling methods.")

			DropDown
			{
				name:		"contributionMatrixMethod"
				label:		qsTr("Method")
				startValue:	"shortestPath"
				info: qsTr("Method used to compute the contributions of the direct comparisons.")
				values:		[
					{ label: qsTr("Shortest path")		, value: "shortestPath"	, info: qsTr("Iterative algorithm that decomposes the flow of evidence in the network into streams, each of them following a shortest path (Papakonstantinou et al., 2018).")},
					{ label: qsTr("Random walk")		, value: "randomWalk"	, info: qsTr("Analytical random walk approach that decomposes the flow of evidence in the network (Davies et al., 2022).")},
					{ label: qsTr("Pseudoinverse")		, value: "pseudoinverse", info: qsTr("Contributions derived from an L2 solution based on a Moore-Penrose pseudoinverse (Rücker et al., 2024).")}
				]
			}
		}
	}

	Group
	{
		CheckBox
		{
			name:		"studyImpact"
			text:		qsTr("Study impact")
			checked:	false
			info: qsTr("Include a matrix with the impact of each study on each network estimate, the reduction in precision of the network estimate if the study is removed. The columns follow the comparisons of the treatment effects table (versus the reference treatment, or all pairwise comparisons if selected there). The computation refits the network once per study and is slow: it can take several minutes, in particular for the Mantel-Haenszel and non-central hypergeometric pooling methods.")
		}

		CheckBox
		{
			name:		"pairwiseMetaAnalyses"
			text:		qsTr("Pairwise meta-analyses")
			checked:	false
			info: qsTr("Include a table with the separate pairwise meta-analyses of the directly compared treatments (the studies that directly compare the two treatments, separately for each comparison). The between-study variance of each comparison is estimated by the method selected for the network meta-analysis; a comparison with a single study has no between-study variance.")
		}
	}
}
