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

Group
{
	title:		qsTr("Ranking")
	info: qsTr("Options for ranking the treatments of the network. The direction of beneficial treatment effects is set by 'Beneficial effect' below the data format.")

	// the metrics from resampled treatment effects (rankogram()) need the standard model and the inverse variance pooling
	readonly property bool resamplingAvailable: !poolingMethod.countBased && networkModel.value === "standard"

	CheckBox
	{
		name:		"rankingTable"
		text:		qsTr("Treatment ranking")
		checked:	false
		info: qsTr("Include a table ranking the treatments.")

		CheckBox
		{
			name:		"rankingPScore"
			text:		qsTr("P-score")
			checked:	true
			info: qsTr("Include the P-score of each treatment, the mean extent of certainty that the treatment is better than the other treatments (analytic, no simulation).")
		}

		CheckBox
		{
			name:		"rankingSucra"
			text:		qsTr("SUCRA")
			checked:	true
			enabled:	resamplingAvailable
			info: qsTr("Include the surface under the cumulative ranking curve (SUCRA) of each treatment. Computed from resampled treatment effects (the number of simulations is set in the 'Advanced' section). Not available for the component model, a network of two treatments, and the Mantel-Haenszel, non-central hypergeometric, and penalized logistic regression pooling methods.")
		}

		CheckBox
		{
			name:		"rankingProbabilityOfBeingBest"
			text:		qsTr("Probability of being best")
			checked:	true
			enabled:	resamplingAvailable
			info: qsTr("Include the probability that each treatment is the best treatment. Computed from resampled treatment effects (the number of simulations is set in the 'Advanced' section). Not available for the component model, a network of two treatments, and the Mantel-Haenszel, non-central hypergeometric, and penalized logistic regression pooling methods.")
		}

		CheckBox
		{
			name:		"rankingMeanRank"
			text:		qsTr("Mean rank")
			checked:	true
			enabled:	resamplingAvailable
			info: qsTr("Include the mean rank of each treatment. Computed from resampled treatment effects (the number of simulations is set in the 'Advanced' section). Not available for the component model, a network of two treatments, and the Mantel-Haenszel, non-central hypergeometric, and penalized logistic regression pooling methods.")
		}

		CheckBox
		{
			name:		"rankingMedianRank"
			text:		qsTr("Median rank")
			checked:	true
			enabled:	resamplingAvailable
			info: qsTr("Include the median rank of each treatment. Computed from resampled treatment effects (the number of simulations is set in the 'Advanced' section). Not available for the component model, a network of two treatments, and the Mantel-Haenszel, non-central hypergeometric, and penalized logistic regression pooling methods.")
		}
	}

	CheckBox
	{
		name:		"rankogram"
		text:		qsTr("Rankogram")
		checked:	false
		enabled:	resamplingAvailable
		info: qsTr("Visualize the probabilities of each treatment taking each rank, computed from resampled treatment effects (the number of simulations is set in the 'Advanced' section). Not available for the component model, a network of two treatments, and the Mantel-Haenszel, non-central hypergeometric, and penalized logistic regression pooling methods.")

		CheckBox
		{
			name:		"rankogramCumulative"
			text:		qsTr("Cumulative")
			checked:	false
			info: qsTr("Show the cumulative rank probabilities instead of the rank probabilities.")
		}
	}
}
