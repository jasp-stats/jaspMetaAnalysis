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
	title:		qsTr("Forest Plot")
	columns:	1
	info: qsTr("Options for visualizing the network estimates in a forest plot. The settings below apply to all forest plots of the analysis.")

	CheckBox
	{
		id:			forestPlot
		name:		"forestPlot"
		text:		qsTr("Forest plot")
		checked:	false
		info: qsTr("Include a forest plot of the network treatment effects.")

		RadioButtonGroup
		{
			name:		"forestPlotComparisons"
			info: qsTr("Select the comparisons shown in the forest plot.")

			RadioButton
			{
				value:		"versusReference"
				label:		qsTr("Versus reference")
				checked:	true
				info: qsTr("Show the effect of each treatment versus the reference treatment.")
			}

			RadioButton
			{
				value:		"allPairwise"
				label:		qsTr("All pairwise comparisons")
				info: qsTr("Show the effects of all pairs of treatments, grouped by treatment (each treatment versus all other treatments).")
			}
		}

		DropDown
		{
			name:		"forestPlotOrder"
			label:		qsTr("Order")
			startValue:	"treatmentOrder"
			info: qsTr("Order of the treatments in the forest plot.")
			values:		[
				{ label: qsTr("Treatment order")		, value: "treatmentOrder"	, info: qsTr("Order of the treatments in the network.")},
				{ label: qsTr("Effect size")			, value: "effectSize"		, info: qsTr("Order by the estimated effect size.")},
				{ label: qsTr("Ranking (P-score)")		, value: "ranking"			, info: qsTr("Order by the P-score ranking of the treatments (the best treatment first, according to the 'Beneficial effect' setting below the data format).")}
			]
		}
	}

	CheckBox
	{
		id:			componentForestPlot
		name:		"componentForestPlot"
		text:		qsTr("Component forest plot")
		checked:	false
		enabled:	networkModel.value === "component"
		info: qsTr("Include a forest plot of the component effects. Available for the component model.")
	}

	Divider {}

	Text
	{
		text:	qsTr("Settings")
	}

	ForestPlotSettings
	{
		// the settings apply to every forest plot of the analysis
		enabled:						forestPlot.checked || sectionInconsistency.nodeSplittingForestPlotChecked || componentForestPlot.checked
		transformEffectSizeValue:		sectionStatistics.transformEffectSizeValue
		showPredictionIntervals:		true
		enablePredictionIntervals:		method.value !== "equalEffects" && !poolingMethod.noHeterogeneity && networkModel.value === "standard"
		enableSubgroupSettings:			subgroup.count > 0
		showMappingSettings:			false
		showSubgroupFullDatasetSettings:	false
		predictionIntervalsInfo:		qsTr("Include the prediction intervals of the network treatment effects in the forest plots (random-effects model of the inverse variance pooling).")
	}
}
