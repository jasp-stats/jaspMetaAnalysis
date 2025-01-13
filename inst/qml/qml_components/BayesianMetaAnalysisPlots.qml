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

Section
{
	columns: 	1
	title: 		qsTr("Plots")

	property alias plotForestObservedClick:	plotForestObserved.checked
	property string modelTypeValue:			"averaging"
	property string modelDirectionValue:	"positive"
	property string module:					"metaAnalysis"

	Group
	{
		columns: 2

		CheckBox
		{
			id: 		checkForest
			name: 		"forestPlot"
			label: 		qsTr("Forest plot")
			checked: 	true

			RadioButtonGroup
			{
				name: "forestPlotEffect"

				RadioButton
				{
					id:		plotForestObserved
					name: 		"observed"
					label: 		qsTr("Observed")
					checked: 	true
				}

				RadioButton
				{
					id:		plotForestEstimated
					enabled: 	!(modelTypeValue == "fixed")
					name: 		"estimated"
					label: 		qsTr("Estimated")
				}

				RadioButton
				{
					id:		plotForestBoth
					enabled: 	!(modelTypeValue == "fixed")
					name: 		"both"
					label: 		qsTr("Both")
				}
			}
		}

		Group
		{
			visible:		module == "cochrane"
			enabled: 		checkForest.checked
			
			CheckBox
			{
				name:		"forestPlotLabel"
				text:		qsTr("Show labels")
				checked:	true
				visible:	module == "cochrane"
			}

			DropDown
			{
				name:			"forestPlotOrder"
				label:			qsTr("Ordering")
				visible:		module == "cochrane"
				currentIndex:	1
				values: [
					{ label: qsTr("Year (ascending)")			, value: "yearAscending"			},
					{ label: qsTr("Year (descending)")			, value: "yearDescending"			},
					{ label: qsTr("Effect size (ascending)")	, value: "effectSizeAscending"		},
					{ label: qsTr("Effect size (descending)")	, value: "effectSizeDescending"		}
				]
			}
		}

		RadioButtonGroup
		{
			name: 		"forestPlotRowOrder"
			title: 		qsTr("Order")
			enabled: 	checkForest.checked
			visible:	module == "metaAnalysis"

			RadioButton
			{
				name: 	"ascending"
				label: 	qsTr("Ascending")
			}

			RadioButton
			{
				name: 	"descending"
				label: 	qsTr("Descending")
			}

			RadioButton
			{
				name: 	"rowOrder"
				label: 	qsTr("Row order")
			}
		}
	}

	CheckBox
	{
		name: 	"cumulativeForestPlot"
		label: 	qsTr("Cumulative forest plot")
		CheckBox
		{
			name: "cumulativeForestPlotPrior"
			label: qsTr("Add prior")
		}
	}

	CheckBox
	{
		name: 		"priorPosterior"
		label: 		qsTr("Prior and posterior")

		CheckBox
		{
			name: "priorPosteriorAdditionalInfo"
			label: qsTr("Additional info")
		}

		CheckBox
		{
			name: "priorPosteriorFixedAndRandom"
			enabled: modelTypeValue == "averaging" || modelTypeValue == "constrainedRandom"
			label: qsTr("Add fixed and random effects posterior")
		}
		CheckBox
		{
			name: 	"priorPosteriorCi"
			label: 	qsTr("Shade 95% CI")
		}
	}

	Group
	{
		title: qsTr("Sequential plot")

		CheckBox
		{
			name: 	"bfSequentialPlot"
			label: 	qsTr("Bayes factors")
		}

		CheckBox
		{
			name: 	"modelProbabilitySequentialPlot"
			label: 	qsTr("Posterior model probabilities")
		}
	}
}
