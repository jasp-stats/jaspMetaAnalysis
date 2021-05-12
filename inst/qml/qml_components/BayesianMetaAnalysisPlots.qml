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
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Section
{
	columns: 	1
	title: 		qsTr("Plots")

	property string modelTypeValue:			"BMA"
	property string modelDirectionValue:	"allPos"
	property string module:					"metaAnalysis"

	Group
	{
		columns: 2

		CheckBox
		{
			id: 		checkForest
			name: 		"checkForestPlot"
			label: 		qsTr("Forest plot")
			checked: 	true

			RadioButtonGroup
			{
				name: "forestPlot"

				RadioButton
				{
					id:		forestObserved
					name: 		"plotForestObserved"
					label: 		qsTr("Observed")
					checked: 	true
				}

				RadioButton
				{
					enabled: 	!(modelTypeValue == "FE")
					name: 		"plotForestEstimated"
					label: 		qsTr("Estimated")
				}

				RadioButton
				{
					enabled: 	!(modelTypeValue == "FE")
					name: 		"plotForestBoth"
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
				name:		"showLabels"
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
			name: 		"orderForest"
			title: 		qsTr("Order")
			enabled: 	checkForest.checked
			visible:	module == "metaAnalysis"

			RadioButton
			{
				name: 	"ascendingForest"
				label: 	qsTr("Ascending")
			}

			RadioButton
			{
				name: 	"descendingForest"
				label: 	qsTr("Descending")
			}

			RadioButton
			{
				name: 	"labelForest"
				label: 	qsTr("Row order")
			}
		}
	}

	CheckBox
	{
		name: 	"plotCumForest"
		label: 	qsTr("Cumulative forest plot")
	}

	CheckBox
	{
		name: 		"plotPosterior"
		label: 		qsTr("Prior and posterior")

		CheckBox
		{
			name: "addInfo"
			label: qsTr("Additional info")
		}

		CheckBox
		{
			name: "addLines"
			enabled: modelTypeValue == "BMA" || modelTypeValue == "CRE"
			label: qsTr("Add fixed and random effects posterior")
		}
		CheckBox
		{
			name: 	"shade"
			label: 	qsTr("Shade 95% CI")
		}
	}

	Group
	{
		title: qsTr("Sequential plot")

		CheckBox
		{
			name: 	"plotSequential"
			label: 	qsTr("Bayes factors")
		}

		CheckBox
		{
			name: 	"plotSeqPM"
			label: 	qsTr("Posterior model probabilities")
		}
	}
}