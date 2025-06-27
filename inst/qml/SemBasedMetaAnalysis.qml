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
import "../qml/qml_components" as MA

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
		}
	}
	
	Group
	{

		CheckBox
		{
			text:		qsTr("Model fit measures")
			name:		"modelFitMeasures"
			checked:	false
			info:		qsTr("Show a summary of the model fit statistics.")
		}

		CheckBox
		{
			text:		qsTr("Pairwise model comparison")
			name:		"pairwiseModelComparison"
			checked:	false
			info:		qsTr("Show a pairwise model comparison table.")
		}

/*
		CheckBox
		{
			text:		qsTr("Model convergence")
			name:		"modelConvergence"
			checked:	false
			info:		qsTr("Show a summary of the model convergence.")
		}
*/

		CheckBox
		{
			text:		qsTr("Model summary")
			name:		"modelSummary"
			checked:	true
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
	}

	MA.SemBasedMetaAnalysisPlot{}

}
