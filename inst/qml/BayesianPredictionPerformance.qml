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
import "../qml/qml_components" as MA

Form {
	id: form

	MA.PredictionPerformanceData
	{
		id:					predictionPerformanceData
	}

	MA.PredictionPerformanceInference
	{
		analysisType:		"bayesian"
		measure:			predictionPerformanceData.measure
		inputN:				predictionPerformanceData.inputN
		inputO:				predictionPerformanceData.inputO
		inputE:				predictionPerformanceData.inputE
	}

	MA.PredictionPerformancePriors
	{
		measure:			predictionPerformanceData.measure
	}

	Section
	{
		title:		qsTr("MCMC Diagnostics")
		expanded:	false

		Group
		{
			title:		qsTr("Plots")

			CheckBox
			{
				name:		"diagnosticsRmPlot"
				label:		qsTr("Running means")
			}

			CheckBox
			{
				name:		"diagnosticsAcPlot"
				label:		qsTr("Autocorrelation")
			}

			CheckBox
			{
				name:		"diagnosticsGelmanRubinPlot"
				label:		qsTr("Gelman Rubin")
			}
		}
	}

	Section
	{
		title:		qsTr("Advanced")
		expanded:	false

		Group
		{
			IntegerField
			{
				label: 			qsTr("Adapt")
				name: 			"adapt"
				defaultValue: 	1000
				min:			100
			}

			IntegerField
			{
				label: 			qsTr("Burnin")
				name: 			"burnin"
				defaultValue: 	4000
				min:			500
			}

			IntegerField
			{
				label: 			qsTr("Sample")
				name: 			"sample"
				defaultValue: 	10000
				min:			1000
			}
			IntegerField
			{
				label: 			qsTr("Chains")
				name: 			"chains"
				defaultValue: 	4
				min:			1
			}
		}
	}
}
