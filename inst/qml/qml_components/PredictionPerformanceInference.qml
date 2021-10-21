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
import QtQuick			2.12
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0
import QtQuick.Layouts	1.3
import "../qml_components" as MA

Section
{
	title:		qsTr("Inference")
	expanded:	true

	property string analysisType:		"classical"
	property string	measure:			"OE"
	property int inputN:				0
	property int inputO:				0
	property int inputE:				0

	Group
	{

		MA.ClassicalMetaAnalysisMethod
		{
			visible:	analysisType == "classical"
			id:			method
		}

		DropDown
		{
			name:		"linkOE"
			visible:	measure == "OE"
			label:		qsTr("Within-study variation")
			values: 	if (method.value == "Fixed Effects" || method.value == "Maximum Likelihood" || analysisType == "bayesian"){
				[
					{ label: qsTr("Normal/Log"),		value: "normal/log"			},
					{ label: qsTr("Normal/Identity"),	value: "normal/identity"	},
					{ label: qsTr("Poisson/Log"),		value: "poisson/log"		}
				]
			} else {
				[
					{ label: qsTr("Normal/Log"),		value: "normal/log"			},
					{ label: qsTr("Normal/Identity"),	value: "normal/identity"	}
				]
			}
		}

		DropDown
		{
			name:		"linkCstat"
			label:		qsTr("Within-study variation")
			visible:	measure == "cstat"
			values: 	[
				{ label: qsTr("Normal/Logit"),		value: "normal/logit"		},
				{ label: qsTr("Normal/Identity"),	value: "normal/identity"	}
			]
		}
	}

	Group
	{
		CheckBox
		{
			name:		"priorAndPosteriorPlot"
			label:		qsTr("Prior and posterior plot")
			visible:	analysisType == "bayesian"
		}

		CheckBox
		{
			name:	"forestPlot"
			label:	qsTr("Forest plot")

			CheckBox
			{
				name:	"forestPlotLabels"
				label:	qsTr("Labels")
				checked: true
			}

			CheckBox
			{
				name:	"forestPlotEstimates"
				label:	qsTr("Estimates")
				checked: true
			}
		}
	}

	CheckBox
	{
		name:	"exportColumns"
		id:		exportColumns
		label:	measure === "OE" ? qsTr("Export O:E ratios") : qsTr("Export C-statistics")

		// need to be each set of the variables separatelly, they will get overwriten otherwise
		ComputedColumnField
		{
			name: 				"exportOE"
			text: 				qsTr("Column name")
			fieldWidth: 		150 * preferencesModel.uiScale
			placeholderText: 	qsTr("O:E ratios")
			visible:			measure == "OE"
			enabled:			measure == "OE" && exportColumns.checked
		}

		ComputedColumnField
		{
			name: 				"exportOElCI"
			text: 				qsTr("Column name (lCI)")
			fieldWidth: 		150 * preferencesModel.uiScale
			placeholderText:	qsTr("O:E ratios (lCI)")
			visible:			measure == "OE"
			enabled:			measure == "OE" && exportColumns.checked
		}

		ComputedColumnField
		{
			name: 				"exportOEuCI"
			text: 				qsTr("Column name (uCI)")
			fieldWidth: 		150 * preferencesModel.uiScale
			placeholderText:	qsTr("O:E ratios (uCI)")
			visible:			measure == "OE"
			enabled:			measure == "OE" && exportColumns.checked
		}

		ComputedColumnField
		{
			name: 				"exportCstat"
			id:					newColumnName
			text: 				qsTr("Column name")
			fieldWidth: 		150 * preferencesModel.uiScale
			placeholderText:	qsTr("C-statistics")
			visible:			measure == "cstat"
			enabled:			measure == "cstat" && exportColumns.checked
		}

		ComputedColumnField
		{
			name: 				"exportCstatlCI"
			text: 				qsTr("Column name (lCI)")
			fieldWidth: 		150 * preferencesModel.uiScale
			placeholderText:	qsTr("C-statistics (lCI)")
			visible:			measure == "cstat"
			enabled:			measure == "cstat" && exportColumns.checked
		}

		ComputedColumnField
		{
			name: 				"exportCstatuCI"
			text: 				qsTr("Column name (uCI)")
			fieldWidth: 		150 * preferencesModel.uiScale
			placeholderText: 	qsTr("C-statistics (uCI)")
			visible:			measure == "cstat"
			enabled:			measure == "cstat" && exportColumns.checked
		}
	}

	CheckBox
	{
		name:	"funnelAsymmetryTest"
		label:	qsTr("Funnel plot asymmetry test")

		CheckBox
		{
			name:	"funnelAsymmetryTestEggerUW"
			id:		funnelAsymmetryTestEggerUW
			label:	qsTr("Egger (unweighted)")
			checked: true
		}

		CheckBox
		{
			name:	"funnelAsymmetryTestEggerFIV"
			id:		funnelAsymmetryTestEggerFIV
			label:	qsTr("Egger (multiplicative overdispersion)")
			checked: false
		}

		CheckBox
		{
			name:	"funnelAsymmetryTestMacaskillFIV"
			id:		funnelAsymmetryTestMacaskillFIV
			label:	qsTr("Macaskill")
			checked: false
			enabled: inputN === 1
		}

		CheckBox
		{
			name:	"funnelAsymmetryTestMacaskillFPV"
			id:		funnelAsymmetryTestMacaskillFPV
			label:	qsTr("Macaskill (pooled)")
			checked: false
			enabled: inputN === 1 && inputO === 1
		}

		CheckBox
		{
			name:	"funnelAsymmetryTestPeters"
			id:		funnelAsymmetryTestPeters
			label:	qsTr("Peters")
			checked: false
			enabled: inputN === 1 && inputO === 1
		}

		CheckBox
		{
			name:	"funnelAsymmetryTestDebrayFIV"
			id:		funnelAsymmetryTestDebrayFIV
			label:	qsTr("Debray")
			checked: false
			enabled: inputO === 1
		}

		/* Not implemented
		CheckBox
		{
			name:	"funnelAsymmetryTestDebrayFAV"
			label:	qsTr("Debray (FAV)")
			checked: false
		}
		*/

		CheckBox
		{
			name:	"funnelAsymmetryTestPlot"
			label:	qsTr("Plot")
			enabled: funnelAsymmetryTestEggerUW.checked || funnelAsymmetryTestEggerFIV.checked || funnelAsymmetryTestMacaskillFIV.checked || funnelAsymmetryTestMacaskillFPV.checked || funnelAsymmetryTestPeters.checked || funnelAsymmetryTestDebrayFIV.checked
			checked: false
		}
	}
}
