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
	property string	measure:			"oeRatio"
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
			name:		"withinStudyVariation"
			label:		qsTr("Within-study variation")
			values: 	if (analysisType == "bayesian" && measure == "oeRatio"){ // removing as metamisc's link function is broken for frequentist option: method.value == "Fixed Effects" || method.value == "Maximum Likelihood" ||
				[
					{ label: qsTr("Normal/Log"),		value: "normal/log"			},
					{ label: qsTr("Normal/Identity"),	value: "normal/identity"	},
					{ label: qsTr("Poisson/Log"),		value: "poisson/log"		}
				]
			} else if (measure == "oeRatio"){
				[
					{ label: qsTr("Normal/Log"),		value: "normal/log"			},
					{ label: qsTr("Normal/Identity"),	value: "normal/identity"	}
				]
			} else {
				[
					{ label: qsTr("Normal/Logit"),		value: "normal/logit"			},
					{ label: qsTr("Normal/Identity"),	value: "normal/identity"	}
				]
			}
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
		name:	"exportComputedEffectSize"
		id:		exportColumns
		label:	qsTr("Export computed effect size")

		// need to be each set of the variables separatelly, they will get overwriten otherwise
		ComputedColumnField
		{
			name: 				"exportComputedEffectSizeOeRatioColumnName"
			text: 				qsTr("Column name (effect size)")
			fieldWidth: 		150 * preferencesModel.uiScale
			placeholderText: 	qsTr("O/E ratios")
			visible:			measure == "oeRatio"
			enabled:			measure == "oeRatio" && exportColumns.checked
		}

		ComputedColumnField
		{
			name: 				"exportComputedEffectSizeOeRatioLCiColumnName"
			text: 				qsTr("Column name (lCI)")
			fieldWidth: 		150 * preferencesModel.uiScale
			placeholderText:	qsTr("O/E ratios (lCI)")
			visible:			measure == "oeRatio"
			enabled:			measure == "oeRatio" && exportColumns.checked
		}

		ComputedColumnField
		{
			name: 				"exportComputedEffectSizeOeRatioUCiColumnName"
			text: 				qsTr("Column name (uCI)")
			fieldWidth: 		150 * preferencesModel.uiScale
			placeholderText:	qsTr("O/E ratios (uCI)")
			visible:			measure == "oeRatio"
			enabled:			measure == "oeRatio" && exportColumns.checked
		}

		ComputedColumnField
		{
			name: 				"exportComputedEffectSizeCStatisticColumnName"
			id:					newColumnName
			text: 				qsTr("Column name (effect size)")
			fieldWidth: 		150 * preferencesModel.uiScale
			placeholderText:	qsTr("C-statistics")
			visible:			measure == "cStatistic"
			enabled:			measure == "cStatistic" && exportColumns.checked
		}

		ComputedColumnField
		{
			name: 				"exportComputedEffectSizeCStatisticLCiColumnName"
			text: 				qsTr("Column name (lCI)")
			fieldWidth: 		150 * preferencesModel.uiScale
			placeholderText:	qsTr("C-statistics (lCI)")
			visible:			measure == "cStatistic"
			enabled:			measure == "cStatistic" && exportColumns.checked
		}

		ComputedColumnField
		{
			name: 				"exportComputedEffectSizeCStatisticUCiColumnName"
			text: 				qsTr("Column name (uCI)")
			fieldWidth: 		150 * preferencesModel.uiScale
			placeholderText: 	qsTr("C-statistics (uCI)")
			visible:			measure == "cStatistic"
			enabled:			measure == "cStatistic" && exportColumns.checked
		}
	}

	CheckBox
	{
		name:	"funnelPlotAsymmetryTest"
		label:	qsTr("Funnel plot asymmetry test")

		CheckBox
		{
			name:	"funnelPlotAsymmetryTestEggerUnweighted"
			id:		funnelPlotAsymmetryTestEggerUW
			label:	qsTr("Egger (unweighted)")
			checked: true
		}

		CheckBox
		{
			name:	"funnelPlotAsymmetryTestEggerMultiplicativeOverdispersion"
			id:		funnelPlotAsymmetryTestEggerFIV
			label:	qsTr("Egger (multiplicative overdispersion)")
			checked: false
		}

		CheckBox
		{
			name:	"funnelPlotAsymmetryTestMacaskill"
			id:		funnelPlotAsymmetryTestMacaskillFIV
			label:	qsTr("Macaskill")
			checked: false
			enabled: inputN === 1
		}

		CheckBox
		{
			name:	"funnelPlotAsymmetryTestMacaskillPooled"
			id:		funnelPlotAsymmetryTestMacaskillFPV
			label:	qsTr("Macaskill (pooled)")
			checked: false
			enabled: inputN === 1 && inputO === 1
		}

		CheckBox
		{
			name:	"funnelPlotAsymmetryTestPeters"
			id:		funnelPlotAsymmetryTestPeters
			label:	qsTr("Peters")
			checked: false
			enabled: inputN === 1 && inputO === 1
		}

		CheckBox
		{
			name:	"funnelPlotAsymmetryTestDebray"
			id:		funnelPlotAsymmetryTestDebrayFIV
			label:	qsTr("Debray")
			checked: false
			enabled: inputO === 1
		}

		/* Not implemented
		CheckBox
		{
			name:	"funnelPlotAsymmetryTestDebrayFAV"
			label:	qsTr("Debray (FAV)")
			checked: false
		}
		*/

		CheckBox
		{
			name:	"funnelPlotAsymmetryTestPlot"
			label:	qsTr("Plot")
			enabled: funnelPlotAsymmetryTestEggerUW.checked || funnelPlotAsymmetryTestEggerFIV.checked || funnelPlotAsymmetryTestMacaskillFIV.checked || funnelPlotAsymmetryTestMacaskillFPV.checked || funnelPlotAsymmetryTestPeters.checked || funnelPlotAsymmetryTestDebrayFIV.checked
			checked: false
		}
	}
}
