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
import JASP 1.0
import "./qml_components"		as MA

Form
{
	property var listVisibility :
	{
		"effectSizeSe":	{ values: ["cohensD", "unstandardizedEffectSizes", "logOr"],	id: inputSE},
		"effectSizeCi":	{ values: ["cohensD", "unstandardizedEffectSizes", "logOr"],	id: inputCI},
		"sampleSize" :	{ values: ["cohensD", "correlation"],		id: inputN}
	}

	function checkListVisibility(name)
	{
		var check = (listVisibility[name]["check2Sample"] ? cohensDTwoSample.checked : true)

		return check && listVisibility[name]["values"].includes(measures.value);
	}


	FileSelector
	{
		Layout.columnSpan:	2
		name:				"pathToFittedModel"
		label:  			qsTr("Path to the fitted model")
		filter:				"*.RDS"
		save:				false
		visible:			measuresFitted.checked
	}

	VariablesForm
	{
		preferredHeight:	250 * preferencesModel.uiScale
		visible:			!measuresFitted.checked
		Layout.columnSpan:	2

		AvailableVariablesList {name: "variablesList"}

		AssignedVariablesList
		{
			id: 			inputES
			name: 			"effectSize"
			enabled: 		inputT.count == 0
			title: 			
			{
			if (measuresCohensD.checked)
				qsTr("Cohen's d")
			else if (measuresCorrelation.checked)
				qsTr("Correlation")
			else if (measureslogOr.checked)
				qsTr("Log Odds Ratio")
			else
				qsTr("Effect Size")
			}
			singleVariable: true
			allowedColumns: ["scale"]
		}

		AssignedVariablesList
		{
			id: 			inputSE
			enabled: 		inputCI.count == 0 && inputN.count == 0
			name: 			"effectSizeSe"
			title: 			qsTr("Effect Size Standard Error")
			singleVariable: true
			allowedColumns: ["scale"]
			visible:		checkListVisibility(name)
		}

		AssignedPairsVariablesList
		{
			id: 			inputCI
			enabled: 		inputSE.count == 0 && inputN.count == 0 && inputN.count == 0 
			name: 			"effectSizeCi"
			title: 			qsTr("95% CI Lower and Upper Bound")
			singleVariable: true
			allowedColumns: ["scale"]
			visible:		checkListVisibility(name)
		}

		AssignedVariablesList
		{
			id: 			inputN
			enabled: 		inputSE.count == 0 && inputCI.count == 0
			name: 			"sampleSize"
			title: 			qsTr("N")
			singleVariable: true
			allowedColumns: ["scale", "ordinal"]
			visible:		checkListVisibility(name)
		}

		AssignedVariablesList
		{
			name: 			"studyLabel"
			title: 			qsTr("Study Labels")
			singleVariable:	true
			allowedColumns: ["nominal"]
		}
	}

	RadioButtonGroup
	{
		id:						measures
		Layout.columnSpan:		2
		name:					"inputType"
		title:					qsTr("Input type")
		radioButtonsOnSameRow:	false
		columns:				2

		onValueChanged:
		{
			for (var inputName in listVisibility)
			{
				if (!checkListVisibility(inputName) && listVisibility[inputName]["id"].count > 0)
					listVisibility[inputName]["id"].itemDoubleClicked(0)
			}

			if(measuresGeneral.checked)
				modelType.value = "custom"
			else
				modelType.value = "PSMA"
		}

		RadioButton
		{
			label:		qsTr("Cohen's d")
			value:		"cohensD"
			id:			measuresCohensD
			enabled:	dataSetInfo.dataAvailable
			checked:	dataSetInfo.dataAvailable
		}

		RadioButton
		{
			label:		qsTr("Correlations")
			value:		"correlation"
			id:			measuresCorrelation
			enabled:	dataSetInfo.dataAvailable
		}

		RadioButton
		{
			label:		qsTr("Log odds ratios")
			value:		"logOr"
			id:			measureslogOr
			enabled:	dataSetInfo.dataAvailable
		}

		RadioButton
		{
			label:		qsTr("Unstandardized effect sizes")
			value:		"unstandardizedEffectSizes"
			id:			measuresGeneral
			enabled:	dataSetInfo.dataAvailable
		}

		RadioButton
		{
			label:		qsTr("Fitted model")
			value:		"fittedModel"
			id:			measuresFitted
			checked:	!dataSetInfo.dataAvailable
		}
	}
	
	RadioButtonGroup
	{
		name:		"modelExpectedDirectionOfEffectSizes"
		title:		qsTr("Expected direction of effect sizes")
		columns:	1
		enabled:	!measuresFitted.checked

		RadioButton
		{
			value:		"positive"
			label:		qsTr("Positive")
			checked: 	true
		}

		RadioButton
		{
			value:		"negative"
			label:		qsTr("Negative")
		}
	}

	Group
	{
		DropDown
		{
			id:			modelType
			name:		"modelEnsembleType"
			label:		qsTr("Model ensemble type")
			enabled:	!measuresFitted.checked && !measuresGeneral.checked
			values: [
				{ label: qsTr("RoBMA-PSMA"),		value: "PSMA"},
				{ label: qsTr("RoBMA-PP"),			value: "PP"},
				{ label: qsTr("RoBMA-original"),	value: "original"},
				{ label: qsTr("Custom"),			value: "custom"}
			]
		}

		Group
		{
			columns:	2

			DropDown
			{
				id:			priorScale
				name:		"priorScale"
				label:		qsTr("Prior scale")
				enabled:	!measuresFitted.checked && !measuresGeneral.checked && modelType.value == "custom"

				values: [
					{ label: qsTr("Cohen's d"),			value: "cohensD"},
					{ label: qsTr("Fisher's z"),		value: "fishersZ"},
					{ label: qsTr("log(OR)"),			value: "logOr"}
				]
				onCurrentValueChanged: 			
				{
					if(priorScale.currentValue == "cohensD")
						inferenceOutputScale.currentValue = "cohensD"
					else if(priorScale.currentValue == "fishersZ")
						inferenceOutputScale.currentValue = "fishersZ"
					else if(priorScale.currentValue == "logOr")
						inferenceOutputScale.currentValue = "logOr"
				}
			}
			/*
			HelpButton
			{
				toolTip:	qsTr("Prior scale can be changed only when specifying a 'Custom' Model ensemble for standardized effect size measures.")
			}
			*/
		}


		CheckBox
		{
			name:		"priorDistributionPlot"
			label:		qsTr("Prior distribution plots")
		}
	}


	//// Inference ////
	MA.RobustBayesianMetaAnalysisInference
	{
		analysisType:				"RoBMA"
		measuresGeneralChecked: 	measuresGeneral.checked
	}

	//// Plots section ////
	MA.RobustBayesianMetaAnalysisPlots
	{
		analysisType:	"RoBMA"
	}

	//// Diagnostics section ////
	MA.RobustBayesianMetaAnalysisDiagnostics
	{
		analysisType:	"RoBMA"
	}

	//// Priors ////
	Section
	{
		title: 				qsTr("Models (Custom Ensemble Only)")
		columns:			1
		enabled:			modelType.value == "custom"
		onEnabledChanged:	if(!enabled) expanded = false


		// effect prior
		MA.RobustBayesianMetaAnalysisPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"modelsEffect"
			analysisType:			"normal"
		}

		// effect prior
		MA.RobustBayesianMetaAnalysisPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"modelsHeterogeneity"
			analysisType:			"normal"
		}

		// bias priors
		MA.RobustBayesianMetaAnalysisWeightfunctions
		{
			Layout.preferredWidth:	parent.width
			componentType:			"modelsSelectionModels"
		}

		MA.RobustBayesianMetaAnalysisPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"modelsPet"
			analysisType:			"normal"
		}

		MA.RobustBayesianMetaAnalysisPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"modelsPeese"
			analysisType:			"normal"
		}

		Divider { }

		CheckBox
		{
			id:						priorsNull
			name:					"priorsNull"
			label:					qsTr("Set null priors")
		}

		// effect prior
		MA.RobustBayesianMetaAnalysisPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"modelsEffectNull"
			analysisType:			"normal"
			visible:				priorsNull.checked
		}

		// effect prior
		MA.RobustBayesianMetaAnalysisPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"modelsHeterogeneityNull"
			analysisType:			"normal"
			visible:				priorsNull.checked
		}

		// bias priors
		MA.RobustBayesianMetaAnalysisWeightfunctions
		{
			Layout.preferredWidth:	parent.width
			componentType:			"modelsSelectionModelsNull"
			visible:				priorsNull.checked
		}

		MA.RobustBayesianMetaAnalysisPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"modelsPetNull"
			analysisType:			"normal"
			visible:				priorsNull.checked
		}

		MA.RobustBayesianMetaAnalysisPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"modelsPeeseNull"
			analysisType:			"normal"
			visible:				priorsNull.checked
		}
	}

	//// Advanced section for prior model probabilities sampling settings ////
	MA.RobustBayesianMetaAnalysisAdvanced
	{
		analysisType:				"RoBMA"
		measuresGeneralChecked: 	measuresGeneral.checked
		measuresFittedChecked: 		measuresFitted.checked
	}

}
