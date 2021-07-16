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
		"inputSE":	{ values: ["cohensD", "general", "logOR"],	id: inputSE},
		"inputCI":	{ values: ["cohensD", "general", "logOR"],	id: inputCI},
		"inputN" :	{ values: ["cohensD", "correlation"],		id: inputN}
	}

	function checkListVisibility(name)
	{
		var check = (listVisibility[name]["check2Sample"] ? cohensDTwoSample.checked : true)

		return check && listVisibility[name]["values"].includes(measures.value);
	}

	RadioButtonGroup
	{
		id:						measures
		Layout.columnSpan:		2
		name:					"measures"
		radioButtonsOnSameRow:	true
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
			enabled:	mainWindow.dataAvailable
			checked:	mainWindow.dataAvailable
		}

		RadioButton
		{
			label:		qsTr("Correlations")
			value:		"correlation"
			id:			measuresCorrelation
			enabled:	mainWindow.dataAvailable
		}

		RadioButton
		{
			label:		qsTr("Log odds ratios")
			value:		"logOR"
			id:			measureslogOR
			enabled:	mainWindow.dataAvailable
		}

		RadioButton
		{
			label:		qsTr("Unstandardized effect sizes")
			value:		"general"
			id:			measuresGeneral
			enabled:	mainWindow.dataAvailable
		}

		RadioButton
		{
			label:		qsTr("Fitted model")
			value:		"fitted"
			id:			measuresFitted
			checked:	!mainWindow.dataAvailable
		}
	}

	FileSelector
	{
		Layout.columnSpan:	2
		name:				"fittedPath"
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
			name: 			"inputES"
			enabled: 		inputT.count == 0
			title: 			
			{
			if (measuresCohensD.checked)
				qsTr("Cohen's d")
			else if (measuresCorrelation.checked)
				qsTr("Correlation")
			else if (measureslogOR.checked)
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
			name: 			"inputSE"
			title: 			qsTr("Effect Size Standard Error")
			singleVariable: true
			allowedColumns: ["scale"]
			visible:		checkListVisibility(name)
		}

		AssignedPairsVariablesList
		{
			id: 			inputCI
			enabled: 		inputSE.count == 0 && inputN.count == 0 && inputN.count == 0 
			name: 			"inputCI"
			title: 			qsTr("95% CI Lower and Upper Bound")
			singleVariable: true
			allowedColumns: ["scale"]
			visible:		checkListVisibility(name)
		}

		AssignedVariablesList
		{
			id: 			inputN
			enabled: 		inputSE.count == 0 && inputCI.count == 0 && inputN.count == 0
			name: 			"inputN"
			title: 			qsTr("N")
			singleVariable: true
			allowedColumns: ["scale", "ordinal"]
			visible:		checkListVisibility(name)
		}

		AssignedVariablesList
		{
			name: 			"inputLabels"
			title: 			qsTr("Study Labels")
			singleVariable:	true
			allowedColumns: ["nominal","nominalText"]
		}
	}

	RadioButtonGroup
	{
		name:		"effectDirection"
		title:		qsTr("Expected effect size direction")
		columns:	1
		visible:	!measuresFitted.checked

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
			name:		"modelType"
			label:		qsTr("Model type")
			visible:	!measuresFitted.checked && !measuresGeneral.checked
			values: [
				{ label: qsTr("RoBMA-PSMA"),		value: "PSMA"},
				{ label: qsTr("RoBMA-PP"),			value: "PP"},
				{ label: qsTr("RoBMA-old"),			value: "2w"},
				{ label: qsTr("Custom"),			value: "custom"}
			]
		}

		DropDown
		{
			id:			priorScale
			name:		"priorScale"
			label:		qsTr("Prior scale")
			visible:	!measuresFitted.checked && !measuresGeneral.checked
			enabled:	modelType.value == "custom"
			values: [
				{ label: qsTr("Cohen's d"),			value: "cohens_d"},
				{ label: qsTr("Fisher's z"),		value: "fishers_z"},
				{ label: qsTr("log(OR)"),			value: "logOR"}
			]
			onCurrentValueChanged: 			
			{
				if(priorScale.currentValue == "cohens_d")
					resultsScale.currentValue = "cohens_d"
				else if(priorScale.currentValue == "fishers_z")
					resultsScale.currentValue = "fishers_z"
				else if(priorScale.currentValue == "logOR")
					resultsScale.currentValue = "logOR"
			}
		}

		CheckBox
		{
			name:		"plotPriors"
			label:		qsTr("Plot priors")
		}
	}


	//// Priors ////
	Section
	{
		title: 				qsTr("Models")
		columns:			1
		enabled:			modelType.value == "custom"
		onEnabledChanged:	if(!enabled) expanded = false


		// effect prior
		MA.RobustBayesianMetaAnalysisPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"effect"
		}

		// effect prior
		MA.RobustBayesianMetaAnalysisPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"heterogeneity"
		}

		// bias priors
		MA.RobustBayesianMetaAnalysisWeightfunctions
		{
			Layout.preferredWidth:	parent.width
			componentType:			"omega"
		}

		MA.RobustBayesianMetaAnalysisPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"pet"
		}

		MA.RobustBayesianMetaAnalysisPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"peese"
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
			componentType:			"effectNull"
			visible:				priorsNull.checked
		}

		// effect prior
		MA.RobustBayesianMetaAnalysisPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"heterogeneityNull"
			visible:				priorsNull.checked
		}

		// bias priors
		MA.RobustBayesianMetaAnalysisWeightfunctions
		{
			Layout.preferredWidth:	parent.width
			componentType:			"omegaNull"
			visible:				priorsNull.checked
		}

		MA.RobustBayesianMetaAnalysisPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"petNull"
			visible:				priorsNull.checked
		}

		MA.RobustBayesianMetaAnalysisPriors
		{
			Layout.preferredWidth:	parent.width
			componentType:			"peeseNull"
			visible:				priorsNull.checked
		}
	}


	//// Inference ////
	Section
	{
		title: qsTr("Inference")

		Group
		{

			CheckBox
			{
				label:		qsTr("Conditional estimates")
				name:		"resultsConditional"
			}

			CheckBox
			{
				columns:	2
				label:		qsTr("Models overview")
				name:		"resultsModels"

				RadioButtonGroup
				{
					name: "resultsModelsBf"
					title: qsTr("BF")

					RadioButton
					{
						name: 		"inclusion"
						label: 		qsTr("Inclusion")
						checked: 	true
					}

					RadioButton
					{
						name: 		"best"
						label: 		qsTr("vs. Best")
					}

					RadioButton
					{
						name: 		"previous"
						label: 		qsTr("vs. Previous")
						enabled:	resultsModelsOrderMarglik.checked
					}
				}

				RadioButtonGroup
				{
					name: 		"resultsModelsOrder"
					title:		qsTr("Order")

					RadioButton
					{
						name: 		"default"
						label: 		qsTr("Model number")
						checked:	true
					}

					RadioButton
					{
						name: 		"marglik"
						label: 		qsTr("Bayes factor")
						id:			resultsModelsOrderMarglik
					}

					RadioButton
					{
						name: 		"posterior"
						label: 		qsTr("Posterior prob.")

					}
				}
			}

			CheckBox
			{
				label:		qsTr("Individual models")
				name:		"resultsIndividual"

				CheckBox
				{
					label:		qsTr("Single model")
					name:		"resultsIndividualSingle"
					childrenOnSameRow: true
					IntegerField
					{
						name:	"resultsIndividualSingleNumber"
						defaultValue:	1
					}
				}
			}

		}

		Group
		{

			BayesFactorType{}

			CIField
			{
				name: "resultsCi"
				label: qsTr("CI width")
			}

			DropDown
			{
				name:		"resultsScale"
				id:			resultsScale
				label:		qsTr("Output scale")
				visible:	!measuresGeneral.checked
				values: [
					{ label: qsTr("Cohen's d"),			value: "cohens_d"},
					{ label: qsTr("Fisher's z"),		value: "fishers_z"},
					{ label: qsTr("log(OR)"),			value: "logOR"},
					{ label: qsTr("Correlation"),		value: "r"}
				]
			}

			CheckBox
			{
				label:		qsTr("Shorten prior names")
				name:		"shortNames"
			}

		}
	}

	//// Plots section ////
	Section
	{
		title: 		qsTr("Plots")


		CheckBox
		{
			columns:	2
			label:		qsTr("Forest plot")
			name:		"plotForest"

			RadioButtonGroup
			{
				name: 		"plotForestOrder"
				title:		qsTr("Order")

				RadioButton
				{
					name: 	"increasing"
					label: 	qsTr("Ascending")
				}

				RadioButton
				{
					name: 	"decreasing"
					label: 	qsTr("Descending")
				}

				RadioButton
				{
					name: 	"alphabetical"
					label: 	qsTr("Row order")
					checked:true
				}
			}
		}

		Group
		{
			title:		" " // Add a line to align with the first column
			columns:	1

			RadioButtonGroup
			{
				name:				"plotForestType"
				title:				qsTr("Type")
				columns:			2

				RadioButton
				{
					value:		"averaged"
					label:		qsTr("Model averaged")
					checked:	true
				}

				RadioButton
				{
					value:		"conditional"
					label:		qsTr("Conditional")
				}

			}
		}

		Divider { }

		Group
		{
			title:	qsTr("Pooled estimates")
			columns: 1

			CheckBox
			{
				label:	qsTr("Effect")
				name:	"plotEstimatesMu"
			}

			CheckBox
			{
				label:	qsTr("Heterogeneity")
				name:	"plotEstimatesTau"
			}

			CheckBox
			{
				label:	qsTr("Weight function")
				name:	"plotEstimatesWeightFunction"

				CheckBox
				{
					name:		"plotEstimatesWeightFunctionRescale"
					text:		qsTr("Rescale x-axis")
					checked:	true
				}
			}

			CheckBox
			{
				label:	qsTr("PET-PEESE")
				name:	"plotEstimatesPetPeese"
			}
		}

		Group
		{
			title:		" " // Add a line to align with the first column
			columns:	1

			RadioButtonGroup
			{
				name:		"plotEstimatesType"
				title:		qsTr("Type")
				columns:	2

				RadioButton
				{
					value:		"averaged"
					label:		qsTr("Model averaged")
					checked:	true
				}

				RadioButton
				{
					value:		"conditional"
					label:		qsTr("Conditional")
				}

			}

			CheckBox
			{
				label:		qsTr("Show priors")
				name:		"plotEstimatesPriors"
				checked:	true
			}
		}

		Divider { }

		Group
		{
			title:	qsTr("Individual models")
			columns: 1

			CheckBox
			{
				label:	qsTr("Effect")
				name:	"plotModelsMu"
			}

			CheckBox
			{
				label:	qsTr("Heterogeneity")
				name:	"plotModelsTau"
			}
		}

		Group
		{
			title:		" "
			columns:	2

			RadioButtonGroup
			{
				name:				"plotModelsType"
				title:				qsTr("Type")
				Layout.columnSpan:	2
				columns:			2

				RadioButton
				{
					value:		"averaged"
					label:		qsTr("Model averaged")
				}

				RadioButton
				{
					value:		"conditional"
					label:		qsTr("Conditional")
					checked:	true
				}
			}

			RadioButtonGroup
			{
				name: 		"plotModelsOrder"
				title:		qsTr("Order")

				RadioButton
				{
					name: 	"increasing"
					label: 	qsTr("Ascending")
				}

				RadioButton
				{
					name: 	"decreasing"
					label: 	qsTr("Descending")
				}
			}

			RadioButtonGroup
			{
				name: 		"plotModelsOrderBy"
				title:		qsTr("Order by")

				RadioButton
				{
					name:		"model"
					label:		qsTr("Model number")
					checked:	true
				}

				RadioButton
				{
					name:		"estimate"
					label:		qsTr("Estimate")
				}

				RadioButton
				{
					name:		"BF"
					label:		qsTr("Bayes factor")
				}

				RadioButton
				{
					name:		"probability"
					label:		qsTr("Posterior prob.")
				}
			}
		}
	}

	//// Diagnostics section ////
	Section
	{
		title: qsTr("MCMC Diagnostics")

		CheckBox
		{
			Layout.columnSpan: 2
			label:		qsTr("Overview")
			name:		"diagnosticsOverview"

				CheckBox
				{
					label:		qsTr("Include theta")
					name:		"diagnosticsOverviewTheta"
					visible:	false
				}
		}

		Group
		{
			title:			qsTr("Plot")
			CheckBox
			{
				label:		qsTr("Effect")
				name:		"diagnosticsMu"
			}

			CheckBox
			{
				label:		qsTr("Heterogeneity")
				name:		"diagnosticsTau"
			}

			CheckBox
			{
				label:		qsTr("Weights")
				name:		"diagnosticsOmega"
			}

			CheckBox
			{
				label:		qsTr("PET")
				name:		"diagnosticsPet"
			}

			CheckBox
			{
				label:		qsTr("PEESE")
				name:		"diagnosticsPeese"
			}
		}

		Group
		{
			Group
			{
				title:			qsTr("Type")
				CheckBox
				{
					label:		qsTr("Trace")
					name:		"diagnosticsTrace"
				}

				CheckBox
				{
					label:		qsTr("Autocorrelation")
					name:		"diagnosticsAutocorrelation"
				}

				CheckBox
				{
					label:		qsTr("Posterior samples densities")
					name:		"diagnosticsSamples"
				}
			}

			CheckBox
			{
				label:		qsTr("Single model")
				name:		"diagnosticsSingle"
				childrenOnSameRow: true

				IntegerField
				{
					name:			"diagnosticsSingleModel"
					defaultValue:	1
				}
			}
		}

	}

	//// Advanced section for prior model probabilities sampling settings ////
	Section
	{
		title: 				qsTr("Advanced")
		columns: 			2
		enabled:			!measuresFitted.checked
		onEnabledChanged:	if(!enabled) expanded = false
		
		Group
		{
			rowSpacing: 10 * preferencesModel.uiScale

			DropDown
			{
				name:		"fittingScale"
				label:		qsTr("Estimation scale")
				visible:	!measuresGeneral.checked
				values: [
					{ label: qsTr("Fisher's z"),		value: "fishers_z"},
					{ label: qsTr("Cohen's d"),			value: "cohens_d"},
					{ label: qsTr("log(OR)"),			value: "logOR"}
				]
			}

			Group
			{
				title: 		qsTr("Estimation settings (MCMC)")

				IntegerField
				{
					name:			"advancedAdapt"
					label:			qsTr("Adaptation")
					defaultValue:	500
					min:			100
					fieldWidth:		55 * preferencesModel.uiScale
				}
				IntegerField
				{
					name:			"advancedBurnin"
					label:			qsTr("Burnin")
					defaultValue:	2000
					min:			100
					fieldWidth:		55 * preferencesModel.uiScale
				}
				IntegerField
				{
					name:			"advancedIteration"
					label:			qsTr("Iterations")
					defaultValue:	5000
					min:			100
					fieldWidth:		55 * preferencesModel.uiScale
				}
				IntegerField
				{
					name:			"advancedChains"
					label:			qsTr("Chains")
					defaultValue:	3
					min:			1
					fieldWidth:		55 * preferencesModel.uiScale
				}
				IntegerField
				{
					name:			"advancedThin"
					label:			qsTr("Thin")
					defaultValue:	1
					min:			1
					fieldWidth:		55 * preferencesModel.uiScale
				}

			}

			SetSeed{}
		}

		
		Group
		{

			CheckBox
			{
				label:			qsTr("Autofit")
				name:			"autofit"
				checked:		true

				CheckBox
				{
					label: 				qsTr("R-hat")
					name:				"autofitRhat"
					checked:			true
					childrenOnSameRow:	true

					DoubleField
					{
						name:			"autofitRhatValue"
						defaultValue:	1.05
						min:			1
						inclusive:		JASP.None
					}
				}

				CheckBox
				{
					label: 				qsTr("Effective sample size")
					name:				"autofitEss"
					checked:			true
					childrenOnSameRow:	true

					DoubleField
					{
						name:			"autofitEssValue"
						defaultValue:	500
						min:			1
						inclusive:		JASP.None
					}
				}

				CheckBox
				{
					label: 				qsTr("MCMC error")
					name:				"autofitMcmcError"
					checked:			false
					childrenOnSameRow:	true

					DoubleField
					{
						name:			"autofitMcmcErrorValue"
						defaultValue:	0.001
						min:			0
						inclusive:		JASP.None
					}
				}

				CheckBox
				{
					label: 				qsTr("MCMC error / SD")
					name:				"autofitMcmcErrorSd"
					checked:			false
					childrenOnSameRow:	true

					DoubleField
					{
						name:			"autofitMcmcErrorSdValue"
						defaultValue:	0.01
						min:			0
						inclusive:		JASP.None
					}
				}

				CheckBox
				{
					label: 				qsTr("Maximum fitting time")
					name:				"autofitTime"
					checked:			false
					childrenOnSameRow:	true

					Group
					{
						Row
						{
							IntegerField
							{
								name:			"autofitTimeValue"
								defaultValue:	1
								min:			0
							}

							DropDown
							{
								name:			"autofitTimeUnit"
								values:
								[
									{ label: qsTr("hours"),				value: "hours"},
									{ label: qsTr("minutes"),			value: "minutes"},
									{ label: qsTr("seconds"),			value: "seconds"}
								]
							}
						}
					}
				}

				IntegerField
				{
					label: 			qsTr("Extend samples")
					name:			"autofitExtendSamples"
					defaultValue:	1000
					min:			100
				}
			}

			CheckBox
			{
				label: 				qsTr("Remove failed models")
				name:				"removeFailed"
				checked:			false
			}

			CheckBox
			{
				label: 				qsTr("Rebalance component probability on model failure")
				name:				"balanceProbability"
				checked:			true
			}

		}

		FileSelector
		{
			Layout.columnSpan:	2
			label: 				qsTr("Save the fitted model")
			name:				"savePath"
			filter:				"*.RDS"
			save:				true
		}
	}

}
