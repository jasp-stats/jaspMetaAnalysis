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
			allowedColumns: ["nominal","nominalText"]
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
			value:		"logOr"
			id:			measureslogOr
			enabled:	mainWindow.dataAvailable
		}

		RadioButton
		{
			label:		qsTr("Unstandardized effect sizes")
			value:		"unstandardizedEffectSizes"
			id:			measuresGeneral
			enabled:	mainWindow.dataAvailable
		}

		RadioButton
		{
			label:		qsTr("Fitted model")
			value:		"fittedModel"
			id:			measuresFitted
			checked:	!mainWindow.dataAvailable
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
				enabled:	!measuresFitted.checked && !measuresGeneral.checked && !modelType.value == "custom"

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
	Section
	{
		title: qsTr("Inference")

		Group
		{

			CheckBox
			{
				label:		qsTr("Conditional parameter estimates")
				name:		"inferenceConditionalParameterEstimates"
			}

			CheckBox
			{
				columns:	2
				label:		qsTr("Models overview")
				name:		"inferenceModelsOverview"

				RadioButtonGroup
				{
					name: "inferenceModelsOverviewBfComparison"
					title: qsTr("BF comparison")

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
						enabled:	inferenceModelsOverviewOrderMarglik.checked
					}
				}

				RadioButtonGroup
				{
					name: 		"inferenceModelsOverviewOrder"
					title:		qsTr("Order")

					RadioButton
					{
						name: 		"modelNumber"
						label: 		qsTr("Model number")
						checked:	true
					}

					RadioButton
					{
						name: 		"marginalLikelihood"
						label: 		qsTr("Marginal likelihood")
						id:			inferenceModelsOverviewOrderMarglik
					}

					RadioButton
					{
						name: 		"posteriorProbability"
						label: 		qsTr("Posterior probability")

					}
				}
			}

			CheckBox
			{
				label:		qsTr("Individual models")
				name:		"inferenceIndividualModels"

				CheckBox
				{
					label:		qsTr("Single model")
					name:		"inferenceIndividualModelsSingleModel"
					childrenOnSameRow: true
					IntegerField
					{
						name:	"inferenceIndividualModelsSingleModelNumber"
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
				name: "inferenceCiWidth"
				label: qsTr("CI width")
			}

			DropDown
			{
				name:		"inferenceOutputScale"
				id:			inferenceOutputScale
				label:		qsTr("Output scale")
				visible:	!measuresGeneral.checked
				values: [
					{ label: qsTr("Cohen's d"),			value: "cohensD"},
					{ label: qsTr("Fisher's z"),		value: "fishersZ"},
					{ label: qsTr("logOR"),				value: "logOr"},
					{ label: qsTr("Correlation"),		value: "r"}
				]
			}

			CheckBox
			{
				label:		qsTr("Shorten prior names")
				name:		"inferenceShortenPriorName"
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
			name:		"plotsForestPlot"

			RadioButtonGroup
			{
				name: 		"plotsForestPlotOrder"
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
					label: 	qsTr("Alphabetical")
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
				name:				"plotsForestPlotType"
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
				name:	"plotsPooledEstimatesEffect"
			}

			CheckBox
			{
				label:	qsTr("Heterogeneity")
				name:	"plotsPooledEstimatesHeterogeneity"
			}

			CheckBox
			{
				label:	qsTr("Weight function")
				name:	"plotsPooledEstimatesWeightFunction"

				CheckBox
				{
					name:		"plotsPooledEstimatesWeightFunctionRescaleXAxis"
					text:		qsTr("Rescale x-axis")
					checked:	true
				}
			}

			CheckBox
			{
				label:	qsTr("PET-PEESE")
				name:	"plotsPooledEstimatesPetPeese"
			}
		}

		Group
		{
			title:		" " // Add a line to align with the first column
			columns:	1

			RadioButtonGroup
			{
				name:		"plotsPooledEstimatesType"
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
				label:		qsTr("Prior distribution")
				name:		"plotsPooledEstimatesPriorDistribution"
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
				name:	"plotsIndividualModelsEffect"
			}

			CheckBox
			{
				label:	qsTr("Heterogeneity")
				name:	"plotsIndividualModelsHeterogeneity"
			}
		}

		Group
		{
			title:		" "
			columns:	2

			RadioButtonGroup
			{
				name:				"plotsIndividualModelsType"
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
				name: 		"plotsIndividualModelsOrderelsOrderBy"
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
				name: 		"plotsIndividualModelsOrderBy"
				title:		qsTr("Order by")

				RadioButton
				{
					name:		"modelNumber"
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
					name:		"bayesFactor"
					label:		qsTr("Bayes factor")
				}

				RadioButton
				{
					name:		"posteriorProbability"
					label:		qsTr("Posterior prob.")
				}
			}

			Group
			{
				title:				qsTr("Show")
				Layout.columnSpan:	2

				CheckBox
				{
					label:		qsTr("Bayesian updating")
					name:		"plotsIndividualModelsShowBayesianUpdating"
					checked:	true
				}

				CheckBox
				{
					label:		qsTr("Posterior estimates")
					name:		"plotsIndividualModelsShowPosteriorEstimates"
					checked:	false
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
			label:		qsTr("Overview table")
			name:		"mcmcDiagnosticsOverviewTable"
		}

		Group
		{
			title:			qsTr("Plot")
			CheckBox
			{
				label:		qsTr("Effect")
				name:		"mcmcDiagnosticsPlotEffect"
			}

			CheckBox
			{
				label:		qsTr("Heterogeneity")
				name:		"mcmcDiagnosticsPlotHeterogeneity"
			}

			CheckBox
			{
				label:		qsTr("Weights")
				name:		"mcmcDiagnosticsPlotWeights"
			}

			CheckBox
			{
				label:		qsTr("PET")
				name:		"mcmcDiagnosticsPlotPet"
			}

			CheckBox
			{
				label:		qsTr("PEESE")
				name:		"mcmcDiagnosticsPlotPeese"
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
					name:		"mcmcDiagnosticsPlotTypeTrace"
				}

				CheckBox
				{
					label:		qsTr("Autocorrelation")
					name:		"mcmcDiagnosticsPlotTypeAutocorrelation"
				}

				CheckBox
				{
					label:		qsTr("Posterior samples density")
					name:		"mcmcDiagnosticsPlotTypePosteriorSamplesDensity"
				}
			}

			CheckBox
			{
				label:		qsTr("Single model")
				name:		"mcmcDiagnosticsPlotSingleModel"
				childrenOnSameRow: true

				IntegerField
				{
					name:			"mcmcDiagnosticsPlotSingleModelNumber"
					defaultValue:	1
				}
			}
		}

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
				name:		"advancedEstimationScale"
				label:		qsTr("Estimation scale")
				visible:	!measuresGeneral.checked
				values: [
					{ label: qsTr("Fisher's z"),		value: "fishersZ"},
					{ label: qsTr("Cohen's d"),			value: "cohensD"},
					{ label: qsTr("logOR"),				value: "logOr"}
				]
			}

			Group
			{
				title: 		qsTr("MCMC")

				IntegerField
				{
					name:			"advancedMcmcAdaptation"
					label:			qsTr("Adaptation")
					defaultValue:	500
					min:			100
					fieldWidth:		55 * preferencesModel.uiScale
				}
				IntegerField
				{
					name:			"advancedMcmcBurnin"
					label:			qsTr("Burnin")
					defaultValue:	2000
					min:			100
					fieldWidth:		55 * preferencesModel.uiScale
				}
				IntegerField
				{
					name:			"advancedMcmcSamples"
					label:			qsTr("Samples")
					defaultValue:	5000
					min:			100
					fieldWidth:		55 * preferencesModel.uiScale
				}
				IntegerField
				{
					name:			"advancedMcmcChains"
					label:			qsTr("Chains")
					defaultValue:	3
					min:			1
					fieldWidth:		55 * preferencesModel.uiScale
				}
				IntegerField
				{
					name:			"advancedMcmcThin"
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
					name:				"advancedAutofitRHat"
					checked:			true
					childrenOnSameRow:	true

					DoubleField
					{
						name:			"advancedAutofitRHatTarget"
						defaultValue:	1.05
						min:			1
						inclusive:		JASP.None
					}
				}

				CheckBox
				{
					label: 				qsTr("Effective sample size")
					name:				"advancedAutofitEss"
					checked:			true
					childrenOnSameRow:	true

					DoubleField
					{
						name:			"advancedAutofitEssTarget"
						defaultValue:	500
						min:			1
						inclusive:		JASP.None
					}
				}

				CheckBox
				{
					label: 				qsTr("MCMC error")
					name:				"advancedAutofitMcmcError"
					checked:			false
					childrenOnSameRow:	true

					DoubleField
					{
						name:			"advancedAutofitMcmcErrorTarget"
						defaultValue:	0.001
						min:			0
						inclusive:		JASP.None
					}
				}

				CheckBox
				{
					label: 				qsTr("MCMC error / SD")
					name:				"advancedAutofitMcmcErrorSd"
					checked:			false
					childrenOnSameRow:	true

					DoubleField
					{
						name:			"advancedAutofitMcmcErrorSdTarget"
						defaultValue:	0.01
						min:			0
						inclusive:		JASP.None
					}
				}

				CheckBox
				{
					label: 				qsTr("Maximum fitting time")
					name:				"advancedAutofitMaximumFittingTime"
					checked:			false
					childrenOnSameRow:	true

					Group
					{
						Row
						{
							IntegerField
							{
								name:			"advancedAutofitMaximumFittingTimeTarget"
								defaultValue:	1
								min:			0
							}

							DropDown
							{
								name:			"advancedAutofitMaximumFittingTimeTargetUnit"
								values:
								[
									{ label: qsTr("hours"),				value: "hours"},
									{ label: qsTr("minutes"),			value: "mins"},
									{ label: qsTr("seconds"),			value: "secs"}
								]
							}
						}
					}
				}

				IntegerField
				{
					label: 			qsTr("Extend samples")
					name:			"advancedAutofitExtendSamples"
					defaultValue:	1000
					min:			100
				}
			}

			CheckBox
			{
				label: 				qsTr("Remove failed models")
				name:				"advancedAutofitRemoveFailedModels"
				checked:			false
			}

			CheckBox
			{
				label: 				qsTr("Rebalance component probability on model failure")
				name:				"advancedAutofitRebalanceComponentProbabilityOnModelFailure"
				checked:			true
			}

		}

		FileSelector
		{
			Layout.columnSpan:	2
			label: 				qsTr("Save the fitted model")
			name:				"advancedSaveFittedModel"
			filter:				"*.RDS"
			save:				true
		}
	}

}
