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
	property string analysisType: "RoBMA"
	// RoBMA: Robust Bayesian Meta-Analsis
	// BiBMA: Binomial Bayesian Meta-Analysis
	// NoBMA: Normal Bayesian Meta-Analysis
	
	property alias enableStudyLevelNesting:			enableStudyLevelNesting

	title: 				qsTr("Advanced")
	columns: 			2

	
	Group
	{

		Group
		{
			CheckBox
			{
				name:		"showRoBMARCode"
				text:		qsTr("Show RoBMA R code")
				info: qsTr("Display the underlying R code used by the RoBMA package to fit the model.")
			}

			CheckBox
			{
				name:		"includeFullDatasetInSubgroupAnalysis"
				text:		qsTr("Include full dataset in subgroup analysis")
				enabled:	subgroup.count == 1
				checked:	false
				info: qsTr("Include the full dataset output in the subgroup analysis. This option is only available when the subgroup analysis is selected.")
			}

			CheckBox
			{
				label:		qsTr("Shorten prior names")
				name:		"shortenPriorName"
				info: qsTr("Shorten the prior names in the output.")
			}

			CheckBox
			{
				label:		qsTr("Enable study-level nesting")
				name:		"enableStudyLevelNesting"
				id:			enableStudyLevelNesting
				visible:	analysisType === "RoBMA"
				info: qsTr("Enables study level nesting. Note that this is an experimental feature.")
			}
		}
	}

	Group
	{
		Group
		{
			title: 		qsTr("MCMC")

			IntegerField
			{
				name:			"advancedMcmcAdaptation"
				label:			qsTr("Adaptation")
				defaultValue:	2000 + 1000 * predictors.count
				min:			100
				fieldWidth:		55 * preferencesModel.uiScale
			}
			IntegerField
			{
				name:			"advancedMcmcBurnin"
				label:			qsTr("Burnin")
				defaultValue:	2000 + 2000 * predictors.count
				min:			100
				fieldWidth:		55 * preferencesModel.uiScale
			}
			IntegerField
			{
				name:			"advancedMcmcSamples"
				label:			qsTr("Samples")
				defaultValue:	5000 + 2500 * predictors.count
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
			checked:		false

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
				defaultValue:	5000
				min:			100
			}
		}

	}

/* TODO: one needs to figure out how to add moderator names to the option list when importing a model 
	Group
	{	
		FileSelector
		{
			name:				"pathToFittedModel"
			label:  			qsTr("Load a fitted model")
			filter:				"*.RDS"
			save:				false
			visible:			analysisType === "RoBMA"
			info:				qsTr("Load a fitted model from a file. This will allow you to load the fitted model in a later session. Note that this will overwrite the current fitted model and all of the model fitting options (Model, Priors, etc.).")
		}

		FileSelector
		{

			label: 				qsTr("Save the fitted model")
			name:				"advancedSaveFittedModel"
			filter:				"*.RDS"
			save:				true
			visible:			analysisType === "RoBMA"
			info:				qsTr("Save the fitted model to a file. This will allow you to load the fitted model in a later session.")
		}
	}
*/

}
