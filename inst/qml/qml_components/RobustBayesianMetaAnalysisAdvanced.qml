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

	property bool measuresGeneralChecked:	false
	property bool measuresFittedChecked:		false

	title: 				qsTr("Advanced")
	columns: 			2
	enabled:			!measuresFittedChecked
	onEnabledChanged:	if(!enabled) expanded = false
	
	Group
	{
		rowSpacing: 10 * preferencesModel.uiScale

		DropDown
		{
			name:		"advancedEstimationScale"
			label:		qsTr("Estimation scale")
			visible:	!measuresGeneralChecked && analysisType === "RoBMA"
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
			name:				"advancedRemoveFailedModels"
			checked:			false

			CheckBox
			{
				label: 				qsTr("R-hat")
				name:				"advancedRemoveFailedModelsRHat"
				checked:			true
				childrenOnSameRow:	true

				DoubleField
				{
					name:			"advancedRemoveFailedModelsRHatTarget"
					defaultValue:	1.05
					min:			1
					inclusive:		JASP.None
				}
			}

			CheckBox
			{
				label: 				qsTr("Effective sample size")
				name:				"advancedRemoveFailedModelsEss"
				checked:			true
				childrenOnSameRow:	true

				DoubleField
				{
					name:			"advancedRemoveFailedModelsEssTarget"
					defaultValue:	500
					min:			1
					inclusive:		JASP.None
				}
			}

			CheckBox
			{
				label: 				qsTr("MCMC error")
				name:				"advancedRemoveFailedModelsMcmcError"
				checked:			false
				childrenOnSameRow:	true

				DoubleField
				{
					name:			"advancedRemoveFailedModelsMcmcErrorTarget"
					defaultValue:	0.001
					min:			0
					inclusive:		JASP.None
				}
			}

			CheckBox
			{
				label: 				qsTr("MCMC error / SD")
				name:				"advancedRemoveFailedModelsMcmcErrorSd"
				checked:			false
				childrenOnSameRow:	true

				DoubleField
				{
					name:			"advancedRemoveFailedModelsMcmcErrorSdTarget"
					defaultValue:	0.01
					min:			0
					inclusive:		JASP.None
				}
			}
		}

		CheckBox
		{
			label: 				qsTr("Rebalance component probability on model failure")
			name:				"advancedRebalanceComponentProbabilityOnModelFailure"
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
		visible:			analysisType === "RoBMA"
	}
}
