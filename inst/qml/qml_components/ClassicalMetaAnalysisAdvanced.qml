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
import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0

Section
{
	title:						qsTr("Advanced")
	property string module:		"metaAnalysis"
	columns:					1

	
	property alias permutationTestChecked:		permutationTest.checked

	Group
	{
		columns:	2

		Group
		{

			CheckBox
			{
				name:		"showMetaforRCode"
				text:		qsTr("Show metafor R code")
				Layout.preferredWidth: 300 * jaspTheme.uiScale
			}

			CheckBox
			{
				name:		"weightedEstimation"
				text:		qsTr("Weighted estimation")
				checked:	true
			}

			Group
			{
				title:		qsTr("Clustering")
				enabled:	clustering.count == 1

				CheckBox
				{
					name:		"clusteringUseClubSandwich"
					text:		qsTr("Use clubSandwich")
					checked:	true
				}

				CheckBox
				{
					name:		"clusteringSmallSampleCorrection"
					text:		qsTr("Small sample correction")
					checked:	true
				}
			}

			Group
			{
				visible:	module == "metaAnalysisMultilevelMultivariate"
				title:		qsTr("Random Effects / Modele Structure")

				CheckBox
				{
					name:		"useSparseMatricies"
					text:		qsTr("Use sparse matricies")
					checked:	false
				}

				CheckBox
				{
					name:		"computeCovarianceMatrix"
					text:		qsTr("Compute covariance matrix")
					checked:	true
				}
			}
		}

		Group
		{
			title:	qsTr("Fix Parameters")

			CheckBox
			{	// TODO: allow fixing in multivariate models
				name:				"fixParametersTau2"
				text:				qsTr("𝜏²")
				enabled:			sectionModel.heterogeneityModelTermsCount == 0
				childrenOnSameRow:	true
				visible:			module == "metaAnalysis"

				FormulaField
				{
					label: 				""
					name: 				"fixParametersTau2Value"
					value:				"1"
					min: 				0
					inclusive: 			JASP.None
				}
			}

			CheckBox
			{
				name:	"fixParametersWeights"
				text:	qsTr("Weights")
				childrenOnSameRow:	true

				DropDown
				{
					label: 				""
					name: 				"fixParametersWeightsVariable"
					source:				"allVariables"
				}
			}
		}

		Group
		{
			title:		qsTr("Add Omibus Moderator Test")
			enabled:	sectionModel.effectSizeModelTermsCount > 0 || sectionModel.heterogeneityModelTermsCount > 0

			CheckBox
			{
				text:	qsTr("Effect size coefficients")
				name:	"addOmnibusModeratorTestEffectSizeCoefficients"
				enabled:			sectionModel.effectSizeModelTermsCount > 0
				childrenOnSameRow:	false

				TextField
				{
					label: 				""
					name: 				"addOmnibusModeratorTestEffectSizeCoefficientsValues"
					value:				"c(1, 2)"
				}
			}

			CheckBox
			{
				text:	qsTr("Heterogeneity coefficients")
				name:	"addOmnibusModeratorTestHeterogeneityCoefficients"
				enabled:			sectionModel.heterogeneityModelTermsCount > 0
				childrenOnSameRow:	false
				visible:			module == "metaAnalysis"

				TextField
				{
					label: 				""
					name: 				"addOmnibusModeratorTestHeterogeneityCoefficientsValues"
					value:				"c(1, 2)"
				}
			}
		}

		Group
		{
			title:		qsTr("Optimizer")
			enabled:	method.value == "restrictedML" || method.value == "maximumLikelihood" || method.value == "empiricalBayes" ||
						method.value == "pauleMandel" || method.value == "pauleMandelMu" || method.value == "qeneralizedQStatMu" ||
						method.value == "sidikJonkman"

			DropDown
			{
				name:		"optimizerMethod"
				label:		qsTr("Method") // TODO: switch default value on heterogeneityModelLink change
				values:		{
					if (module == "metaAnalysis") {
						if (sectionModel.heterogeneityModelLinkValue == "log")
							["nlminb", "BFGS", "Nelder-Mead", "uobyqa", "newuoa", "bobyqa", "nloptr", "nlm"]
						else
							["constrOptim", "nlminb", "BFGS", "Nelder-Mead", "uobyqa", "newuoa", "bobyqa", "nloptr", "nlm"]
					} else	if (module == "metaAnalysisMultilevelMultivariate") {
							["nlminb", "BFGS", "Nelder-Mead", "uobyqa", "newuoa", "bobyqa", "nloptr", "nlm", "hjk", "nmk", "mads", "ucminf", "lbfgsb3c", "BBoptim"]
					}

				}
				visible:	module == "metaAnalysisMultilevelMultivariate" || sectionModel.heterogeneityModelTermsCount > 0
			}

			CheckBox
			{
				name:		"optimizerInitialTau2"
				text:		qsTr("Initial 𝜏²")
				checked:	false
				childrenOnSameRow:	true
				visible:	(method.value == "restrictedML" || method.value == "maximumLikelihood" || method.value == "empiricalBayes" ||
							method.value == "sidikJonkman") && sectionModel.heterogeneityModelTermsCount == 0 && module == "metaAnalysis"

				DoubleField
				{
					label: 				""
					name:				"optimizerInitialTau2Value"
					defaultValue:		1
					min: 				0
					inclusive: 			JASP.None
				}
			}

			CheckBox
			{
				name:		"optimizerMinimumTau2"
				text:		qsTr("Minimum 𝜏²")
				checked:	false
				childrenOnSameRow:	true
				visible:	(method.value == "pauleMandel" || method.value == "pauleMandelMu" || method.value == "qeneralizedQStatMu") &&
							sectionModel.heterogeneityModelTermsCount == 0 && module == "metaAnalysis"

				DoubleField
				{
					label: 				""
					name: 				"optimizerMinimumTau2Value"
					id:					optimizerMinimumTau2Value
					defaultValue:		1e-6
					min: 				0
					max: 				optimizerMaximumTau2Value.value
				}
			}

			CheckBox
			{
				name:		"optimizerMaximumTau2"
				text:		qsTr("Maximum 𝜏²")
				checked:	false
				childrenOnSameRow:	true
				visible:	(method.value == "pauleMandel" || method.value == "pauleMandelMu" || method.value == "qeneralizedQStatMu") &&
							sectionModel.heterogeneityModelTermsCount == 0 && module == "metaAnalysis"

				DoubleField
				{
					label: 				""
					name: 				"optimizerMaximumTau2Value"
					id:					optimizerMaximumTau2Value
					defaultValue:		100
					min: 				optimizerMinimumTau2Value.value
					inclusive: 			JASP.None
				}
			}

			CheckBox
			{
				name:		"optimizerMaximumIterations"
				text:		qsTr("Maximum iterations")
				checked:	false
				childrenOnSameRow:	true
				visible:	method.value == "restrictedML" || method.value == "maximumLikelihood" || method.value == "empiricalBayes" ||
							method.value == "pauleMandel" || method.value == "pauleMandelMu" || method.value == "qeneralizedQStatMu"

				IntegerField
				{
					label: 				""
					name: 				"optimizerMaximumIterationsValue"
					value:				{
						if (sectionModel.heterogeneityModelTermsCount == 0)
							100
						else
							1000
					}
					min: 				1
					inclusive: 			JASP.None
				}
			}

			CheckBox
			{
				name:		"optimizerConvergenceTolerance"
				text:		qsTr("Convergence tolerance")
				checked:	false
				childrenOnSameRow:	true
				visible:	(method.value == "restrictedML" || method.value == "maximumLikelihood" || method.value == "empiricalBayes" ||
							method.value == "pauleMandel" || method.value == "pauleMandelMu" || method.value == "qeneralizedQStatMu") &&
							sectionModel.heterogeneityModelTermsCount == 0

				DoubleField
				{
					label: 				""
					name: 				"optimizerConvergenceToleranceValue"
					defaultValue:		{
						if (method.value == "restrictedML" || method.value == "maximumLikelihood" || method.value == "empiricalBayes")
							1e-5
						else if (method.value == "pauleMandel" || method.value == "pauleMandelMu" || method.value == "qeneralizedQStatMu")
							1e-4
						else
							1
					}
					min: 				0
					inclusive: 			JASP.None
				}
			}

			CheckBox
			{
				name:		"optimizerConvergenceRelativeTolerance"
				text:		qsTr("Convergence relative tolerance")
				checked:	false
				childrenOnSameRow:	true
				visible:	sectionModel.heterogeneityModelTermsCount > 0

				DoubleField
				{
					label: 				""
					name: 				"optimizerConvergenceRelativeToleranceValue"
					defaultValue:		1e-8
					min: 				0
					inclusive: 			JASP.None
				}
			}

			CheckBox
			{
				name:		"optimizerStepAdjustment"
				text:		qsTr("Step adjustment")
				checked:	false
				childrenOnSameRow:	true
				visible:	(method.value == "restrictedML" || method.value == "maximumLikelihood" || method.value == "empiricalBayes") &&
							sectionModel.heterogeneityModelTermsCount == 0

				DoubleField
				{
					label: 				""
					name: 				"optimizerStepAdjustmentValue"
					defaultValue:		1
					min: 				0
					inclusive: 			JASP.None
				}
			}
		}

		CheckBox
		{
			text:		qsTr("Permutation test")
			name:		"permutationTest"
			id:			permutationTest
			visible:	module == "metaAnalysis"
			enabled:	clustering.count == 0

			RadioButtonGroup
			{
				name:		"permutationTestType"
				title:		qsTr("Type")
				columns:	2
				radioButtonsOnSameRow: true

				RadioButton
				{
					value:	"approximate"
					label:	qsTr("Approximate")
					checked: true
					id:		approximate
				}

				RadioButton
				{
					value:	"exact"
					label:	qsTr("Exact")
				}
			}

			Group
			{
				visible:	approximate.checked

				IntegerField
				{
					label: 				qsTr("Iteration")
					name: 				"permutationTestIteration"
					value:				1000
					min: 				10
					inclusive: 			JASP.None
				}

				SetSeed{}
			}

		}
	}
}