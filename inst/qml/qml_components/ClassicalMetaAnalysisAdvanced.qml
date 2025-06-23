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

Section
{
	title:							qsTr("Advanced")
	property string analysisType:	"metaAnalysis"
	columns:						1

	info: qsTr("Advanced options for the meta-analysis, including optimization settings, clustering, and permutation tests.")
	
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
				info: qsTr("Display the underlying R code used by the metafor package to fit the model.")
			}

			CheckBox
			{
				name:		"weightedEstimation"
				text:		qsTr("Weighted estimation")
				checked:	true
				info: qsTr("Perform weighted estimation using inverse-variance weights. Uncheck for unweighted estimation.")
			}

			CheckBox
			{
				name:		"includeFullDatasetInSubgroupAnalysis"
				text:		qsTr("Include full dataset in subgroup analysis")
				enabled:	subgroup.count == 1
				checked:	false
				info: qsTr("Include the full dataset output in the subgroup analysis. This option is only available when the subgroup analysis is selected.")
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
					info: qsTr("Use the clubSandwich package for robust variance estimation in clustered data. Enabled when a clustering variable is specified.")
				}

				CheckBox
				{
					name:		"clusteringSmallSampleCorrection"
					text:		qsTr("Small sample correction")
					checked:	true
					info: qsTr("Apply a small-sample correction to the clustered standard errors.")
				}
			}

			Group
			{
				visible:	analysisType === "metaAnalysisMultilevelMultivariate"
				title:		qsTr("Random Effects / Modele Structure")

				CheckBox
				{
					name:		"useSparseMatricies"
					text:		qsTr("Use sparse matricies")
					checked:	false
					info: qsTr("Use sparse matrix representations to speed up computations in models with large datasets or complex random effects structures. Available in multilevel/multivariate meta-analysis.")
				}

				CheckBox
				{
					name:		"computeCovarianceMatrix"
					text:		qsTr("Compute covariance matrix")
					checked:	true
					info: qsTr("Compute the covariance matrix of the parameter estimates. Available in multilevel/multivariate meta-analysis.")
				}
			}
		}

		Group
		{
			title:		qsTr("Fix Parameters")
			visible:	analysisType === "metaAnalysis"

			CheckBox
			{	// TODO: allow fixing in multivariate models
				name:				"fixParametersTau2"
				text:				qsTr("𝜏²")
				enabled:			sectionModel.heterogeneityModelTermsCount == 0
				childrenOnSameRow:	true
				info: qsTr("Fix the value of 𝜏² in the model instead of estimating it. Unavailable in multilevel/multivariate meta-analysis or with meta-regression model for heterogeneity. A more complex heterogeneity terms in the multilevel/multivariate meta-analysis can be fixed via the 'Extend metafor call' option.")

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
				info: qsTr("Use custom weights for the effect sizes instead of inverse-variance weights.")

				DropDown
				{
					label: 				""
					name: 				"fixParametersWeightsVariable"
					source:				"allVariables"
					addEmptyValue:		true
					allowedColumns:		["scale"]
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
				info: qsTr("Include an omnibus test for the specified effect size regression coefficients. Available when effect size model terms are included. The coefficients should be selected via their comma-separated indicies which correspond to the order presented in the 'Effect Size Meta-Regression Coefficients' Table.")

				TextField
				{
					label: 				""
					name: 				"addOmnibusModeratorTestEffectSizeCoefficientsValues"
					value:				"(1, 2)"
					info: qsTr("Specify the indices of the effect size meta-regression coefficients to include in the omnibus test, e.g. '(1, 2)' for the first and the second coefficient.")
				}
			}

			CheckBox
			{
				text:	qsTr("Heterogeneity coefficients")
				name:	"addOmnibusModeratorTestHeterogeneityCoefficients"
				enabled:			sectionModel.heterogeneityModelTermsCount > 0
				childrenOnSameRow:	false
				visible:			analysisType === "metaAnalysis"
				info: qsTr("Include an omnibus test for the specified heterogeneity regression coefficients. Available when heterogeneity model terms are included. The coefficients should be selected via their comma-separated indicies which correspond to the order presented in the 'Heterogeneity Meta-Regression Coefficients' Table.")

				TextField
				{
					label: 				""
					name: 				"addOmnibusModeratorTestHeterogeneityCoefficientsValues"
					value:				"(1, 2)"
					info: qsTr("Specify the indices of the heterogeneity meta-regression coefficients to include in the omnibus test, e.g. '(1, 2)' for the first and the second coefficient.")
				}
			}
		}

		Group
		{
			title:		qsTr("Optimizer")
			enabled:	method.value === "restrictedML" || method.value === "maximumLikelihood" || method.value === "empiricalBayes" ||
						method.value === "pauleMandel" || method.value === "pauleMandelMu" || method.value === "qeneralizedQStatMu" ||
						method.value === "sidikJonkman"
			info: qsTr("Optimizer settings for estimating the meta-analytic models. A more complex/unavailbe settings can be specified via the 'Extend metafor call' option.")

			DropDown
			{
				name:		"optimizerMethod"
				id:			optimizerMethod
				label:		qsTr("Method") // TODO: switch default value on heterogeneityModelLink change
				info: qsTr("Select the optimization method to use for fitting the model. Available in multilevel/multivariate meta-analysis or when heterogeneity model terms are included.")
				values:		{
					if (analysisType === "metaAnalysis") {
						if (sectionModel.heterogeneityModelLinkValue === "log")
							["nlminb", "BFGS", "Nelder-Mead", "uobyqa", "newuoa", "bobyqa", "nloptr", "nlm"]
						else
							["constrOptim", "nlminb", "BFGS", "Nelder-Mead", "uobyqa", "newuoa", "bobyqa", "nloptr", "nlm"]
					} else	if (analysisType === "metaAnalysisMultilevelMultivariate") {
							["nlminb", "BFGS", "Nelder-Mead", "uobyqa", "newuoa", "bobyqa", "nloptr", "nlm", "hjk", "nmk", "mads"] // many else could be added "ucminf", "lbfgsb3c", "BBoptim"
					}

				}
				visible:	analysisType === "metaAnalysisMultilevelMultivariate" || sectionModel.heterogeneityModelTermsCount > 0
			}

			CheckBox
			{
				name:		"optimizerInitialTau2"
				text:		qsTr("Initial 𝜏²")
				checked:	false
				childrenOnSameRow:	true
				info: qsTr("Specify the initial value of 𝜏² for the optimization algorithm. Available only for specific optimization methods and unavailable in multilevel/multivariate meta-analysis or when heterogeneity model terms are included.")
				visible:	(method.value === "restrictedML" || method.value === "maximumLikelihood" || method.value === "empiricalBayes" ||
							method.value === "sidikJonkman") && sectionModel.heterogeneityModelTermsCount == 0 && analysisType === "metaAnalysis"

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
				info: qsTr("Specify the minimum allowable value of 𝜏² during optimization. Available only for specific optimization methods and unavailable in multilevel/multivariate meta-analysis or when heterogeneity model terms are included.")
				visible:	(method.value === "pauleMandel" || method.value === "pauleMandelMu" || method.value === "qeneralizedQStatMu") &&
							sectionModel.heterogeneityModelTermsCount == 0 && analysisType === "metaAnalysis"

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
				info: qsTr("Specify the maximim allowable value of 𝜏² during optimization. Available only for specific optimization methods and unavailable in multilevel/multivariate meta-analysis or when heterogeneity model terms are included.")
				visible:	((method.value === "pauleMandel" || method.value === "pauleMandelMu" || method.value === "qeneralizedQStatMu") &&
							sectionModel.heterogeneityModelTermsCount == 0 && analysisType === "metaAnalysis")

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
				name:		"optimizerMaximumEvaluations"
				text:		qsTr("Maximum evaluations")
				checked:	false
				childrenOnSameRow:	true
				info: qsTr("Set the maximum number of function evaluations for the optimizer. Available when using specific optimization methods in multilevel/multivariate meta-analysis.")
				visible:	(optimizerMethod.value === "nlminb" || optimizerMethod.value === "uobyqa" || optimizerMethod.value === "newuoa" || optimizerMethod.value === "bobyqa" ||
							optimizerMethod.value === "hjk" || optimizerMethod.value === "nmk" || optimizerMethod.value === "mads") && analysisType === "metaAnalysisMultilevelMultivariate"

				IntegerField
				{
					label: 				""
					name: 				"optimizerMaximumEvaluationsValue"
					value:				250
					min: 				1
					inclusive: 			JASP.None
				}
			}

			CheckBox
			{
				name:		"optimizerMaximumIterations"
				text:		qsTr("Maximum iterations")
				checked:	false
				childrenOnSameRow:	true
				info: qsTr("Set the maximum number of iterations for the optimizer. Available when using certain estimation or optimization methods.")
				visible:	((method.value === "restrictedML" || method.value === "maximumLikelihood" || method.value === "empiricalBayes" ||
							method.value === "pauleMandel" || method.value === "pauleMandelMu" || method.value === "qeneralizedQStatMu") && analysisType === "metaAnalysis") ||
							((optimizerMethod.value === "nlminb" || optimizerMethod.value === "Nelder-Mead" || optimizerMethod.value === "BFGS" || 
							optimizerMethod.value === "nloptr" || optimizerMethod.value === "nlm") && analysisType === "metaAnalysisMultilevelMultivariate")

				IntegerField
				{
					label: 				""
					name: 				"optimizerMaximumIterationsValue"
					value:				{
						if (sectionModel.heterogeneityModelTermsCount == 0)
							150
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
				info: qsTr("Set the convergence tolerance for the optimizer. Available when using certain methods without heterogeneity model terms or specific optimizers in multilevel/multivariate meta-analysis.")
				visible:	((method.value === "restrictedML" || method.value === "maximumLikelihood" || method.value === "empiricalBayes" ||
							method.value === "pauleMandel" || method.value === "pauleMandelMu" || method.value === "qeneralizedQStatMu") &&
							sectionModel.heterogeneityModelTermsCount == 0 && analysisType === "metaAnalysis") ||
							((optimizerMethod.value === "hjk" || optimizerMethod.value === "nmk" || optimizerMethod.value === "mads") && analysisType === "metaAnalysisMultilevelMultivariate")

				DoubleField
				{
					label: 				""
					name: 				"optimizerConvergenceToleranceValue"
					defaultValue:		{
						if (method.value === "restrictedML" || method.value === "maximumLikelihood" || method.value === "empiricalBayes")
							1e-5
						else if (method.value === "pauleMandel" || method.value === "pauleMandelMu" || method.value === "qeneralizedQStatMu")
							1e-4
						else
							1
					}
					min: 				0
					inclusive: 			JASP.None
					decimals:			5
				}
			}

			CheckBox
			{
				name:		"optimizerConvergenceRelativeTolerance"
				text:		qsTr("Convergence relative tolerance")
				checked:	false
				childrenOnSameRow:	true
				info: qsTr("Set the relative convergence tolerance for the optimizer. Available when heterogeneity model terms are included or using specific optimizers in multilevel/multivariate meta-analysis.")
				visible:	(sectionModel.heterogeneityModelTermsCount > 0  && analysisType === "metaAnalysis") ||
							((optimizerMethod.value === "nlminb" || optimizerMethod.value === "Nelder-Mead" || optimizerMethod.value === "BFGS") && analysisType === "metaAnalysisMultilevelMultivariate")

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
				info: qsTr("Set the step adjustment factor for the optimizer. Available when using certain methods without heterogeneity model terms and unavailable in multilevel/multivariate meta-analysis.")
				visible:	((method.value === "restrictedML" || method.value === "maximumLikelihood" || method.value === "empiricalBayes") &&
							sectionModel.heterogeneityModelTermsCount == 0 && analysisType === "metaAnalysis")


				DoubleField
				{
					label: 				""
					name: 				"optimizerStepAdjustmentValue"
					defaultValue:		1
					min: 				0
					inclusive: 			JASP.None
				}
			}

			CheckBox
			{
				name:		"optimizerInitialTrustRegionRadius"
				text:		qsTr("Initial trust region radius")
				checked:	false
				childrenOnSameRow:	true
				info: qsTr("Set the initial trust region radius for the optimizer. Available when using specific optimization methods in multilevel/multivariate meta-analysis.")
				visible:	((optimizerMethod.value === "uobyqa" || optimizerMethod.value === "newuoa" || optimizerMethod.value === "bobyqa") && analysisType === "metaAnalysisMultilevelMultivariate")

				DoubleField
				{
					label: 				""
					name: 				"optimizerInitialTrustRegionRadiusValue"
					defaultValue:		1
					min: 				0
					inclusive: 			JASP.None
				}
			}

			CheckBox
			{
				name:		"optimizerFinalTrustRegionRadius"
				text:		qsTr("Final trust region radius")
				checked:	false
				childrenOnSameRow:	true
				info: qsTr("Set the final trust region radius for the optimizer. Available when using specific optimization methods in multilevel/multivariate meta-analysis.")
				visible:	((optimizerMethod.value === "uobyqa" || optimizerMethod.value === "newuoa" || optimizerMethod.value === "bobyqa") && analysisType === "metaAnalysisMultilevelMultivariate")

				DoubleField
				{
					label: 				""
					name: 				"optimizerFinalTrustRegionRadiusValue"
					defaultValue:		1
					min: 				0
					inclusive: 			JASP.None
				}
			}

			CheckBox
			{
				name:		"optimizerMaximumRestarts"
				text:		qsTr("Maximum restarts")
				checked:	false
				childrenOnSameRow:	true
				info: qsTr("Set the maximum number of restarts for the optimizer. Available when using the Nelder-Mead method ('nmk') in multilevel/multivariate meta-analysis.")
				visible:	optimizerMethod.value === "mmk" && analysisType === "metaAnalysisMultilevelMultivariate"

				IntegerField
				{
					label: 				""
					name: 				"optimizerMaximumRestartsValue"
					defaultValue:		3
					min: 				1
					inclusive: 			JASP.None
				}
			}
		}

		CheckBox
		{
			text:		qsTr("Permutation test")
			name:		"permutationTest"
			id:			permutationTest
			visible:	analysisType === "metaAnalysis"
			enabled:	clustering.count == 0
			info: qsTr("Perform a permutation test for the model coefficients. Available in the meta-analysis analysisType when clustering is not specified. The resulting permuation p-values are displayed in the 'p (permutation)' column. Note that permutation can be computationally intesive.")


			RadioButtonGroup
			{
				name:		"permutationTestType"
				title:		qsTr("Type")
				columns:	2
				radioButtonsOnSameRow: true
				info: qsTr("Select the type of permutation test.")

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
					info: qsTr("Specify the number of permutations to use in the approximate permutation test.")
				}

				SetSeed{}
			}

		}
	}

	CheckBox
	{
		name:		"advancedExtendMetaforCall"
		id:			advancedExtendMetaforCall
		text:		qsTr("Extend metafor call")
		checked:	false
		info: qsTr("Allow adding custom arguments to the metafor function call. Consult the metafor R package documentation for the available commands (for %1meta-analysis%2 and %3meta-analysis (multilevel/multivariate)%4).")
			.arg("<a href='https://wviechtb.github.io/metafor/reference/rma.uni.html'>")
			.arg("</a>")
			.arg("<a href='https://wviechtb.github.io/metafor/reference/rma.mv.html'>")
			.arg("</a>")
	}

	TextArea
	{
		name: 				"advancedExtendMetaforCallCode"
		visible:			advancedExtendMetaforCall.checked
		info: qsTr("The additional arguments to the metafor function call must be specified as a named list (the 'list()' call can be ommited). E.g., 'list(tau2 = 1)' (or 'tau2 = 1') can be used to fix the between-study heterogeneity to a given value. Multiple arguments must be comma-seprated, e.g. 'list(tau2 = 1, gamma2 = 0.5)' (or 'tau2 = 1, gamma2 = 0.5'). New lines are ignored." )
	}
}
