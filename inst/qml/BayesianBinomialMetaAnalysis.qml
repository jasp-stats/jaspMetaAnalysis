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
import "./qml_components"		as MA

Form
{
	info: qsTr("Bayesian Binomial meta-analysis allows you to conduct a binomial meta-analysis using the Bayesian approach. " + 
	"By default, Bayesian meta-analysis model-averages across models assuming the presence vs. absence of the effect, heterogeneity, and moderation (if specified). " +
	"The analysis provides pre-specified prior distributions for different effect size measures and fields. " + 
	"The analysis allows you to specify meta-regression, 3-level meta-analysis, and subgroup analysis. " +
	"The results include estimates of effect sizes, heterogeneity, moderation, and various plots to visualize the results.\n\n" +
	"The analysis is based on the Bayesian meta-analysis/meta-regression parameterization as outlined in Bartoš et al. (2025) with the binomial-normal model described in Bartoš et al. (2023).")
	infoBottom: "## " + qsTr("References") + "\n" +
	"- Bartoš F, Gronau QF, Timmers B, Otte WM, Ly A, Wagenmakers EJ (2021). “Bayesian model‐averaged meta‐analysis in medicine.” _Statistics in Medicine, 40_(30), 6743-6761. https://doi.org/10.1002/sim.9170\n" +
	"- Bartoš F, Otte WM, Gronau QF, Timmers B, Ly A, Wagenmakers EJ (2023). “Empirical prior distributions for Bayesian meta-analyses of binary and time to event outcomes.” _arXiv Preprint_ https://doi.org/10.48550/arXiv.2306.11468\n" + 
	"- Bartoš F, Maier M, Stanley TD, Wagenmakers EJ (2025). “Robust Bayesian meta-regression: Model-averaged moderation analysis in the presence of publication bias.” _Psychological Methods_.  https://doi.org/10.1037/met0000737\n" +
	"- Bartoš F, Maier M, Wagenmakers EJ (2025). “Robust Bayesian multilevel meta-analysis: Adjusting for publication bias in the presence of dependent effect sizes”  _PsyArXiv Preprint_\n" +
    "- Bartoš F, Maier M, (2025) _RoBMA: An R package for robust Bayesian meta-analyses_. R package version 3.5.0 Available at: <https://CRAN.R-project.org/package=RoBMA>.\n" +
	"## " + qsTr("R Packages") + "\n" +
	"- RoBMA"

	VariablesForm
	{
		preferredHeight:	500 * preferencesModel.uiScale

		AvailableVariablesList
		{
			name:				"allVariables"
		}

		AssignedVariablesList
		{
			name: 			"successesGroup1"
			title: 			qsTr("Successes Group 1")
			singleVariable: true
			allowedColumns: ["scale"]
		}

		AssignedVariablesList
		{
			name: 			"successesGroup2"
			title: 			qsTr("Successes Group 2")
			singleVariable: true
			allowedColumns: ["scale"]
		}

		AssignedVariablesList
		{
			name: 			"sampleSizeGroup1"
			title: 			qsTr("Sample Size Group 1")
			singleVariable: true
			allowedColumns: ["scale"]
		}

		AssignedVariablesList
		{
			name: 			"sampleSizeGroup2"
			title: 			qsTr("Sample Size Group 2")
			singleVariable: true
			allowedColumns: ["scale"]
		}

		AssignedVariablesList
		{
			name:				"predictors"
			id:					predictors
			title:				qsTr("Predictors")
			allowedColumns:		["nominal", "scale"]
			allowTypeChange:	true
			info: qsTr("Variables to include as predictors (moderators) in the meta-regression model. See the 'Model' section for the meta-regression specification details.")
		}


		AssignedVariablesList
		{
			name:				"studyLevelMultilevel"
			title:				qsTr("Study Level (Multilevel)")
			id:					studyLevelMultilevel
			singleVariable:		true
			allowedColumns:		["nominal"]
			info: qsTr("Variable indicating the study level nesting. This variable is used to specify the nesting of the studies in the meta-analysis. The nesting is used to specify the model structure and to account for the correlation between the effect sizes within each study.")
		}

		AssignedVariablesList
		{
			name:				"subgroup"
			id:					subgroup
			title:				qsTr("Subgroup")
			singleVariable:		true
			allowedColumns:		["nominal"]
			info: qsTr("Variable indicating subgroup stratification. For each subgroup, an independent model is fitted to the corresponding data set subset.")
		}
	}


	Group
	{
		title:		qsTr("Bayesian Model-Averaging")
		info:		qsTr("Specify which components should be included Bayesian model-averaging. If selected, prior distribution under both the presence and absence of the component are specified. This allows for testing for the presence vs. absence of the component (if the component is not selected, a Bayes factor test is not conducted). The displayed estimates are averaged across null and alternative prior distributions of all specified components.")

		CheckBox
		{
			name:		"bayesianModelAveragingEffectSize"
			id:			bayesianModelAveragingEffectSize
			label:		qsTr("Effect size")
			info:		qsTr("Average over the presence vs. absence of the effect. If unspecified, the resulting model assumes that the effect is present.")
			checked:	true
		}

		CheckBox
		{
			name:		"bayesianModelAveragingHeterogeneity"
			id:			bayesianModelAveragingHeterogeneity
			label:		qsTr("Heterogeneity")
			info:		qsTr("Average over the presence vs. absence of heterogeneity. If unspecified, the resulting model assumes that heterogeneity is present.")
			checked:	true
		}

		CheckBox
		{
			name:		"bayesianModelAveragingModerations"
			id:			bayesianModelAveragingModerations
			label:		qsTr("Moderation")
			info:		qsTr("Average over the presence vs. absence of moderators. If unspecified, the resulting model assumes that all moderators models are present.")
			checked:	true
			enabled:	predictors.count > 0
		}
	}

	Group
	{
		title:		qsTr("Prior Distributions")

		DropDown
		{
			name:		"priorDistributionsEffectSizeAndHeterogeneity"
			id:			priorDistributionsEffectSizeAndHeterogeneity
			label:		qsTr("Effect size and heterogeneity")
			info:		qsTr("Specify the type of prior distributions for the effect size and heterogeneity.")
			values: 
			[
				{ label: qsTr("Default"),		value: "default",		info: qsTr("Use default prior distributions for the effect size and heterogeneity based on Bartoš et al. (2022).")},
				{ label: qsTr("Medicine"),		value: "medicine",		info: qsTr("Use prior distributions based on the Cochrane Database of Systematic Reviews developed by Bartoš et al. (2021)")},
				{ label: qsTr("Custom"),		value: "custom",		info: qsTr("Use custom prior distributions. This option allows you to specify a custom model ensemble for the effect size and heterogeneity prior distributions.")}
			]
		}

		MA.RobustBayesianMetaAnalysisCochranePriorDistributions
		{
			visible:				priorDistributionsEffectSizeAndHeterogeneity.value === "medicine"
			effectSizeMeasure:		"logOR"
		}

		DoubleField
		{
			name:		"priorDistributionsScale"
			label:		qsTr("Scale")
			enabled:	priorDistributionsEffectSizeAndHeterogeneity.value === "default" || priorDistributionsEffectSizeAndHeterogeneity.value === "medicine"
			info:		qsTr("Setting value different than 1 re-scales the pre-specified prior distributions. Values smaller than 1 lead to more informative priors, while values larger than 1 lead to less informative priors.")
			startValue:	1
			min:		0
		}

		CheckBox
		{
			name:		"showModelSpecification"
			label:		qsTr("Show model specification")
			info:		qsTr("Show the model specification. This is useful for understanding and reporting the specified model.")
			checked:	false
		}
	}

	//// Model Section ////
	MA.RobustBayesianMetaAnalysisModel
	{
		analysisType:	"BiBMA"
		id:				sectionModel
	}

	//// Priors Section ////
	Section
	{
		title: 				qsTr("Prior Distributions (Custom)")
		columns:			1
		enabled:			priorDistributionsEffectSizeAndHeterogeneity.value === "custom"
		onEnabledChanged:	if(!enabled) expanded = false


		// effect prior
		MA.RobustBayesianMetaAnalysisPriors
		{
			visible:				priorDistributionsEffectSizeAndHeterogeneity.value === "custom"
			Layout.preferredWidth:	parent.width
			componentType:			"priorsEffect"
			analysisType:			"normal"
		}

		// heterogeneity prior
		MA.RobustBayesianMetaAnalysisPriors
		{
			visible:				priorDistributionsEffectSizeAndHeterogeneity.value === "custom"
			Layout.preferredWidth:	parent.width
			componentType:			"priorsHeterogeneity"
			analysisType:			"normal"
		}

		// moderation (continuous) prior
		MA.RobustBayesianMetaAnalysisPriorsContinuousModerators
		{
			visible:				priorDistributionsEffectSizeAndHeterogeneity.value === "custom" && predictors.count > 0
			Layout.preferredWidth:	parent.width
			componentType:			"alternative"
		}

		// moderation (factor) prior
		MA.RobustBayesianMetaAnalysisPriorsFactorModerators
		{
			visible:				priorDistributionsEffectSizeAndHeterogeneity.value === "custom" && predictors.count > 0
			Layout.preferredWidth:	parent.width
			componentType:			"alternative"
		}

		// baseline prior
		MA.RobustBayesianMetaAnalysisBaseline
		{
			Layout.preferredWidth:	parent.width
			componentType:			"priorsBaseline"
		}

		Divider { }

		// effect prior
		MA.RobustBayesianMetaAnalysisPriors
		{
			visible:				priorDistributionsEffectSizeAndHeterogeneity.value === "custom" && bayesianModelAveragingEffectSize.checked
			Layout.preferredWidth:	parent.width
			componentType:			"priorsEffectNull"
			analysisType:			"normal"
		}

		// effect prior
		MA.RobustBayesianMetaAnalysisPriors
		{
			visible:				priorDistributionsEffectSizeAndHeterogeneity.value === "custom" && bayesianModelAveragingHeterogeneity.checked
			Layout.preferredWidth:	parent.width
			componentType:			"priorsHeterogeneityNull"
			analysisType:			"normal"
		}

		// moderation (continuous) prior
		MA.RobustBayesianMetaAnalysisPriorsContinuousModerators
		{
			visible:				priorDistributionsEffectSizeAndHeterogeneity.value === "custom" && predictors.count > 0 && bayesianModelAveragingModerations.checked
			Layout.preferredWidth:	parent.width
			componentType:			"null"
		}

		// moderation (factor) prior
		MA.RobustBayesianMetaAnalysisPriorsFactorModerators
		{
			visible:				priorDistributionsEffectSizeAndHeterogeneity.value === "custom" && predictors.count > 0 && bayesianModelAveragingModerations.checked
			Layout.preferredWidth:	parent.width
			componentType:			"null"
		}

		// baseline prior
		MA.RobustBayesianMetaAnalysisBaseline
		{
			Layout.preferredWidth:	parent.width
			componentType:			"priorsBaselineNull"
		}
	}

	//// Inference Section ////
	MA.RobustBayesianMetaAnalysisInference
	{
		analysisType:	"BiBMA"
	}

	//// Inference Section ////
	MA.RobustBayesianMetaAnalysisEstimatedMarginalMeans
	{
		analysisType:	"BiBMA"
	}

	//// Prior and Posterior Plots Section ////
	MA.RobustBayesianMetaAnalysisPlots
	{
		analysisType:	"BiBMA"
	}

	//// Forest Plots Section ////
	MA.ForestPlot
	{
		analysisType:	"BiBMA"
	}

	//// Bubble Plot Section ////
	MA.BubblePlot
	{
		analysisType:	"BiBMA"
	}

	//// Diagnostics section ////
	MA.RobustBayesianMetaAnalysisDiagnostics
	{
		analysisType:	"BiBMA"
	}

	//// Advanced section for prior model probabilities sampling settings ////
	MA.RobustBayesianMetaAnalysisAdvanced
	{
		analysisType:	"BiBMA"
		id:				sectionAdvanced
	}

}
