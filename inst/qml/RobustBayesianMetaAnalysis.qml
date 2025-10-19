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
	info: qsTr("Robust Bayesian meta-analysis allows you to conduct a publication-bias adjusted meta-analysis using the Bayesian approach. " + 
	"By default, Bayesian meta-analysis model-averages across models assuming the presence vs. absence of the effect, heterogeneity, publication bias, and moderation (if specified). " +
	"The analysis provides pre-specified prior distributions for different effect size measures and fields. " + 
	"Additional care is required when using a different type of effect sizes as the default prior distributions might not match the proper scaling of the effect size and heterogeneity. " + 
	"The analysis allows you to specify meta-regression, 3-level meta-analysis, and subgroup analysis. " +
	"The results include estimates of effect sizes, heterogeneity, moderation, and various plots to visualize the results.\n\n" +
	"The analysis is based on the Bayesian meta-analysis/meta-regression parameterization as outlined in Bartoš et al. (2025)."\n\n" + 
	"See [this tutorial](https://doi.org/10.48550/arXiv.2509.09850) for a detailed introduction to the module.")
	infoBottom: "## " + qsTr("References") + "\n" +
	"- Bartoš F & Wagenmakers EJ (2025). “Meta-analysis with JASP, Part II: Bayesian approaches.” _ArXiv Preprint_. https://doi.org/10.48550/arXiv.2509.09850\n" + 
	"- Bartoš F, Gronau QF, Timmers B, Otte WM, Ly A, Wagenmakers EJ (2021). “Bayesian model‐averaged meta‐analysis in medicine.” _Statistics in Medicine, 40_(30), 6743-6761. https://doi.org/10.1002/sim.9170\n" +
	"- Bartoš F, Maier M, Stanley TD, Wagenmakers EJ (2025). “Robust Bayesian meta-regression: Model-averaged moderation analysis in the presence of publication bias.” _Psychological Methods_.  https://doi.org/10.1037/met0000737\n" +
	"- Bartoš F, Maier M, Quintana DS, Wagenmakers EJ (2022). “Adjusting for publication bias in JASP and R: Selection models, PET-PEESE, and robust Bayesian meta-analysis.” _Advances in Methods and Practices in Psychological Science, 5_(3), 25152459221109259. https://doi.org/10.1177/25152459221109259\n" +
	"- Bartoš F, Maier M, Wagenmakers E, Doucouliagos H, Stanley TD (2023). “Robust Bayesian meta-analysis: Model-averaging across complementary publication bias adjustment methods.” _Research Synthesis Methods, 14_(1), 99–116. https://doi.org/10.1002/jrsm.1594\n" +
	"- Bartoš F, Otte WM, Gronau QF, Timmers B, Ly A, Wagenmakers EJ (2023). “Empirical prior distributions for Bayesian meta-analyses of binary and time to event outcomes.” _arXiv Preprint_ https://doi.org/10.48550/arXiv.2306.11468\n" + 
	"- Bartoš F, Maier M, Wagenmakers EJ (2025). “Robust Bayesian multilevel meta-analysis: Adjusting for publication bias in the presence of dependent effect sizes”  _PsyArXiv Preprint_\n" +
	"- Maier M, Bartoš F, Wagenmakers E (2023). “Robust Bayesian meta-analysis: Addressing publication bias with model-averaging.” _Psychological Methods, 28_(1), 107–122. https://doi.org/10.1037/met0000405\n" +
	"- van Erp S, Verhagen J, Grasman RP, Wagenmakers EJ (2017). “Estimates of between-study heterogeneity for 705 meta-analyses reported in Psychological Bulletin from 1990–2013.” _Journal of Open Psychology Data, 5_(1), 1–5. https://doi.org/10.5334/jopd.33\n" +
	"- Bartoš F, Maier M, (2025) _RoBMA: An R package for robust Bayesian meta-analyses_. R package version 3.5.0 Available at: <https://CRAN.R-project.org/package=RoBMA>.\n" +
	"## " + qsTr("R Packages") + "\n" +
	"- RoBMA"

	VariablesForm
	{
		preferredHeight:	400 * preferencesModel.uiScale
		removeInvisibles:	true

		AvailableVariablesList
		{
			name:				"allVariables"
		}

		AssignedVariablesList
		{
			name:				"effectSize"
			id:					effectSize
			title:				qsTr("Effect Size")
			singleVariable:		true
			allowedColumns:		["scale"]
			info: qsTr("Variable containing the observed effect sizes.")
		}
		AssignedVariablesList
		{
			name:				"effectSizeStandardError"
			id:					effectSizeStandardError
			title:				qsTr("Effect Size Standard Error")
			singleVariable:		true
			allowedColumns:		["scale"]
			info: qsTr("Variable containing the standard errors corresponding to the effect sizes.")
		}

		DropDown
		{
			id:			effectSizeMeasure
			name:		"effectSizeMeasure"
			label:		qsTr("Effect size measure")
			info: qsTr("Effect size measure supplied as the 'Effect Size'. This is an important setting as it determines the defaul prior distributions and the model fitting transformation.")
			values: [
				{ label: qsTr("Standardized mean difference"),	value: "SMD",			info: qsTr("Standardized mean difference such as Cohen's d, Hedge's g")},
				{ label: qsTr("Fisher's z"),					value: "fishersZ",		info: qsTr("Fisher's z transformation of correlation coefficients")},
				{ label: qsTr("Log odds ratio"),				value: "logOR",			info: qsTr("Log odds ratio")},
				{ label: qsTr("Log risk ratio"),				value: "logRR",			info: qsTr("Log risk ratio")},
				{ label: qsTr("Log hazard ratio"),				value: "logHR",			info: qsTr("Log hazard ratio")},
				{ label: qsTr("Risk difference"),				value: "RD",			info: qsTr("Risk difference")},
				{ label: qsTr("General"),						value: "general",		info: qsTr("General effect size that does not correspond to any of the measures above. Please, be careful when specifying the prior distributions as no sensible defaults are available.")}
			]
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
			info: qsTr("Variable indicating the study level nesting. This variable is used to specify the nesting of the studies in the meta-analysis. The nesting is used to specify the model structure and to account for the correlation between the effect sizes within each study. Note that this is an experimental feature that needs to be manually enabled in the 'Advanced' section.")
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
		title:		qsTr("Bayesian Model Averaging")
		info:		qsTr("Specify which components should be included Bayesian model averaging. If selected, prior distribution under both the presence and absence of the component are specified. This allows for testing for the presence vs. absence of the component (if the component is not selected, a Bayes factor test is not conducted). The displayed estimates are averaged across null and alternative prior distributions of all specified components.")

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

		CheckBox
		{
			name:		"bayesianModelAveragingPublicationBias"
			id:			bayesianModelAveragingPublicationBias
			label:		qsTr("Publication bias")
			info:		qsTr("Average over the presence vs. absence of publication bias. If unspecified, the resulting model assumes that publication bias is present.")
			checked:	true
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
				if (effectSizeMeasure.value === "SMD" || effectSizeMeasure.value === "fishersZ" || effectSizeMeasure.value === "logOR")
					[
						{ label: qsTr("Default"),		value: "default",		info: qsTr("Use default prior distributions for the effect size and heterogeneity based on Bartoš et al. (2022).")},
						{ label: qsTr("Medicine"),		value: "medicine",		info: qsTr("Use prior distributions based on the Cochrane Database of Systematic Reviews developed by Bartoš et al. (2021)")},
						{ label: qsTr("Custom"),		value: "custom",		info: qsTr("Use custom prior distributions. This option allows you to specify a custom model ensemble for the effect size and heterogeneity prior distributions.")}
					]
				else if (effectSizeMeasure.value === "logRR" || effectSizeMeasure.value === "logHR" || effectSizeMeasure.value === "RD")
					[
						{ label: qsTr("Medicine"),		value: "medicine",		info: qsTr("Use prior distributions based on the Cochrane Database of Systematic Reviews developed by Bartoš et al. (2023).")},
						{ label: qsTr("Custom"),		value: "custom",		info: qsTr("Use custom prior distributions. This option allows you to specify a custom model ensemble for the effect size and heterogeneity prior distributions.")}
					]
				else 
					[
						{ label: qsTr("Custom"),		value: "custom",		info: qsTr("Use custom prior distributions. This option allows you to specify a custom model ensemble for the effect size and heterogeneity prior distributions.")}
					]
		}

		MA.RobustBayesianMetaAnalysisCochranePriorDistributions
		{
			visible:				priorDistributionsEffectSizeAndHeterogeneity.value === "medicine"
			effectSizeMeasure:		effectSizeMeasure.value
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

		DropDown
		{
			name:		"publicationBiasAdjustment"
			id:			publicationBiasAdjustment
			label:		qsTr("Publication bias adjustment")
			info:		qsTr("Publication bias adjustment method. The default is the RoBMA-PSMA method (combination of 6 weight functions and PET-PEESE).")
			startValue:	"PSMA"
			values: [
				{ label: qsTr("RoBMA-PSMA"),		value: "PSMA",		info: qsTr("RoBMA-PSMA method (combination of 6 weight functions and PET-PEESE) as described in Bartoš et al. (2022). This is the recommended method.")},
				{ label: qsTr("RoBMA-PP"),			value: "PP",		info: qsTr("RoBMA PET-PEESE method. A simplification of the RoBMA-PSMA method that uses only the PET-PEESE weight function.")},
				{ label: qsTr("RoBMA-original"),	value: "original",	info: qsTr("Original RoBMA method (combination of 2 weight functions) as described in Maier et al. (2023).")},
				{ label: qsTr("Custom"),			value: "custom",	info: qsTr("Custom model ensemble. This option allows you to specify a custom model ensemble for the publication bias adjustment.")},
				{ label: qsTr("None"),				value: "none",		info: qsTr("No publication bias adjustment.")}
			]
		}

		DropDown
		{
			name:		"modelExpectedDirectionOfTheEffect"
			label:		qsTr("Expected direction of the effect")
			enabled:	modelEnsembleTestPublicationBias.checked
			info:		qsTr("The expected direction of the effect. This setting is important for the publication bias adjustment (one-sided selection models specify different publication probabilities for estimates in the expected vs. unexpected direction and PET-PEESE is bounded to adjust in the expected direction). The default is 'Positive'.")
			startValue:	"detect"
			values: [
				{ label: qsTr("Detect"),	value: "detect",	info: qsTr("Determine the expected direction of the effect automatically based on the median effect size in the data.")},
				{ label: qsTr("Positive"),	value: "positive",	info: qsTr("Positive effect")},
				{ label: qsTr("Negative"),	value: "negative",	info: qsTr("Negative effect")}
			]
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
		analysisType:	"RoBMA"
		id:				sectionModel
	}

	//// Priors Section ////
	Section
	{
		title: 				qsTr("Prior Distributions (Custom)")
		columns:			1
		enabled:			priorDistributionsEffectSizeAndHeterogeneity.value === "custom" || publicationBiasAdjustment.value === "custom"
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

		// bias priors
		MA.RobustBayesianMetaAnalysisWeightfunctions
		{
			visible:				publicationBiasAdjustment.value === "custom"
			Layout.preferredWidth:	parent.width
			componentType:			"priorsBiasSelectionModels"
		}

		MA.RobustBayesianMetaAnalysisPriors
		{
			visible:				publicationBiasAdjustment.value === "custom"
			Layout.preferredWidth:	parent.width
			componentType:			"priorsBiasPet"
			analysisType:			"normal"
		}

		MA.RobustBayesianMetaAnalysisPriors
		{
			visible:				publicationBiasAdjustment.value === "custom"
			Layout.preferredWidth:	parent.width
			componentType:			"priorsBiasPeese"
			analysisType:			"normal"
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

		// bias priors
		MA.RobustBayesianMetaAnalysisWeightfunctions
		{
			visible:				publicationBiasAdjustment.value === "custom" && bayesianModelAveragingPublicationBias.checked
			Layout.preferredWidth:	parent.width
			componentType:			"priorsBiasSelectionModelsNull"
		}

		MA.RobustBayesianMetaAnalysisPriors
		{
			visible:				publicationBiasAdjustment.value === "custom" && bayesianModelAveragingPublicationBias.checked
			Layout.preferredWidth:	parent.width
			componentType:			"priorsBiasPetNull"
			analysisType:			"normal"
		}

		MA.RobustBayesianMetaAnalysisPriors
		{
			visible:				publicationBiasAdjustment.value === "custom" && bayesianModelAveragingPublicationBias.checked
			Layout.preferredWidth:	parent.width
			componentType:			"priorsBiasPeeseNull"
			analysisType:			"normal"
		}
	}

	//// Inference Section ////
	MA.RobustBayesianMetaAnalysisInference
	{
		analysisType:	"RoBMA"
	}

	//// Inference Section ////
	MA.RobustBayesianMetaAnalysisEstimatedMarginalMeans
	{
		analysisType:	"RoBMA"
	}

	//// Prior and Posterior Plots Section ////
	MA.RobustBayesianMetaAnalysisPlots
	{
		analysisType:	"RoBMA"
	}

	//// Forest Plots Section ////
	MA.ForestPlot
	{
		analysisType:	"RoBMA"
	}

	//// Bubble Plot Section ////
	MA.BubblePlot
	{
		analysisType:	"RoBMA"
	}

	//// Diagnostics section ////
	MA.RobustBayesianMetaAnalysisDiagnostics
	{
		analysisType:	"RoBMA"
	}

	//// Advanced section for prior model probabilities sampling settings ////
	MA.RobustBayesianMetaAnalysisAdvanced
	{
		analysisType:	"RoBMA"
		id:				sectionAdvanced
	}

}
