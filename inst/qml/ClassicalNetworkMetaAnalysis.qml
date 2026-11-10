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
import "./qml_components" as MA

Form
{
	info: qsTr("Classical network meta-analysis compares multiple treatments simultaneously by combining direct and indirect evidence from studies comparing different sets of treatments. " +
		"It uses the frequentist graph-theoretical approach implemented in the netmeta package and supports contrast-based data (one row per treatment comparison) and arm-based data (one row per study arm) for binary, continuous, incidence rate, and generic outcomes. " +
		"The analysis provides common-effect and random-effects network estimates, heterogeneity and inconsistency assessments, treatment rankings, league tables, network and forest plots, small-study effects assessment, network meta-regression, subgroup analysis, rare-event pooling methods, component network meta-analysis, and the netmeta R code.")
	infoBottom: "## " + qsTr("References") + "\n" +
		"- Balduzzi S, Rücker G, Nikolakopoulou A, Papakonstantinou T, Salanti G, Efthimiou O, Schwarzer G (2023). “netmeta: An R Package for Network Meta-Analysis Using Frequentist Methods.” _Journal of Statistical Software, 106_(2), 1-40. https://doi.org/10.18637/jss.v106.i02\n" +
		"- Begg CB, Mazumdar M (1994). “Operating characteristics of a rank correlation test for publication bias.” _Biometrics, 50_, 1088-101.\n" +
		"- Chaimani A, Salanti G (2012). “Using network meta-analysis to evaluate the existence of small-study effects in a network of interventions.” _Research Synthesis Methods, 3_, 161-76.\n" +
		"- Davies AL, Papakonstantinou T, Nikolakopoulou A, Rücker G, Galla T (2022). “Network meta-analysis and random walks.” _Statistics in Medicine, 41_, 2091-2114.\n" +
		"- Dias S, Welton NJ, Caldwell DM, Ades AE (2010). “Checking consistency in mixed treatment comparison meta-analysis.” _Statistics in Medicine, 29_, 932-44.\n" +
		"- Efthimiou O, Rücker G, Schwarzer G, Higgins J, Egger M, Salanti G (2019). “A Mantel-Haenszel model for network meta-analysis of rare events.” _Statistics in Medicine, 38_, 2992-3012.\n" +
		"- Egger M, Smith GD, Schneider M, Minder C (1997). “Bias in meta-analysis detected by a simple, graphical test.” _British Medical Journal, 315_, 629-34.\n" +
		"- Evrenoglou T, White IR, Afach S, Mavridis D, Chaimani A (2022). “Network Meta-analysis of rare events using penalized likelihood regression.” _Statistics in Medicine, 41_, 5203-19.\n" +
		"- Higgins JPT, Jackson D, Barrett JK, Lu G, Ades AE, White IR (2012). “Consistency and inconsistency in network meta-analysis: concepts and models for multi-arm studies.” _Research Synthesis Methods, 3_, 98-110.\n" +
		"- Jackson D, White IR, Riley RD (2012). “Quantifying the impact of between-study heterogeneity in multivariate meta-analyses.” _Statistics in Medicine, 31_, 3805-20.\n" +
		"- König J, Krahn U, Binder H (2013). “Visualizing the flow of evidence in network meta-analysis and characterizing mixed treatment comparisons.” _Statistics in Medicine, 32_, 5414-29.\n" +
		"- Krahn U, Binder H, König J (2013). “A graphical tool for locating inconsistency in network meta-analyses.” _BMC Medical Research Methodology, 13_, 35.\n" +
		"- Kwarteng N, Evrenoglou T, Mueller J, Elsaesser M, Schramm E, Schwarzer G, Nikolakopoulou A (2026). “Illustrating the assumptions of meta-regression in treatment networks.” Preprint available at _Research Square_, doi:10.21203/rs.3.rs-8235913/v1.\n" +
		"- Mills EJ, Thorlund K, Ioannidis JP (2012). “Calculating additive treatment effects from multiple randomized trials provides useful estimates of combination therapies.” _Journal of Clinical Epidemiology, 65_, 1282-8.\n" +
		"- Papakonstantinou T, Nikolakopoulou A, Rücker G, Chaimani A, Schwarzer G, Egger M, Salanti G (2018). “Estimating the contribution of studies in network meta-analysis: paths, flows and streams.” _F1000Research_.\n" +
		"- Rücker G (2012). “Network meta-analysis, electrical networks and graph theory.” _Research Synthesis Methods, 3_, 312-24.\n" +
		"- Rücker G, Schwarzer G (2014). “Reduce dimension or reduce weights? Comparing two approaches to multi-arm studies in network meta-analysis.” _Statistics in Medicine, 33_, 4353-69.\n" +
		"- Rücker G, Schwarzer G (2015). “Ranking treatments in frequentist network meta-analysis works without resampling methods.” _BMC Medical Research Methodology, 15_, 58.\n" +
		"- Rücker G, Schwarzer G (2016). “Automated drawing of network plots in network meta-analysis.” _Research Synthesis Methods, 7_, 94-107.\n" +
		"- Rücker G, Petropoulou M, Schwarzer G (2020). “Network meta-analysis of multicomponent interventions.” _Biometrical Journal, 62_, 808-21.\n" +
		"- Rücker G, Nikolakopoulou A, Papakonstantinou T, Salanti G, Riley RD, Schwarzer G (2020). “The statistical importance of a study for a network meta-analysis estimate.” _BMC Medical Research Methodology, 20_, 190.\n" +
		"- Rücker G, Schmitz S, Schwarzer G (2021). “Component network meta-analysis compared to a matching method in a disconnected network: A case study.” _Biometrical Journal, 63_(2), 447-461. https://doi.org/10.1002/bimj.201900339\n" +
		"- Rücker G, Papakonstantinou T, Nikolakopoulou A, Schwarzer G, Galla T, Davies AL (2024). “Shortest path or random walks? A framework for path weights in network meta-analysis.” _Statistics in Medicine_.\n" +
		"- Salanti G, Ades AE, Ioannidis JP (2011). “Graphical methods and numerical summaries for presenting results from multiple-treatment meta-analysis: an overview and tutorial.” _Journal of Clinical Epidemiology, 64_, 163-71.\n" +
		"- Stijnen T, Hamza TH, Ozdemir P (2010). “Random effects meta-analysis of event outcome in the framework of the generalized linear mixed model with applications in sparse data.” _Statistics in Medicine, 29_, 3046-67.\n" +
		"- Thompson SG, Sharp SJ (1999). “Explaining heterogeneity in meta-analysis: a comparison of methods.” _Statistics in Medicine, 18_, 2693-708.\n" +
		"- Welton NJ, Caldwell DM, Adamopoulos E, Vedhara K (2009). “Mixed treatment comparison meta-analysis of complex interventions: psychological interventions in coronary heart disease.” _American Journal of Epidemiology, 169_, 1158-65.\n" +
		"## " + qsTr("R Packages") + "\n" +
		"- brglm2\n" +
		"- meta\n" +
		"- metafor\n" +
		"- netmeta"

	VariablesForm
	{
		// the available list is as long as the column of the controls on the right; the component indicators (a list with several
		// rows) get the space of about three variables
		preferredHeight:	{
			var controls	= [outcomeType, effectSizeMeasure, effectSizeVariable, effectSizeStandardErrorVariable, treatment1Variable, treatment2Variable,
				treatmentVariable, armEventsVariable, armMeanVariable, armStandardDeviationVariable, armSampleSizeVariable, armPersonTimeVariable,
				armEffectSizeVariable, armEffectSizeStandardErrorVariable, studyIdVariable, poolingMethod, method, fixedEffectTest, predictors, subgroup]
			var margin		= 8 * preferencesModel.uiScale
			var height		= componentVariables.visible ? 125 * preferencesModel.uiScale + margin : 0
			for (var i = 0; i < controls.length; i++)
				if (controls[i].visible)
					height += controls[i].height + margin
			return Math.max(height - margin, 250 * preferencesModel.uiScale)
		}
		removeInvisibles:	true

		AvailableVariablesList
		{
			name:				"allVariables"
		}

		// the arm-level outcome selects the variables below and the effect size measures (as the method of Mantel-Haenszel / Peto)
		DropDown
		{
			name:			"outcomeType"
			id:				outcomeType
			label:			qsTr("Outcome")
			startValue:		"general"
			visible:		dataFormat.value === "armBased"
			info:			qsTr("Type of the arm-level outcome data.")
			values:			[
				{ label: qsTr("Binary")			, value: "binary"			, info: qsTr("Number of events and sample size per arm.")},
				{ label: qsTr("Continuous")		, value: "continuous"		, info: qsTr("Mean, standard deviation, and sample size per arm.")},
				{ label: qsTr("Incidence rate")	, value: "incidenceRate"	, info: qsTr("Number of events and person-time at risk per arm.")},
				{ label: qsTr("General")		, value: "general"			, info: qsTr("Arm-level effect size and its standard error per arm.")}
			]
		}

		// the effect size measure of contrast-based data and general arm-level data uses the labels of Robust Bayesian Meta-Analysis
		DropDown
		{
			name:			"effectSizeMeasure"
			id:				effectSizeMeasure
			label:			qsTr("Effect size measure")
			startValue:		"general"
			info:			qsTr("Effect size measure of the treatment effects: supplied as the 'Effect Size' (contrast-based data) or computed from the arm-level data (arm-based data). The measure labels the netmeta model; the estimates of contrast-based and general arm-level data do not depend on it.")
			values: (function() {
				if (dataFormat.value === "armBased" && outcomeType.value === "binary") {
					return [
						{ label: qsTr("Log odds ratio")						, value: "OR"	, info: qsTr("Log odds ratio")},
						{ label: qsTr("Log risk ratio")						, value: "RR"	, info: qsTr("Log risk ratio")},
						{ label: qsTr("Risk difference")					, value: "RD"	, info: qsTr("Risk difference")},
						{ label: qsTr("Arcsine square root difference")		, value: "ASD"	, info: qsTr("Arcsine square root difference")}
					];
				} else if (dataFormat.value === "armBased" && outcomeType.value === "continuous") {
					return [
						{ label: qsTr("Mean difference")					, value: "MD"	, info: qsTr("Mean difference")},
						{ label: qsTr("Standardized mean difference")		, value: "SMD"	, info: qsTr("Standardized mean difference (Cohen's d as computed by meta::pairwise(), without a small-sample correction)")},
						{ label: qsTr("Log ratio of means")					, value: "ROM"	, info: qsTr("Log ratio of means")}
					];
				} else if (dataFormat.value === "armBased" && outcomeType.value === "incidenceRate") {
					return [
						{ label: qsTr("Log incidence rate ratio")			, value: "IRR"	, info: qsTr("Log incidence rate ratio")},
						{ label: qsTr("Incidence rate difference")			, value: "IRD"	, info: qsTr("Incidence rate difference")},
						{ label: qsTr("Square root incidence rate difference")	, value: "IRSD"	, info: qsTr("Square root incidence rate difference")}
					];
				} else {
					return [
						{ label: qsTr("Standardized mean difference")		, value: "SMD"		, info: qsTr("Standardized mean difference such as Cohen's d or Hedges' g")},
						{ label: qsTr("Fisher's z")							, value: "ZCOR"		, info: qsTr("Fisher's z transformation of correlation coefficients")},
						{ label: qsTr("Log odds ratio")						, value: "OR"		, info: qsTr("Log odds ratio")},
						{ label: qsTr("Log risk ratio")						, value: "RR"		, info: qsTr("Log risk ratio")},
						{ label: qsTr("Log hazard ratio")					, value: "HR"		, info: qsTr("Log hazard ratio")},
						{ label: qsTr("Risk difference")					, value: "RD"		, info: qsTr("Risk difference")},
						{ label: qsTr("General")							, value: "general"	, info: qsTr("General effect size that does not correspond to any of the measures above.")}
					];
				}})()
		}

		AssignedVariablesList
		{
			name:				"effectSize"
			id:				effectSizeVariable
			title:				qsTr("Effect Size")
			singleVariable:		true
			allowedColumns:		["scale"]
			visible:			dataFormat.value === "contrastBased"
			enabled:			dataFormat.value === "contrastBased"
			onEnabledChanged:	itemDoubleClicked(0)
			info:				qsTr("Variable containing the observed treatment effect of 'Treatment 1' versus 'Treatment 2', on the scale used to fit the model (e.g., log odds ratios for odds ratios).")
		}

		AssignedVariablesList
		{
			name:				"effectSizeStandardError"
			id:				effectSizeStandardErrorVariable
			title:				qsTr("Effect Size Standard Error")
			singleVariable:		true
			allowedColumns:		["scale"]
			visible:			dataFormat.value === "contrastBased"
			enabled:			dataFormat.value === "contrastBased"
			onEnabledChanged:	itemDoubleClicked(0)
			info:				qsTr("Variable containing the standard errors corresponding to the effect sizes.")
		}

		AssignedVariablesList
		{
			name:				"treatment1"
			id:					treatment1Variable
			title:				qsTr("Treatment 1")
			singleVariable:		true
			allowedColumns:		["nominal"]
			visible:			dataFormat.value === "contrastBased"
			enabled:			dataFormat.value === "contrastBased"
			onEnabledChanged:	itemDoubleClicked(0)
			info:				qsTr("Variable containing the first treatment of the comparison. The effect size is the effect of this treatment versus 'Treatment 2'.")
		}

		AssignedVariablesList
		{
			name:				"treatment2"
			id:					treatment2Variable
			title:				qsTr("Treatment 2")
			singleVariable:		true
			allowedColumns:		["nominal"]
			visible:			dataFormat.value === "contrastBased"
			enabled:			dataFormat.value === "contrastBased"
			onEnabledChanged:	itemDoubleClicked(0)
			info:				qsTr("Variable containing the second treatment of the comparison (the comparator).")
		}

		AssignedVariablesList
		{
			name:				"treatment"
			id:					treatmentVariable
			title:				qsTr("Treatment")
			singleVariable:		true
			allowedColumns:		["nominal"]
			visible:			dataFormat.value === "armBased"
			enabled:			dataFormat.value === "armBased"
			onEnabledChanged:	itemDoubleClicked(0)
			info:				qsTr("Variable containing the treatment received in the study arm.")
		}

		AssignedVariablesList
		{
			name:				"armEvents"
			id:				armEventsVariable
			title:				qsTr("Events")
			singleVariable:		true
			allowedColumns:		["scale"]
			visible:			dataFormat.value === "armBased" && (outcomeType.value === "binary" || outcomeType.value === "incidenceRate")
			enabled:			dataFormat.value === "armBased" && (outcomeType.value === "binary" || outcomeType.value === "incidenceRate")
			onEnabledChanged:	itemDoubleClicked(0)
			info:				qsTr("Variable containing the number of events in the study arm.")
		}

		AssignedVariablesList
		{
			name:				"armMean"
			id:				armMeanVariable
			title:				qsTr("Mean")
			singleVariable:		true
			allowedColumns:		["scale"]
			visible:			dataFormat.value === "armBased" && outcomeType.value === "continuous"
			enabled:			dataFormat.value === "armBased" && outcomeType.value === "continuous"
			onEnabledChanged:	itemDoubleClicked(0)
			info:				qsTr("Variable containing the mean outcome in the study arm.")
		}

		AssignedVariablesList
		{
			name:				"armStandardDeviation"
			id:				armStandardDeviationVariable
			title:				qsTr("Standard Deviation")
			singleVariable:		true
			allowedColumns:		["scale"]
			visible:			dataFormat.value === "armBased" && outcomeType.value === "continuous"
			enabled:			dataFormat.value === "armBased" && outcomeType.value === "continuous"
			onEnabledChanged:	itemDoubleClicked(0)
			info:				qsTr("Variable containing the standard deviation of the outcome in the study arm.")
		}

		AssignedVariablesList
		{
			name:				"armSampleSize"
			id:				armSampleSizeVariable
			title:				qsTr("Sample Size")
			singleVariable:		true
			allowedColumns:		["scale"]
			visible:			dataFormat.value === "armBased" && (outcomeType.value === "binary" || outcomeType.value === "continuous")
			enabled:			dataFormat.value === "armBased" && (outcomeType.value === "binary" || outcomeType.value === "continuous")
			onEnabledChanged:	itemDoubleClicked(0)
			info:				qsTr("Variable containing the number of participants in the study arm.")
		}

		AssignedVariablesList
		{
			name:				"armPersonTime"
			id:				armPersonTimeVariable
			title:				qsTr("Person-Time")
			singleVariable:		true
			allowedColumns:		["scale"]
			visible:			dataFormat.value === "armBased" && outcomeType.value === "incidenceRate"
			enabled:			dataFormat.value === "armBased" && outcomeType.value === "incidenceRate"
			onEnabledChanged:	itemDoubleClicked(0)
			info:				qsTr("Variable containing the person-time at risk in the study arm.")
		}

		AssignedVariablesList
		{
			name:				"armEffectSize"
			id:				armEffectSizeVariable
			title:				qsTr("Arm Effect Size")
			singleVariable:		true
			allowedColumns:		["scale"]
			visible:			dataFormat.value === "armBased" && outcomeType.value === "general"
			enabled:			dataFormat.value === "armBased" && outcomeType.value === "general"
			onEnabledChanged:	itemDoubleClicked(0)
			info:				qsTr("Variable containing the arm-level effect size (e.g., a log rate or a mean) in the study arm.")
		}

		AssignedVariablesList
		{
			name:				"armEffectSizeStandardError"
			id:				armEffectSizeStandardErrorVariable
			title:				qsTr("Arm Effect Size Standard Error")
			singleVariable:		true
			allowedColumns:		["scale"]
			visible:			dataFormat.value === "armBased" && outcomeType.value === "general"
			enabled:			dataFormat.value === "armBased" && outcomeType.value === "general"
			onEnabledChanged:	itemDoubleClicked(0)
			info:				qsTr("Variable containing the standard errors of the arm-level effect sizes.")
		}

		AssignedVariablesList
		{
			name:				"studyId"
			id:				studyIdVariable
			title:				qsTr("Study ID")
			singleVariable:		true
			allowedColumns:		["nominal"]
			info:				qsTr("Variable identifying the study. All rows (contrast-based data) or arms (arm-based data) with the same value belong to the same study; studies with more than two arms are handled as multi-arm studies.")
		}

		DropDown
		{
			name:			"poolingMethod"
			id:				poolingMethod
			label:			qsTr("Pooling method")
			startValue:		"inverseVariance"
			visible:		dataFormat.value === "armBased" && outcomeType.value === "binary" && effectSizeMeasure.value === "OR"
			info:			qsTr("Method used to pool the binary data. The inverse variance method pools the log odds ratios. The Mantel-Haenszel, non-central hypergeometric, and penalized logistic regression methods (suitable for sparse data) work with the event counts directly. Available for log odds ratios of arm-based binary data. The Mantel-Haenszel and non-central hypergeometric methods provide only common-effect (equal effects) results. Penalized logistic regression models random effects by a dispersion factor instead of a between-study variance. These methods require the arm-level event counts and provide no heterogeneity statistics and prediction intervals. Penalized logistic regression can take a while to fit (about 15 seconds per model).")
			values:			[
				{ label: qsTr("Inverse variance")				, value: "inverseVariance"				, info: qsTr("Inverse variance method applied to the pairwise log odds ratios.")},
				{ label: qsTr("Mantel-Haenszel")				, value: "mantelHaenszel"				, info: qsTr("Mantel-Haenszel method (common-effect only).")},
				{ label: qsTr("Non-central hypergeometric")		, value: "nonCentralHypergeometric"		, info: qsTr("Method based on the non-central hypergeometric distribution (common-effect only).")},
				{ label: qsTr("Penalized logistic regression")	, value: "penalizedLogisticRegression"	, info: qsTr("Penalized logistic regression (slow: about 15 seconds per model).")}
			]

			// the Mantel-Haenszel and the non-central hypergeometric methods provide the common-effect model only
			readonly property bool commonEffectOnly: dataFormat.value === "armBased" && outcomeType.value === "binary" && effectSizeMeasure.value === "OR" && (value === "mantelHaenszel" || value === "nonCentralHypergeometric")

			// penalized logistic regression models random effects by a dispersion factor instead of a between-study variance estimated by an estimator
			readonly property bool dispersionModel: dataFormat.value === "armBased" && outcomeType.value === "binary" && effectSizeMeasure.value === "OR" && value === "penalizedLogisticRegression"

			// these methods (and penalized logistic regression) provide no between-study variance, Q statistics, I^2, and prediction intervals
			readonly property bool noHeterogeneity: dataFormat.value === "armBased" && outcomeType.value === "binary" && effectSizeMeasure.value === "OR" && value !== "inverseVariance"

			// the count-based methods lack the model structures of the standard network meta-analysis: rank probabilities, design decomposition,
			// net heat plot, contribution matrix, component network meta-analysis, and network meta-regression are not available (R mirrors these conditions and builds none of the disabled outputs)
			readonly property bool countBased: dataFormat.value === "armBased" && outcomeType.value === "binary" && effectSizeMeasure.value === "OR" && value !== "inverseVariance"
		}

		MA.ClassicalMetaAnalysisMethod
		{
			id:					method
			analysisType:		"networkMetaAnalysis"
			commonEffectOnly:	poolingMethod.commonEffectOnly
			dispersionModel:	poolingMethod.dispersionModel
			componentModel:		networkModel.value === "component"
			fieldWidth:			125 * preferencesModel.uiScale
		}

		DropDown
		{
			name:			"fixedEffectTest"
			id:			fixedEffectTest
			label:			qsTr("Fixed effects test")
			startValue:		"z"
			enabled:		method.value !== "equalEffects" && !poolingMethod.noHeterogeneity && networkModel.value === "standard"
			values:			method.value === "equalEffects" || poolingMethod.noHeterogeneity || networkModel.value === "component" ? [ "z" ] : [ "z", "t" ]
			info:			qsTr("Method for testing the treatment effects and computing their confidence intervals: 'z' uses the standard normal approximation, 't' uses the t-distribution with the degrees of freedom of the overall Q statistic (for the confidence intervals, the p-values, and the P-scores). The t-distribution is available for the random-effects model of the standard model only.")
		}

		AssignedVariablesList
		{
			name:				"componentVariables"
			id:				componentVariables
			title:				qsTr("Component Indicators")
			allowedColumns:		["scale"]
			visible:			networkModel.value === "component" && componentSpecification.value === "indicatorVariables" && dataFormat.value === "armBased"
			enabled:			networkModel.value === "component" && componentSpecification.value === "indicatorVariables" && dataFormat.value === "armBased"
			// (a list with several variables: all of them are returned to the available variables when the list is disabled)
			onEnabledChanged:
			{
				if (!enabled)
				{
					while (count > 0)
						itemDoubleClicked(0)
				}
			}
			info:				qsTr("Variables containing 0/1 indicators of the components that form each treatment. The indicators must be constant within each treatment. An interaction between components is an additional indicator variable that is 1 for the treatments that contain all of its components. Available for arm-based data when the component model defines the components by indicator variables.")
		}

		AssignedVariablesList
		{
			name:				"predictor"
			id:					predictors
			title:				qsTr("Predictor")
			singleVariable:		true
			allowedColumns:		["nominal", "scale"]
			allowTypeChange:	true
			visible:			networkModel.value === "standard"
			enabled:			networkModel.value === "standard"
			onEnabledChanged:	itemDoubleClicked(0)
			info:				qsTr("Study-level variable to include as a predictor (moderator) in the network meta-regression. The predictor must be constant within each study and can be continuous or have two levels. Continuous predictors are not centered; centering (e.g., years since 2000) can help the meta-regression converge. The moderation test is the Wald test of the slope coefficients of the predictor; the residual heterogeneity test is the test of the heterogeneity not explained by the treatment effects and the predictor. The network meta-regression is available for the standard model only (the input is hidden for the component model and the variable is returned to the available variables), and not for the Mantel-Haenszel, non-central hypergeometric, and penalized logistic regression pooling methods.")
		}

		AssignedVariablesList
		{
			name:				"subgroup"
			id:					subgroup
			title:				qsTr("Subgroup")
			singleVariable:		true
			allowedColumns:		["nominal"]
			info:				qsTr("Study-level variable indicating subgroup stratification. For each subgroup, an independent network meta-analysis is fitted to the corresponding data set subset, and the network estimates of the subgroups are compared by a test for subgroup differences: for each comparison estimated in at least two subgroups, Cochran's Q test of the difference between the network estimates of the subgroups, with inverse-variance weights. The subgroup must be constant within each study.")
		}
	}

	Group
	{
		RadioButtonGroup
		{
			name:					"dataFormat"
			id:						dataFormat
			title:					qsTr("Data format")
			radioButtonsOnSameRow:	true
			info:					qsTr("Specify whether the data contain one row per treatment comparison (contrast-based) or one row per study arm (arm-based).")

			RadioButton
			{
				value:		"contrastBased"
				label:		qsTr("Contrast-based")
				checked:	true
				info:		qsTr("Each row contains a treatment effect (with its standard error) of 'Treatment 1' versus 'Treatment 2'. A multi-arm study must be specified by all of its pairwise comparisons (three rows for a three-arm study, six rows for a four-arm study).")
			}

			RadioButton
			{
				value:		"armBased"
				label:		qsTr("Arm-based")
				info:		qsTr("Each row contains the data of one treatment arm of a study. The treatment effects are computed from the arm-level data.")
			}
		}

		CheckBox
		{
			name:		"inputSummary"
			label:		qsTr("Input summary")
			checked:	false
			info:		qsTr("Include a table summarizing the analysed network: the numbers of studies, pairwise comparisons, treatments, designs, and multi-arm studies, and, for arm-based data, the number of observations or the person-time.")
		}

		DropDown
		{
			name:			"referenceTreatment"
			label:			qsTr("Reference treatment")
			source:			dataFormat.value === "armBased" ? [{ name: "treatment", use: "levels" }] : [{ name: "treatment1", use: "levels" }, { name: "treatment2", use: "levels" }]
			info:			qsTr("Treatment used as the reference for the treatment effects (versus reference), the forest plot, the default treatment order of the funnel plot, and the effects and slopes of the network meta-regression. If the reference treatment is not part of the analysed network (e.g., in a subgroup), the first treatment of the network (the default reference treatment of netmeta) is used; the component model of a disconnected network uses the inactive treatment if it is part of the network.")
		}

		DropDown
		{
			name:		"beneficialEffect"
			label:		qsTr("Beneficial effect")
			startValue:	"negative"
			info:		qsTr("Direction of the treatment effects that indicates a benefit of the treatment. It determines the treatment ranking (P-score, SUCRA, probability of being best, mean and median rank, and rankogram) and the order by ranking of the league table and the forest plot. For ratio measures (e.g., odds ratios), negative effects are log ratios below zero (ratios below one).")
			values:		[
				{ label: qsTr("Negative")	, value: "negative"	, info: qsTr("Treatment effects below zero are beneficial (e.g., fewer adverse events or lower symptom scores).")},
				{ label: qsTr("Positive")	, value: "positive"	, info: qsTr("Treatment effects above zero are beneficial (e.g., more recoveries or higher quality of life).")}
			]
		}
	}

	RadioButtonGroup
	{
		name:		"model"
		id:			networkModel
		title:		qsTr("Model")
		info:		qsTr("Model of the treatment effects. The standard model estimates a separate effect for each treatment. The component model (additive component network meta-analysis) estimates the effects of the components that form the treatments (e.g., the parts of combination therapies) and models each treatment effect as the sum of the effects of its components.")

		RadioButton
		{
			value:		"standard"
			id:			standardModel
			label:		qsTr("Standard")
			checked:	true
			info:		qsTr("Standard network meta-analysis: a separate effect for each treatment. The Q statistics of the network meta-analytic tests (total, within designs, and between designs) are based on the common-effect model, also for random-effects analyses; the design-by-treatment interaction model is a random-effects model.")

			DropDown
			{
				name:		"metaregressionAssumption"
				label:		qsTr("Slopes")
				startValue:	"independent"
				enabled:	predictors.count > 0 && !poolingMethod.countBased
				info:		qsTr("Assumption about the effect of the predictor on the treatment effects (versus the reference treatment) in the network meta-regression, available when a predictor is included. 'Independent' estimates a separate slope for each treatment, 'Common' a single slope shared by all treatments. The slopes are not transformed; their standard errors are shown under an effect size transformation only if 'Standard errors' is checked.")
				values:		[
					{ label: qsTr("Independent")	, value: "independent"	, info: qsTr("Separate slope for each treatment versus the reference treatment.")},
					{ label: qsTr("Common")			, value: "common"		, info: qsTr("Common slope for all treatments versus the reference treatment.")}
				]
			}
		}

		RadioButton
		{
			value:		"component"
			label:		qsTr("Component")
			enabled:	!poolingMethod.countBased
			// the model switches to Standard when a count-based pooling method is selected (the component settings stay in their controls)
			onEnabledChanged:	if (!enabled && checked) standardModel.click()
			info:		qsTr("Additive component network meta-analysis: each treatment effect is the sum of the effects of its components (plus the checked interactions). The Q statistics of the additive (component) model and the standard model are based on the common-effect model; their difference tests whether the standard model, which does not restrict the treatment effects to the sums of the effects of their components, fits better than the additive model. The component model can also analyze disconnected networks whose subnetworks share components. It uses a moment estimator of the between-study variance and normal-theory inference; the inconsistency, small-study effects, and evidence structure analyses, the network meta-regression, the rank probabilities, and the prediction intervals are not available. Not available for the Mantel-Haenszel, non-central hypergeometric, and penalized logistic regression pooling methods (selecting one of them switches the model to Standard).")

			Group
			{
				columns:	2

				DropDown
				{
					name:		"componentSpecification"
					id:			componentSpecification
					label:		qsTr("Components")
					startValue:	"separator"
					info:		qsTr("Specify how the components of the treatments are defined.")
					values:		dataFormat.value === "armBased" ? [
						{ label: qsTr("Split names")			, value: "separator"			, info: qsTr("The components are the parts of the treatment names split at the separator (e.g., 'A+B' consists of the components 'A' and 'B').")},
						{ label: qsTr("Indicators")				, value: "indicatorVariables"	, info: qsTr("The components are defined by the 0/1 indicator variables assigned in 'Component Indicators'. The treatment with all indicators equal to zero is the inactive treatment.")}
					] : [
						{ label: qsTr("Split names")			, value: "separator"			, info: qsTr("The components are the parts of the treatment names split at the separator (e.g., 'A+B' consists of the components 'A' and 'B').")}
					]
				}

				TextField
				{
					name:			"componentSeparator"
					id:				componentSeparator
					label:			""
					value:			"+"
					fieldWidth:		30 * preferencesModel.uiScale
					visible:		componentSpecification.value === "separator"
					info:			qsTr("Character separating the components in the treatment names (a single character).")
				}
			}

			DropDown
			{
				name:				"componentInactiveTreatment"
				label:				qsTr("Inactive treatment")
				addEmptyValue:		true
				placeholderText:	qsTr("None")
				// (the indicators define the inactive treatment: the treatment with all indicators equal to zero)
				visible:			componentSpecification.value === "separator"
				// the treatments that consist of a single component: the treatment names without the separator
				values:				{
					var levels		= dataFormat.value === "armBased" ? treatmentVariable.levels : treatment1Variable.levels.concat(treatment2Variable.levels)
					var separator	= componentSeparator.value
					var single		= []
					for (var i = 0; i < levels.length; i++)
						if (single.indexOf(levels[i]) < 0 && (separator === "" || levels[i].indexOf(separator) < 0))
							single.push(levels[i])
					return single.sort()
				}
				info:				qsTr("Treatment without active components (e.g., placebo), available if the components are defined by splitting the treatment names. Its effect is fixed to zero and it is not treated as a component. The list shows the treatments whose names do not contain the separator.")
			}

			ComponentsList
			{
				name:		"componentInteractions"
				title:		qsTr("Interactions")
				// the treatments that combine components: the treatment names containing the separator (the levels come from the assigned
				// treatment variables); with indicators, an interaction is an additional indicator variable (no list)
				values:		{
					if (componentSpecification.value !== "separator")
						return []
					var levels		= dataFormat.value === "armBased" ? treatmentVariable.levels : treatment1Variable.levels.concat(treatment2Variable.levels)
					var separator	= componentSeparator.value
					var combinations = []
					for (var i = 0; i < levels.length; i++)
						if (combinations.indexOf(levels[i]) < 0 && separator !== "" && levels[i].indexOf(separator) >= 0)
							combinations.push(levels[i])
					return combinations.sort()
				}
				visible:			count > 0
				addItemManually:	false
				info:		qsTr("Treatment combinations whose effect may differ from the sum of the effects of their components (synergy or antagonism): an interaction between the components of each checked combination is added to the additive model. The list shows the treatments whose names contain the separator. With indicators, an interaction is specified by an additional indicator variable (1 for the treatments that contain all of its components).")

				rowComponent: CheckBox
				{
					name:		"include"
					label:		rowValue
					checked:	false
				}
			}
		}
	}

	MA.ClassicalMetaAnalysisStatistics
	{
		id:					sectionStatistics
		analysisType:		"networkMetaAnalysis"
		noHeterogeneity:	poolingMethod.noHeterogeneity
		componentModel:		networkModel.value === "component"

		// (the ranking is a group of the statistics of the network meta-analysis)
		MA.NetworkMetaAnalysisRanking {}
	}

	MA.NetworkMetaAnalysisForestPlot {}

	MA.NetworkMetaAnalysisNetworkPlot {}

	MA.NetworkMetaAnalysisInconsistency
	{
		id:		sectionInconsistency
	}

	MA.NetworkMetaAnalysisSmallStudyEffects {}

	MA.NetworkMetaAnalysisEvidence {}

	MA.NetworkMetaAnalysisAdvanced {}
}
