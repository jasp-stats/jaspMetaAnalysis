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
	title:							qsTr("Statistics")
	columns: 						2
	property string analysisType:	"metaAnalysis"
	property bool multilevelMultivariate: analysisType === "multilevelMultivariateMetaAnalysis"
	property string transformEffectSizeValue: transformEffectSizeDropdown.value
	readonly property bool unrestrictedWeightedLeastSquares: method.value === "unrestrictedWeightedLeastSquares"
	readonly property bool networkMetaAnalysis: analysisType === "networkMetaAnalysis"
	// network meta-analysis pooling methods without heterogeneity statistics (Mantel-Haenszel, non-central hypergeometric, penalized logistic regression)
	property bool noHeterogeneity: false
	// network meta-analysis component model (component effects; no direct estimates and prediction intervals)
	property bool componentModel: false
	info: qsTr("Options for summarizing the meta-analytic results.")

	Group
	{
		title:		qsTr("Heterogeneity")
		columns:	2
		// I² of a network meta-analysis is based on the total Q statistic, which equal effects models provide as well
		enabled:	networkMetaAnalysis ? !noHeterogeneity : method.value != "fixedEffects" && method.value != "equalEffects"
		visible:	(analysisType === "metaAnalysis" || analysisType === "selectionModels" || analysisType === "petPeese" || analysisType === "waapWls" || analysisType === "mantelHaenszelPeto" || analysisType === "generalizedMetaAnalysis" || networkMetaAnalysis) && !multilevelMultivariate
		info: qsTr("Summarize the meta-analytic between-study heterogeneity. Unavailable when performing multilevel/multivariate meta-analysis.")

		CheckBox
		{
			text:		qsTr("𝜏")
			name:		"heterogeneityTau"
			checked:	true
			visible:	(analysisType === "metaAnalysis" || analysisType === "selectionModels" || analysisType === "petPeese" || analysisType === "waapWls" || analysisType === "generalizedMetaAnalysis" || networkMetaAnalysis) && !unrestrictedWeightedLeastSquares
			enabled:	!networkMetaAnalysis || method.value !== "equalEffects"
			info: qsTr("Include 𝜏, the square root of the estimated between-study variance.")
		}

		CheckBox
		{
			text:		qsTr("𝜏²")
			name:		"heterogeneityTau2"
			checked:	true
			visible:	(analysisType === "metaAnalysis" || analysisType === "selectionModels" || analysisType === "petPeese" || analysisType === "waapWls" || analysisType === "generalizedMetaAnalysis" || networkMetaAnalysis) && !unrestrictedWeightedLeastSquares
			enabled:	!networkMetaAnalysis || method.value !== "equalEffects"
			info: qsTr("Include 𝜏², the estimated between-study variance.")
		}

		CheckBox
		{
			text:		qsTr("I²")
			name:		"heterogeneityI2"
			visible: analysisType !== "selectionModels" && !unrestrictedWeightedLeastSquares
			checked:	analysisType === "mantelHaenszelPeto" || networkMetaAnalysis
			info: networkMetaAnalysis ? qsTr("Include I², the percentage of total variation across studies due to heterogeneity within designs and inconsistency between designs (it is based on the total Q statistic of the network, or, for the component model, on the Q statistic of the additive model), with its confidence interval.") : qsTr("Include I², the percentage of total variation across studies due to heterogeneity.")
		}

		CheckBox
		{
			text:		qsTr("H²")
			name:		"heterogeneityH2"
			visible: analysisType !== "selectionModels" && !unrestrictedWeightedLeastSquares && !networkMetaAnalysis
			checked:	false
			info: qsTr("Include H², an index indicating the ratio of total variability to sampling variability.")
		}

		CheckBox
		{
			text:		qsTr("γ")
			name:		"heterogeneityGamma"
			checked:	true
			visible:	unrestrictedWeightedLeastSquares
			info: qsTr("Include γ, the multiplicative heterogeneity of the unrestricted weighted least squares model, i.e., the factor by which the sampling variances are multiplied.")
		}

		CheckBox
		{
			text:		qsTr("√γ")
			name:		"heterogeneitySqrtGamma"
			checked:	true
			visible:	unrestrictedWeightedLeastSquares
			info: qsTr("Include √γ, the square root of the multiplicative heterogeneity of the unrestricted weighted least squares model.")
		}
	}

	Group
	{
		title:		qsTr("Random Effects / Model Components")
		visible:	multilevelMultivariate
		info: qsTr("Available when performing multilevel/multivariate meta-analysis.")

		CheckBox
		{
			text:		qsTr("Confidence intervals")
			name:		"randomEffectsConfidenceIntervals"
			checked:	false
			info: qsTr("Include confidence intervals for the Random Effects / Model Components.")
		}
		
		CheckBox
		{
			text:		qsTr("Test inclusion")
			name:		"randomEffectsTestInclusion"
			checked:	false
			info: qsTr("Test the inclusion of the individual Random Effects / Model Components. The test compares the complete model (i.e., including all components) with a model without one of the specified Random Effects / Model Structure components at a time.")
		}
	}

	Group
	{
		title:		qsTr("Meta-Regression")
		enabled:	predictors.count > 0
		visible:	analysisType === "metaAnalysis" || analysisType === "selectionModels" || analysisType === "petPeese" || analysisType === "waapWls" || analysisType === "multilevelMultivariateMetaAnalysis" || analysisType === "generalizedMetaAnalysis"
		info: qsTr("Create summaries of the meta-regression. Available when predictors are included.")

		CheckBox
		{
			name:		"metaregressionTermTests"
			text:		qsTr("Term tests")
			checked:	true
			info: qsTr("Include tests for each term in the meta-regression model. The null hypothesis states that the effect size at all levels of the categorical variable are equal or that there is no linear association between the effect size and the continuous variable).")
		}

		CheckBox
		{
			name:		"metaregressionCoefficientEstimates"
			text:		qsTr("Coefficient estimates")
			checked:	true
			info: qsTr("Include estimates of the regression coefficients in the meta-regression model.")
		}

		CheckBox
		{
			name:		"metaregressionCoefficientCorrelationMatrix"
			text:		qsTr("Coefficient correlation matrix")
			checked:	false
			info: qsTr("Include the correlation matrix of the regression coefficients.")
		}
	}

	Group
	{
		visible:	networkMetaAnalysis
		info: qsTr("Summarize the estimated relative treatment effects of the network.")

		CheckBox
		{
			name:		"treatmentEffects"
			text:		qsTr("Treatment effects")
			checked:	true
			info: qsTr("Include a table with the estimated relative treatment effects.")

			RadioButtonGroup
			{
				name:		"treatmentEffectsComparisons"
				info: qsTr("Select the comparisons shown in the treatment effects table.")

				RadioButton
				{
					value:		"versusReference"
					label:		qsTr("Versus reference")
					checked:	true
					info: qsTr("Show the effect of each treatment versus the reference treatment.")
				}

				RadioButton
				{
					value:		"allPairwise"
					label:		qsTr("All pairwise comparisons")
					info: qsTr("Show the effects of all pairs of treatments.")
				}
			}
		}

		CheckBox
		{
			name:		"leagueTable"
			text:		qsTr("League table")
			checked:	false
			info: qsTr("Include a league table with the network estimates (lower triangle, column versus row treatment) and the direct estimates (upper triangle, row versus column treatment).")

			DropDown
			{
				name:		"leagueTableOrder"
				label:		qsTr("Order")
				startValue:	"treatmentOrder"
				info: qsTr("Order of the treatments in the league table.")
				values:		[
					{ label: qsTr("Treatment order")		, value: "treatmentOrder"	, info: qsTr("Order of the treatments in the network.")},
					{ label: qsTr("Ranking (P-score)")		, value: "ranking"			, info: qsTr("Order by the P-score ranking of the treatments (the best treatment first, according to the 'Beneficial effect' setting below the data format).")}
				]
			}

			CheckBox
			{
				name:		"leagueTableDirectEstimates"
				text:		qsTr("Direct estimates in upper triangle")
				checked:	true
				enabled:	!componentModel
				info: qsTr("Show the direct (pairwise) estimates in the upper triangle. Otherwise, the upper triangle shows the network estimates. Not available for the component model, which shows its estimates in both triangles.")
			}

			CheckBox
			{
				name:		"leagueTableConfidenceIntervals"
				text:		qsTr("Confidence intervals")
				checked:	true
				info: qsTr("Show confidence intervals next to the estimates in the league table.")
			}
		}

		CheckBox
		{
			name:		"componentEffects"
			text:		qsTr("Component effects")
			checked:	true
			enabled:	componentModel
			info: qsTr("Include a table with the estimated effects of the components (relative to the inactive treatment if one is specified). Available for the component model.")
		}
	}

	Group
	{

		CheckBox
		{
			text:		qsTr("Standard errors")
			name:		"standardErrors"
			checked:	true
			enabled:	transformEffectSizeDropdown.value === "none"
			info: networkMetaAnalysis ? qsTr("Include standard errors in the tabular output. Only available when no effect size transformation is applied (the standard errors of the network meta-regression slopes, which are not transformed, are shown under a transformation only if this option is checked).") : qsTr("Include standard errors in the tabular output. Only available when no effect size transformation is applied.")
		}

		CheckBox
		{
			name:				"confidenceIntervals"
			text:				qsTr("Confidence intervals")
			checked:			true
			childrenOnSameRow:	true
			info: qsTr("Include confidence intervals in the tabular output.")

			CIField
			{
				name:		"confidenceIntervalsLevel"
			}
		}

		CheckBox
		{
			text:		qsTr("Prediction intervals")
			name:		"predictionIntervals"
			checked:	true
			visible:	analysisType === "metaAnalysis" || analysisType === "selectionModels" || analysisType === "petPeese" || analysisType === "waapWls" || analysisType === "multilevelMultivariateMetaAnalysis" || analysisType === "generalizedMetaAnalysis" || networkMetaAnalysis
			enabled:	networkMetaAnalysis ? method.value !== "equalEffects" && !noHeterogeneity && !componentModel : !unrestrictedWeightedLeastSquares
			info: networkMetaAnalysis ? qsTr("Include prediction intervals in the tabular output, based on the t-distribution with the degrees of freedom of the overall Q statistic. Only available for random-effects models of the standard model (not for the component model and the Mantel-Haenszel, non-central hypergeometric, and penalized logistic regression pooling methods).") : qsTr("Include prediction intervals in the tabular output. Unavailable for unrestricted weighted least squares.")
		}

		DropDown
		{
			id:				transformEffectSizeDropdown
			name:			"transformEffectSize"
			label:			qsTr("Transform effect size")
			setLabelAbove:	true
			fieldWidth:		125 * preferencesModel.uiScale
			info: networkMetaAnalysis ? qsTr("Select a transformation to apply to the treatment effect estimates in the output. This transformation applies to the estimates and intervals of the treatment effects, league table, direct and indirect evidence, network meta-regression, component effects, the forest plots, and the axis of the funnel plot. The heterogeneity statistics, the rankings, the tests, and the slopes of the network meta-regression are not transformed. The exponential transformation turns the differences between the direct and indirect estimates into ratios.") : qsTr("Select a transformation to apply to the effect size estimates in the output. This transformation applies to the 'Meta-Analytic Estimates Table', 'Estimated Marginal Means Table', 'Forest Plot', and  the 'Bubble Plot'. The 'Meta-Regression Coeffient Estimates' are not transformed.")
			values:			(function() {
				if (analysisType === "metaAnalysis" || analysisType === "selectionModels" || analysisType === "petPeese" || analysisType === "waapWls" || analysisType === "multilevelMultivariateMetaAnalysis") {
					return [
						{ label: qsTr("None")								, value: "none"							},  // NULL
						{ label: qsTr("Fisher's z to r")					, value: "fishersZToCorrelation"		},  // transf.ztor
						{ label: qsTr("Exponential")						, value: "exponential"					},  // exp
						{ label: qsTr("Log odds to proportions")			, value: "logOddsToProportions"			},  // transf.logit
						{ label: qsTr("Log odds to SMD (normal)")			, value: "logOddsToSmdNormal"			},  // transf.lnortod.norm
						{ label: qsTr("Log odds to SMD (logistic)")			, value: "logOddsToSmdLogistic"			},  // transf.lnortod.logis
						{ label: qsTr("SMD to log odds (normal)")			, value: "smdToLogOddsNormal"			},  // transf.dtolnor.norm
						{ label: qsTr("SMD to log odds (logistic)")			, value: "smdToLogOddsLogistic"			},  // transf.dtolnor.logis
						{ label: qsTr("Hakstian & Whalen inverse α")		, value: "hakstianAndWhalenInverseAlpha"},  // transf.iahw 
						{ label: qsTr("Bonett inverse α")					, value: "bonettInverseAlpha"			},  // transf.iabt
						{ label: qsTr("Z to R²")							, value: "zToR2"						}, 	// transf.ztor2
						{ label: qsTr("SMD to Cohen's U₁")					, value: "smdToCohensU1"				},  // transf.dtou1
						{ label: qsTr("SMD to Cohen's U₂")					, value: "smdToCohensU2"				},  // transf.dtou2
						{ label: qsTr("SMD to Cohen's U₃")					, value: "smdToCohensU3"				},  // transf.dtou3
						{ label: qsTr("SMD to CLES, Pr(superiority)")		, value: "smdToCles"					},  // transf.dtocles
					];
				} else if (analysisType === "mantelHaenszelPeto" || analysisType === "generalizedMetaAnalysis"){
					return [
						{ label: qsTr("None")								, value: "none"							},  // NULL
						{ label: qsTr("Exponential")						, value: "exponential"					},  // exp
						{ label: qsTr("Log odds to proportions")			, value: "logOddsToProportions"			},  // transf.logit
						{ label: qsTr("Log odds to SMD (normal)")			, value: "logOddsToSmdNormal"			},  // transf.lnortod.norm
						{ label: qsTr("Log odds to SMD (logistic)")			, value: "logOddsToSmdLogistic"			},  // transf.lnortod.logis
					];
				} else if (networkMetaAnalysis) {
					// transformations of relative treatment effects (contrasts) only
					return [
						{ label: qsTr("None")								, value: "none"							},  // NULL
						{ label: qsTr("Exponential")						, value: "exponential"					},  // exp
						{ label: qsTr("Log odds to SMD (normal)")			, value: "logOddsToSmdNormal"			},  // transf.lnortod.norm
						{ label: qsTr("Log odds to SMD (logistic)")			, value: "logOddsToSmdLogistic"			},  // transf.lnortod.logis
						{ label: qsTr("SMD to log odds (normal)")			, value: "smdToLogOddsNormal"			},  // transf.dtolnor.norm
						{ label: qsTr("SMD to log odds (logistic)")			, value: "smdToLogOddsLogistic"			}   // transf.dtolnor.logis
					];
				}})()
		}
	}

	Group
	{
		title:		qsTr("PET-PEESE")
		visible:	analysisType === "petPeese"

		CheckBox
		{
			name:		"petPeeseCoefficientEstimates"
			text:		qsTr("Coefficient estimates")
			checked:	true
			info: qsTr("Include the estimate of the bias coefficient: the standard error (PET) or the sampling variance (PEESE) coefficient of the reported model.")
		}

		CheckBox
		{
			name:		"petPeeseRegressionPlot"
			text:		qsTr("Regression plot")
			checked:	false
			info: qsTr("Visualize the fitted relationship between the effect sizes and their standard errors. The fitted line (PET) or curve (PEESE) at a standard error of zero corresponds to the adjusted estimate.")
		}
	}

	Group
	{
		title: qsTr("Weight Function")
		visible: analysisType === "selectionModels"
		CheckBox
		{
			name: "weightFunctionCoefficientEstimates"
			label: qsTr("Coefficient estimates")
			checked: true
			info: qsTr("Include weight function estimates in the output.")
		}

		CheckBox
		{
			name: "weightFunctionPValueFrequencyTable"
			label: qsTr("P-value frequency table")
			info: qsTr("Estimate counts within the specified p-value intervals.")
		}

		CheckBox
		{
			name: "weightFunctionPlot"
			label: qsTr("Plot")
			info: qsTr("Visualize the relative probability of study being published across the range of p-values.")

			CheckBox
			{
				name: "weightFunctionPlotRescaleXAxis"
				label: qsTr("Rescale x-axis")
				checked: true
				info: qsTr("Space p-value thresholds equally along the x-axis.")
			}
		}
	}

	CheckBox
	{
		name:		"fitMeasures"
		text:		qsTr("Fit measures")
		visible:	!networkMetaAnalysis
		info: 		qsTr("Include fit statistics for the model, such as AIC and BIC.")
	}

}
