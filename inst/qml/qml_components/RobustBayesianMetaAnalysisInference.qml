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
	title:						qsTr("Statistics")
	columns: 					2
	info: qsTr("Options for summarizing the meta-analytic results.")

	property string analysisType: "RoBMA"
	// RoBMA: Robust Bayesian Meta-Analsis
	// BiBMA: Binomial Bayesian Meta-Analysis
	// NoBMA: Normal Bayesian Meta-Analysis

	Group
	{
		title:		qsTr("Heterogeneity")
		columns:	2
		info: qsTr("Summarize the meta-analytic between-study heterogeneity. Unvailable when performing multilevel/multivariate meta-analysis.")

		CheckBox
		{
			text:		qsTr("𝜏")
			name:		"heterogeneityTau"
			checked:	true
			info: qsTr("Include 𝜏, the square root of the estimated between-study variance.")
		}

		CheckBox
		{
			text:		qsTr("𝜏²")
			name:		"heterogeneityTau2"
			checked:	true
			info: qsTr("Include 𝜏², the estimated between-study variance.")
		}

		CheckBox
		{
			text:		qsTr("I²")
			name:		"heterogeneityI2"
			checked:	false
			info: qsTr("Include I², the percentage of total variation across studies due to heterogeneity.")
		}

		CheckBox
		{
			text:		qsTr("H²")
			name:		"heterogeneityH2"
			checked:	false
			info: qsTr("Include H², an index indicating the ratio of total variability to sampling variability.")
		}
	}

	Group
	{
		title:		qsTr("Meta-Regression")
		enabled:	predictors.count > 0
		info: qsTr("Create summaries of the meta-regression. Available when predictors are included.")

		CheckBox
		{
			name:		"metaregressionTermTests"
			enabled:	bayesianModelAveragingModerations.checked
			text:		qsTr("Term tests")
			checked:	true
			info: qsTr("Include tests for each term in the meta-regression model. The null hypothesis states that the effect size at all levels of the categorical variable are equal or that there is no linear association between the effect size and the continuous variable).")
		}

		CheckBox
		{
			name:		"metaregressionCoefficientEstimates"
			text:		qsTr("Coefficient estimates")
			checked:	true
			info: qsTr("Include estimates of the regression coefficients in the meta-regression model. Note that the regression coefficients are not standardized and correspond to the scale of the prior distribution (the RoBMA R package provides only standardized regression coefficients).")
		}

		CheckBox
		{
			name:		"metaregressionStandardizedCoefficientEstimates"
			text:		qsTr("Standardized coefficient estimates")
			checked:	false
			info: qsTr("Include estimates of the standardized regression coefficients in the meta-regression model. The standardized regression coefficients are the regression coefficients divided by the standard deviation of the predictor variable and correspond to the scale of prior distribution (there is no difference between standardized and non-standardized regression coefficients for categorical moderators).")
		}
	}

	Group
	{
		CheckBox
		{
			// the name is 'confidenceIntervals' rather then 'credibleIntervals' for reusing the standard MA code`
			name:				"confidenceIntervals"
			text:				qsTr("Credible intervals")
			checked:			true
			childrenOnSameRow:	true
			info: qsTr("Include credible intervals in the tabular output.")

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
			info: qsTr("Include prediction intervals in the tabular output.")
		}

		CheckBox
		{
			label:		qsTr("Conditional estimates")
			name:		"conditionalEstimates"
			info: qsTr("Include conditional estimates, i.e., estimates assuming the presence of the corresponding model component/parameter, in the tabular output and figures.")
		}

		DropDown
		{
			name:			"transformEffectSize"
			label:			qsTr("Transform effect size")
			enabled:		effectSizeMeasure.value !== "RD"
			setLabelAbove:	true
			info: qsTr("Select a transformation to apply to the effect size estimates in the output. This transformation applies to the 'Meta-Analytic Estimates Table', 'Estimated Marginal Means Table', 'Forest Plot', and  the 'Bubble Plot'. The 'Meta-Regression Coeffient Estimates' are not transformed.")
			values:		
				if (analysisType === "BiBMA") // treat as: effectSizeMeasure.value === "logOR"
				[
					{ label: qsTr("None")								, value: "none"							},  // NULL
					{ label: qsTr("Exponential")						, value: "exponential"					},  // exp
					{ label: qsTr("Log odds to proportions")			, value: "logOddsToProportions"			},  // transf.logit
					{ label: qsTr("Log odds to SMD (normal)")			, value: "logOddsToSmdNormal"			},  // transf.lnortod.norm
					{ label: qsTr("Log odds to SMD (logistic)")			, value: "logOddsToSmdLogistic"			}   // transf.lnortod.logis
				]	
				else if (effectSizeMeasure.value === "SMD")
				[
					{ label: qsTr("None")								, value: "none"							},  // NULL				
					{ label: qsTr("SMD to log odds (normal)")			, value: "smdToLogOddsNormal"			},  // transf.dtolnor.norm
					{ label: qsTr("SMD to log odds (logistic)")			, value: "smdToLogOddsLogistic"			},  // transf.dtolnor.logis				
					{ label: qsTr("SMD to Cohen's U₁")					, value: "smdToCohensU1"				},  // transf.dtou1
					{ label: qsTr("SMD to Cohen's U₂")					, value: "smdToCohensU2"				},  // transf.dtou2
					{ label: qsTr("SMD to Cohen's U₃")					, value: "smdToCohensU3"				},  // transf.dtou3
					{ label: qsTr("SMD to CLES, Pr(supperiority)")		, value: "smdToCles"					}  // transf.dtocles
				]
				else if (effectSizeMeasure.value === "fishersZ")	
				[
					{ label: qsTr("Fisher's z to r")					, value: "fishersZToCorrelation"		},  // transf.ztor
					{ label: qsTr("None")								, value: "none"							},  // NULL				
					{ label: qsTr("Z to R²")							, value: "zToR2"						} 	// transf.ztor2
				]
				else if (effectSizeMeasure.value === "logOR")
				[
					{ label: qsTr("None")								, value: "none"							},  // NULL
					{ label: qsTr("Exponential")						, value: "exponential"					},  // exp
					{ label: qsTr("Log odds to proportions")			, value: "logOddsToProportions"			},  // transf.logit
					{ label: qsTr("Log odds to SMD (normal)")			, value: "logOddsToSmdNormal"			},  // transf.lnortod.norm
					{ label: qsTr("Log odds to SMD (logistic)")			, value: "logOddsToSmdLogistic"			}   // transf.lnortod.logis
				]
				else if (effectSizeMeasure.value === "logRR" || effectSizeMeasure.value === "logHR" )
				[
					{ label: qsTr("None")								, value: "none"							},  // NULL
					{ label: qsTr("Exponential")						, value: "exponential"					}  // exp
				]
				else if (effectSizeMeasure.value === "RD")
				[
					{ label: qsTr("None")								, value: "none"							}  // NULL
				]
				else
				[
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
					{ label: qsTr("SMD to CLES, Pr(supperiority)")		, value: "smdToCles"					}  // transf.dtocles
				]
		}
	}


	Group
	{
		title:		qsTr("Publication Bias Adjustment")
		enabled:	publicationBiasAdjustment.value != "none"
		visible:	analysisType === "RoBMA"
		info: qsTr("Create summaries of the publication bias adjustment. Available when publication bias adjustment is specified.")

		CheckBox
		{
			enabled:	publicationBiasAdjustment.value != "PP"
			name:		"publicationBiasAdjustmentWeightfunctionEstimates"
			text:		qsTr("Weight function estimates")
			checked:	false
			info: qsTr("Include estimates of the weight function parameters in the publication bias adjustment model.")
		}

		CheckBox
		{
			enabled:	publicationBiasAdjustment.value != "original"
			name:		"publicationBiasAdjustmentPetPeeseEstimates"
			text:		qsTr("PET-PEESE estimates")
			checked:	false
			info: qsTr("Include estimates of the PET-PEESE parameters in the publication bias adjustment model.")
		}
	}

	BayesFactorType {}
}

