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
	info: qsTr("Options for summarizing the meta-analytic results.")

	Group
	{
		title:		qsTr("Heterogeneity")
		columns:	2
		enabled:	method.value != "fixedEffects" && method.value != "equalEffects"
		visible:	analysisType === "metaAnalysis"
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
		title:		qsTr("Random Effects / Model Components")
		visible:	analysisType === "metaAnalysisMultilevelMultivariate"
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
			info: qsTr("Include prediction intervals in the tabular output.")
		}

		DropDown
		{//TODO: make shorter or across both rows?
			name:			"transformEffectSize"
			label:			qsTr("Transform effect size")
			setLabelAbove:	true
			info: qsTr("Select a transformation to apply to the effect size estimates in the output. This transformation applies to the 'Meta-Analytic Estimates Table', 'Estimated Marginal Means Table', 'Forest Plot', and  the 'Bubble Plot'. The 'Meta-Regression Coeffient Estimates' are not transformed.")
			values:			[
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
					{ label: qsTr("SMD to CLES, Pr(supperiority)")		, value: "smdToCles"					},  // transf.dtocles
				]
		}
	}

	CheckBox
	{
		name:		"fitMeasures"
		text:		qsTr("Fit measures")
		info: qsTr("Include fit statistics for the model, such as AIC and BIC.")
	}

}
