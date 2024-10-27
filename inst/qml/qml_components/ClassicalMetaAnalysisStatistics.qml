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
	title:						qsTr("Statistics")
	columns: 					2
	property string module:		"metaAnalysis"

	Group
	{
		title:		qsTr("Heterogeneity")
		columns:	2
		enabled:	method.value != "fixedEffects" && method.value != "equalEffects"
		visible:	module == "metaAnalysis"

		CheckBox
		{
			text:		qsTr("𝜏")
			name:		"heterogeneityTau"
			checked:	true
		}

		CheckBox
		{
			text:		qsTr("𝜏²")
			name:		"heterogeneityTau2"
			checked:	true
		}

		CheckBox
		{
			text:		qsTr("I²")
			name:		"heterogeneityI2"
			checked:	false
		}

		CheckBox
		{
			text:		qsTr("H²")
			name:		"heterogeneityH2"
			checked:	false
		}
	}

	Group
	{
		title:		qsTr("Random Effects / Model Structure")
		visible:	module == "metaAnalysisMultilevelMultivariate"

		CheckBox
		{
			text:		qsTr("Test inclusion")
			name:		"randomEffectsTestInclusion"
			checked:	false
		}
		/* TODO: will require a lot of work in sorting out which value belongs where
		CheckBox
		{
			text:		qsTr("Confidence intervals")
			name:		"randomEffectsConfidenceIntervals"
			checked:	false
		}
		*/	
	}

	Group
	{
		title:		qsTr("Meta-Regression")
		enabled:	predictors.count > 0

		CheckBox
		{
			name:		"metaregressionTermTests"
			text:		qsTr("Term tests")
			checked:	true
		}

		CheckBox
		{
			name:		"metaregressionCoefficientEstimates"
			text:		qsTr("Coefficient estimates")
			checked:	true
		}

		CheckBox
		{
			name:		"metaregressionCoefficientCorrelationMatrix"
			text:		qsTr("Coefficient correlation matrix")
			checked:	false
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
		}

		DropDown
		{//TODO: make shorter or across both rows?
			name:			"transformEffectSize"
			label:			qsTr("Transform effect size")
			setLabelAbove:	true
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
	}

}