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
import "../qml/qml_components" as MA

Form
{
	info: qsTr("Generalized (GLMM) meta-analysis fits meta-analytic models via generalized linear (mixed-effects) models using metafor's rma.glmm function. It directly models event counts and proportions without requiring pre-computed effect sizes, making it suitable for sparse binary and count data where the normal approximation breaks down.")
	infoBottom: "## " + qsTr("References") + "\n" +

	"- Stijnen T, Hamza TH, & Ozdemir P (2010). “Random effects meta-analysis of event outcome in the framework of the generalized linear mixed model with applications in sparse data.” _Statistics in Medicine, 29_(29), 3046–3067. ⁠https://doi.org/10.1002/sim.4040\n", +
    "- Jackson D, Law M, Stijnen T, Viechtbauer W, & White IR (2018). “A comparison of seven random-effects models for meta-analyses that estimate the summary odds ratio.” _Statistics in Medicine, 37_(7), 1059–1085. https://doi.org/10.1002/sim.7588⁠\n", +
	"- Viechtbauer W (2010). “Conducting meta-analyses in R with the metafor package.” _Journal of Statistical Software, 36_(3), 1-48. https://doi.org/10.18637/jss.v036.i03\n" +
	"## " + qsTr("R Packages") + "\n" +
	"- metafor"

	VariablesForm
	{
		preferredHeight: 600 * preferencesModel.uiScale
		removeInvisibles:	true

		AvailableVariablesList
		{
			name:				"allVariables"
		}

		// events inputs (OR, RR, RD, IRR)
		AssignedVariablesList
		{
			name:			"eventsGroup1"
			title:			qsTr("Events Group 1")
			singleVariable:	true
			allowedColumns:	["scale"]
		}

		AssignedVariablesList
		{
			name:			"eventsGroup2"
			title:			qsTr("Events Group 2")
			singleVariable:	true
			allowedColumns:	["scale"]
		}

		// sample size inputs (OR, RR, RD)
		AssignedVariablesList
		{
			name:			"sampleSizeGroup1"
			title:			qsTr("Sample Size Group 1")
			singleVariable:	true
			allowedColumns:	["scale"]
			visible:		effectSizeMeasure.value === "OR" || effectSizeMeasure.value === "RR" || effectSizeMeasure.value === "RD"
			enabled:		effectSizeMeasure.value === "OR" || effectSizeMeasure.value === "RR" || effectSizeMeasure.value === "RD"
			onEnabledChanged:	itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name:			"sampleSizeGroup2"
			title:			qsTr("Sample Size Group 2")
			singleVariable:	true
			allowedColumns:	["scale"]
			visible:		effectSizeMeasure.value === "OR" || effectSizeMeasure.value === "RR" || effectSizeMeasure.value === "RD"
			enabled:		effectSizeMeasure.value === "OR" || effectSizeMeasure.value === "RR" || effectSizeMeasure.value === "RD"
			onEnabledChanged:	itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name:			"personTimeGroup1"
			title:			qsTr("Person-Time Group 1")
			singleVariable:	true
			allowedColumns:	["scale"]
			visible:		effectSizeMeasure.value === "IRR"
			enabled:		effectSizeMeasure.value === "IRR"
			onEnabledChanged:	itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name:			"personTimeGroup2"
			title:			qsTr("Person-Time Group 2")
			singleVariable:	true
			allowedColumns:	["scale"]
			visible:		effectSizeMeasure.value === "IRR"
			enabled:		effectSizeMeasure.value === "IRR"
			onEnabledChanged:	itemDoubleClicked(0)
		}

		// effect size measure
		DropDown
		{
			id:				effectSizeMeasure
			name:			"effectSizeMeasure"
			label:			qsTr("Effect size measure")
			info: qsTr("Select the effect size measure to be used in the generalized meta-analysis.")
			values:			[
				{ label: qsTr("Log odds ratio"),			value: "OR"		},
				{ label: qsTr("Log risk ratio"),			value: "RR"		},
				{ label: qsTr("Risk difference"),			value: "RD"		},
				{ label: qsTr("Log incidence rate ratio"),	value: "IRR"	}
			]
		}

		// GLMM model type
		DropDown
		{
			name:			"glmmModel"
			id:				glmmModel
			label:			qsTr("Model type")
			info: qsTr("Select the generalized linear model type. UM.FS = unconditional with fixed study effects (default); UM.RS = unconditional with random study effects; CM.AL = conditional with approximate likelihood; CM.EL = conditional with exact likelihood (OR only, slowest but most accurate).")
			values: (function() {
				if (effectSizeMeasure.value === "OR") {
					return [
						{ label: qsTr("Unconditional (fixed study effects)"),		value: "UM.FS"	},
						{ label: qsTr("Unconditional (random study effects)"),		value: "UM.RS"	},
						{ label: qsTr("Conditional (approximate likelihood)"),		value: "CM.AL"	},
						{ label: qsTr("Conditional (exact likelihood)"),			value: "CM.EL"	}
					];
				} else {
					return [
						{ label: qsTr("Unconditional (fixed study effects)"),		value: "UM.FS"	},
						{ label: qsTr("Unconditional (random study effects)"),		value: "UM.RS"	},
						{ label: qsTr("Conditional (approximate likelihood)"),		value: "CM.AL"	}
					];
				}})()
		}

		DropDown
		{
			name:			"method"
			id:				method
			label:			qsTr("Method")
			startValue:		"maximumLikelihood"
			info: qsTr("Select the estimation method. Maximum Likelihood estimates a random-effects model; Equal Effects fixes tau-squared to zero.")
			values:			[
				{ label: qsTr("Maximum Likelihood"),	value: "maximumLikelihood"	},
				{ label: qsTr("Equal Effects"),			value: "equalEffects"		}
			]
		}

		DropDown
		{
			name:			"fixedEffectTest"
			label:			qsTr("Coefficient test")
			startValue:		"z"
			info: qsTr("Test type for the fixed-effect coefficients: z (Wald test) or t (t-distribution).")
			values:			[
				{ label: qsTr("z"),	value: "z"	},
				{ label: qsTr("t"),	value: "t"	}
			]
		}

		// predictors
		AssignedVariablesList
		{
			name:			"predictors"
			id:				predictors
			title:			qsTr("Predictors")
			allowedColumns:	["scale", "nominal"]
			info: qsTr("Variables to include as predictors (moderators) in a meta-regression model.")
		}

		AssignedVariablesList
		{
			name:				"studyLabels"
			title:				qsTr("Study Labels")
			singleVariable:		true
			allowedColumns:		["nominal"]
			info: qsTr("Variable containing labels for the studies.")
		}

		AssignedVariablesList
		{
			name:				"subgroup"
			id:					subgroup
			title:				qsTr("Subgroup")
			singleVariable:		true
			allowedColumns:		["nominal"]
			info: qsTr("Variable indicating subgroup stratification.")
		}
	}

	MA.ClassicalMetaAnalysisModel
	{
		id:				sectionModel
		analysisType:	"generalizedMetaAnalysis"
	}

	MA.ClassicalMetaAnalysisStatistics
	{
		id:				sectionStatistics
		analysisType:	"generalizedMetaAnalysis"
	}

	MA.ClassicalMetaAnalysisEstimatedMarginalMeans
	{
		analysisType:	"generalizedMetaAnalysis"
	}

	MA.ForestPlotSection
	{
		analysisType:				"generalizedMetaAnalysis"
		transformEffectSizeValue:	sectionStatistics.transformEffectSizeValue
	}

	MA.BubblePlot
	{
		analysisType:	"generalizedMetaAnalysis"
	}

	MA.ClassicalMetaAnalysisDiagnostics
	{
		analysisType:	"generalizedMetaAnalysis"
	}

	MA.ClassicalGeneralizedMetaAnalysisAdvanced {}
}
