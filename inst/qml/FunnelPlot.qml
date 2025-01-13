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

Form
{

	infoBottom: qsTr("Kossmeier, M., Tran, U. S., & Voracek, M. (2020). Power-enhanced funnel plots for meta-analysis. Zeitschrift für Psychologie, 228(1).")

	VariablesForm
	{
		preferredHeight: 200 * preferencesModel.uiScale

		AvailableVariablesList
		{
			name: "allVariables"
		}

		AssignedVariablesList
		{
			title:				qsTr("Effect Size")
			name:				"effectSize"
			singleVariable:		true
			allowedColumns:		["scale"]
			info: qsTr("Variable containing the observed effect sizes.")
		}

		AssignedVariablesList
		{
			name:				"effectSizeStandardError"
			title:				qsTr("Effect Size Standard Error")
			singleVariable:		true
			allowedColumns:		["scale"]
			info: qsTr("Variable containing the standard errors corresponding to the effect sizes.")
		}

		AssignedVariablesList
		{
			name:				"studyLabel"
			id:					studyLabel
			title:				qsTr("Study Label")
			singleVariable:		true
			allowedColumns:		["nominal"]
			info: qsTr("Variable containing labels for the studies. Used for labeling outputs and plots.")
		}

		AssignedVariablesList
		{
			name:				"split"
			title:				qsTr("Split")
			singleVariable:		true
			allowedColumns:		["nominal"]
			info: qsTr("Variable used to split the funnel plot into separate groups or categories.")
		}
	}

	CheckBox
	{
		name:		"funnelUnderH0"
		id:			funnelUnderH0
		label:		qsTr("Funnel under H₀")
		checked:	true
		info: qsTr("Specify the funnel plot under the null hypothesis.")

		Group
		{
			title:	qsTr("Parameters")

			DoubleField
			{
				text:				qsTr("μ")
				name: 				"funnelUnderH0ParametersFixedMu"
				defaultValue:		0
				negativeValues:		true
				info: qsTr("Fixed value for the mean effect size under the null hypothesis.")
			}

			DoubleField
			{
				text:				qsTr("𝜏")
				name: 				"funnelUnderH0ParametersFixedTau"
				defaultValue:		0
				min: 				0
				info: qsTr("Fixed value for 𝜏, representing the heterogeneity under the null hypothesis.")
			}
		}

		CheckBox
		{
			name:		"funnelUnderH0FillColors"
			label:		qsTr("Fill colors")
			checked:	true
			info: qsTr("Fill the funnel plot's prediction intervals under the null hypothesis with different colors.")
		}

		DropDown
		{
			name:		"funnelUnderH0LineType"
			label:		qsTr("Line type")
			info: qsTr("Set the type of line of the funnel plot's prediction intervals under the null hypothesis.")
			values:		[
				{ label: qsTr("None"),		value: "none"	},
				{ label: qsTr("Solid"),		value: "solid"	},
				{ label: qsTr("Dashed"),	value: "dashed"	},
				{ label: qsTr("Dotted"),	value: "dotted"	}
			]
		}
	}

	CheckBox
	{
		name:		"funnelUnderH1"
		id:			funnelUnderH1
		label:		qsTr("Funnel under H₁")
		checked:	false
		info: qsTr("Estimate or specify the funnel plot under the alternative hypothesis.")

		RadioButtonGroup
		{
			name:		"funnelUnderH1Parameters"
			title:		qsTr("Parameters")
			columns:	2
			radioButtonsOnSameRow: true
			info: qsTr("Choose whether to estimate or manually fix the parameters for the funnel plot under the alternative hypothesis.")

			RadioButton
			{
				value:	"estimated"
				label:	qsTr("Estimated")
				checked: true
				id:		estimated
			}

			RadioButton
			{
				value:	"fixed"
				label:	qsTr("Fixed")
				id:		fixed
			}
		}

		Group
		{
			visible:	estimated.checked

			DropDown
			{
				name:			"method"
				id:				method
				label:			qsTr("Method")
				startValue:		"restrictedML"
				info: qsTr("Select the heterogeneity estimation method for the funnel plot under the alternative hypothesis.")
				values:			[
					{ label: qsTr("Equal Effects")			, value: "equalEffects"		},
					{ label: qsTr("Fixed Effects")			, value: "fixedEffects"		},
					{ label: qsTr("Maximum Likelihood")		, value: "maximumLikelihood"},
					{ label: qsTr("Restricted ML")			, value: "restrictedML"		},
					{ label: qsTr("DerSimonian-Laird")		, value: "derSimonianLaird"	},
					{ label: qsTr("Hedges")					, value: "hedges"			},
					{ label: qsTr("Hunter-Schmidt")			, value: "hunterSchmidt"	},
					{ label: qsTr("Hunter-Schmidt (SSC)")	, value: "hunterSchmidtSsc"	},
					{ label: qsTr("Sidik-Jonkman")			, value: "sidikJonkman"		},
					{ label: qsTr("Empirical Bayes")		, value: "empiricalBayes"	},
					{ label: qsTr("Paule-Mandel")			, value: "pauleMandel"		},
					{ label: qsTr("Paule-Mandel (MU)")		, value: "pauleMandelMu"	}
				]
			}

			CheckBox
			{
				name:		"funnelUnderH1IncludeHeterogeneity"
				label:		qsTr("Include heterogeneity")
				enabled:	method.value != "fixedEffects" && method.value != "equalEffects"
				info: qsTr("Include heterogeneity (𝜏) in the funnel plot under the alternative hypothesis. If unselected, the heterogeneity estimate is not used to adjust the prediction intervals.") 
			}

			CheckBox
			{
				name:		"funnelUnderH1EstimatesTable"
				label:		qsTr("Estimates table")
				info: qsTr("Summarize the effect size and heterogeneity estimates used for the funnel plot under the alternative hypothesis in a table.")
			}
		}		

		Group
		{
			visible:	fixed.checked

			DoubleField
			{
				text:				qsTr("μ")
				name: 				"funnelUnderH1ParametersFixedMu"
				defaultValue:		0
				negativeValues:		true
				info: qsTr("Fixed value for the mean effect size under the alternative hypothesis.")
			}

			DoubleField
			{
				text:				qsTr("𝜏")
				name: 				"funnelUnderH1ParametersFixedTau"
				defaultValue:		0
				min: 				0
				info: qsTr("Fixed value for 𝜏, representing the heterogeneity under the alternative hypothesis.")
			}
		}

		CheckBox
		{
			name:		"funnelUnderH1FillColors"
			label:		qsTr("Fill colors")
			info: qsTr("Fill the funnel plot's prediction intervals under the alternative hypothesis with different colors.")
		}

		DropDown
		{
			name:		"funnelUnderH1LineType"
			label:		qsTr("Line type")
			startValue:	"solid"
			info: qsTr("Set the type of line of the funnel plot's prediction intervals under the alternative hypothesis.")
			values:		[
				{ label: qsTr("None"),		value: "none"	},
				{ label: qsTr("Solid"),		value: "solid"	},
				{ label: qsTr("Dashed"),	value: "dashed"	},
				{ label: qsTr("Dotted"),	value: "dotted"	}
			]
		}

		CheckBox
		{
			name:		"funnelUnderH1PowerEnhancement"
			enabled:	funnelUnderH1.checked
			label:		qsTr("Power enhancement")
			info: qsTr("Visualize power levels to detect the effect size under the alternative hypothesis with alpha = 0.05.")

			TextField
			{
				label: 				qsTr("Breaks")
				name: 				"funnelUnderH1PowerEnhancementBreaks"
				value:				"(0.15, 0.30, 0.50, 0.70, 0.90)"
				fieldWidth: 		200 * preferencesModel.uiScale
				info: qsTr("Specify the breakpoints for power enhancement. All levels must be within the 0.05 - 1 range.")
			}
		}
	}


	Group
	{
		title:	qsTr("Estimates Mapping")
		info: qsTr("Set mapping for labels, colors, and shapes of the effect size estimates.")

		DropDown
		{
			label:		qsTr("Label")
			name:		"estimatesMappingLabel"
			info: qsTr("Show all or only a subset of the effect size estimate labels.")
			enabled: 	studyLabel.count > 0
			values:		{
				if (funnelUnderH0.checked && funnelUnderH1.checked) {
					[
						{ label: qsTr("All"),			value: "all"		},
						{ label: qsTr("None"),			value: "none"		},
						{ label: qsTr("Outside H₀"),	value: "outsideH0"	},
						{ label: qsTr("Outside H₁"),	value: "outsideH1"	}
					]
				} else if (funnelUnderH0.checked) {
					[
						{ label: qsTr("All"),			value: "all"		},
						{ label: qsTr("None"),			value: "none"		},
						{ label: qsTr("Outside H₀"),	value: "outsideH0"	}
					]
				} else if (funnelUnderH1.checked) {
					[
						{ label: qsTr("All"),			value: "all"		},
						{ label: qsTr("None"),			value: "none"		},
						{ label: qsTr("Outside H₁"),	value: "outsideH1"	}
					]
				} else {
					[
						{ label: qsTr("All"),			value: "all"		},
						{ label: qsTr("None"),			value: "none"		}
					]
				}
			}
		}

		DropDown
		{
			name:			"estimatesMappingColor"
			id:				estimatesMappingColor
			label:			qsTr("Color")
			addEmptyValue:	true
			allowedColumns:	["nominal"]
			info: qsTr("Map colors of the effect size estimates in the funnel plot based on the selected variable.")
		}

		DropDown
		{
			name:			"estimatesMappingShape"
			id:				estimatesMappingShape
			label:			qsTr("Shape")
			addEmptyValue:	true
			allowedColumns:	["nominal"]
			info: qsTr("Map shapes of the effect size estimates in the funnel plot based on the selected variable.")

		}

		DropDown
		{
			name:			"estimatesLegendPosition"
			enabled:		estimatesMappingColor.checked || estimatesMappingShape.checked
			label:			qsTr("Legend position")
			startValue:		"right"
			info: qsTr("Set the legend position of the funnel plot. Available when color or shape mapping is enabled.")
			values:
			[
				{ label: qsTr("None"),			value: "none"},
				{ label: qsTr("Bottom"),		value: "bottom"},
				{ label: qsTr("Right"),			value: "right"},
				{ label: qsTr("Top"),			value: "top"},
				{ label: qsTr("Left"), 			value: "left"}
			]
		}

		DoubleField
		{
			name:			"estimatesMappingLabelOffset"
			label:			qsTr("Label offset")
			defaultValue:	0.10
			info: qsTr("Adjust the offset of labels in the funnel plot.")
		}
	}
	

	Group
	{
		TextField
		{
			label: 				qsTr("Funnel prediction interval")
			name: 				"funnelPredictionInterval"
			value:				"(0.90, 0.95, 0.99)"
			fieldWidth: 		120 * preferencesModel.uiScale
			info: qsTr("Specify the confidence levels for the funnel plot prediction intervals.")
		}

		CheckBox
		{
			name:		"invertColors"
			label:		qsTr("Invert colors")
			info: qsTr("Invert the colors used in the funnel plot.")
		}
	}

	CheckBox
	{
		name:		"funnelPlotAsymmetryTests"
		label:		qsTr("Funnel plot asymmetry tests")
		info: qsTr("Perform tests to detect asymmetry in the funnel plot indicating potential publication bias. The tests are performed with the 'Method' specified in the 'Funnel under H₁' option.")


		CheckBox
		{
			name:		"funnelPlotAsymmetryTestsMetaRegression"
			label:		qsTr("Meta-regression")
			checked:	true
			info: qsTr("Include meta-regression tests for funnel plot asymmetry.")
		}

		CheckBox
		{
			name:		"funnelPlotAsymmetryTestsWeightedRegression"
			label:		qsTr("Weighted regression")
			info: qsTr("Include weighted regression tests for funnel plot asymmetry.")

		}

		CheckBox
		{
			name:		"funnelPlotAsymmetryTestsRankCorrelation"
			label:		qsTr("Rank correlation")
			info: qsTr("Include rank correlation tests for funnel plot asymmetry.")
		}
	}
}
