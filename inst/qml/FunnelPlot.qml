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
				info: qsTr("Estimate the mean effect size and heterogeneity under the alternative hypothesis. The paramters are estimated with the 'Method' specified under 'General Settings'.")
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

	Divider { }

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

		ColorPalette {}
	}
	

	Group
	{
		title:	qsTr("General Settings")

		DropDown
		{
			name:			"method"
			id:				method
			enabled:		(funnelUnderH1.checked && estimated.checked) || (funnelPlotAsymmetryTests.checked && funnelPlotAsymmetryTestsMetaRegression.checked) || trimAndFill.checked || (failSafeNGeneral.checked && failSafeNGeneral.checked)
			label:			qsTr("Method")
			startValue:		"restrictedML"
			info: qsTr("Select the heterogeneity estimation method for the funnel plot under the alternative hypothesis, meta-regression funnel plot asymmetry tests, trim and fill, and the fail-safe N general method.")
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

	Section
	{
		title:	qsTr("Publication Bias / Sensitivity Analyses")
		info:	qsTr("Detect and perform sensitivity analyses to funel plot asymmetry, publication bias, and small-study effects.")

		CheckBox
		{
			Layout.preferredWidth: 150 * jaspTheme.uiScale
			name:		"funnelPlotAsymmetryTests"
			id:			funnelPlotAsymmetryTests
			label:		qsTr("Asymmetry tests")
			info: qsTr("Perform tests to detect asymmetry in the funnel plot indicating potential publication bias.")


			CheckBox
			{
				name:		"funnelPlotAsymmetryTestsMetaRegression"
				id:			funnelPlotAsymmetryTestsMetaRegression	
				label:		qsTr("Meta-regression")
				checked:	true
				info: qsTr("Include meta-regression tests for funnel plot asymmetry. The test is performed with the 'Method' specified under 'General Settings'.")
			}

			CheckBox
			{
				name:		"funnelPlotAsymmetryTestsWeightedRegression"
				label:		qsTr("Weighted regression (Egger's test)")
				info: qsTr("Include weighted regression tests (also known as Egger's test) for funnel plot asymmetry.")

			}

			CheckBox
			{
				name:		"funnelPlotAsymmetryTestsRankCorrelation"
				label:		qsTr("Rank correlation (Begg's test)")
				info: qsTr("Include rank correlation tests (also known as Begg's test) for funnel plot asymmetry.")
			}
		}

		CheckBox
		{
			name:		"trimAndFill"
			id:			trimAndFill
			label:		qsTr("Trim and fill")
			info: qsTr("Perform the trim and fill to adjust the funnel plot for publication bias. The test is performed with the 'Method' specified under 'General Settings'.")

			DropDown
			{
				name:		"trimAndFillEstimator"
				label:		qsTr("Estimator")
				startValue:	"L0"
				info: qsTr("Select the method for the trim-and-fill adjustment.")
				values:		[
					{ label: qsTr("L0")	, value: "L0"	},
					{ label: qsTr("R0")	, value: "R0"	},
					{ label: qsTr("Q0")	, value: "Q0"	}
				]
			}

			CheckBox
			{
				name:		"trimAndFillIncludeHeterogeneity"
				label:		qsTr("Include heterogeneity")
				enabled:	trimAndFillMethod.value != "fixedEffects" && trimAndFillMethod.value != "equalEffects"
				info: qsTr("Include heterogeneity (𝜏) in the trim and fill funnel plot. If unselected, the heterogeneity estimate is not used to adjust the prediction intervals.") 
			}

			CheckBox
			{
				name:		"trimAndFillEstimatesTable"
				label:		qsTr("Estimates table")
				info: qsTr("Summarize the effect size, heterogeneity, and number of imputed estimatesused for the trim and fill in a table.")
			}

			CheckBox
			{
				name:		"trimAndFillFillColors"
				label:		qsTr("Fill colors")
				checked:	true
				info: qsTr("Fill the funnel plot's prediction intervals under trim and fill with different colors.")
			}

			DropDown
			{
				name:		"trimAndFillLineType"
				label:		qsTr("Line type")
				startValue:	"none"
				info: qsTr("Set the type of line of the funnel plot's prediction intervals under trim and fill.")
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
			name:		"failSafeN"
			label:		qsTr("Fail-safe N")
			info: qsTr("Compute the minimum number of studies averaging null results that would have to be added to a given set of studies to change the conclusion of a meta-analysis tests to detect asymmetry in the funnel plot indicating potential publication bias.")


			CheckBox
			{
				name:		"failSafeNRosenthal"
				id:			failSafeNRosenthal	
				label:		qsTr("Rosenthal")
				checked:	true
				info: qsTr("The Rosenthal method calculates the minimum number of studies averaging null results needed to reduce the combined significance level to a specified alpha level.")
			}

			CheckBox
			{
				name:		"failSafeNOrwin"
				id:			failSafeNOrwin
				label:		qsTr("Orwin")
				info: qsTr("The Orwin method calculates the minimum number of studies averaging null results needed to reduce the average effect size to a target value. The default is set to 0.10 which is completely aribitrary.")

			}

			CheckBox
			{
				name:		"failSafeNRosenberg"
				id:			failSafeNRosenberg
				label:		qsTr("Rosenberg")
				info: qsTr("The Rosenberg method calculates the minimum number of studies averaging null results needed to reduce the significance level of the average effect size to a specified alpha level.")
			}

			CheckBox
			{
				name:		"failSafeNGeneral"
				id:			failSafeNGeneral
				label:		qsTr("General")
				info: qsTr("A general method to calculate the minimum number of studies averaging null results needed to reduce the significance level or the average effect size to a target effect size value. The default is set to 0.10 which is completely aribitrary.")

				CheckBox
				{
					name:	"failSafeNGeneralExact"
					label:	qsTr("Exact")
					info:	qsTr("Use the exact method to calculate the fail-safe N.")
				}
			}

			DoubleField
			{
				name:			"failSafeNAlpha"
				label:			qsTr("Alpha")
				enabled:		failSafeNRosenthal.checked || failSafeNRosenberg.checked || failSafeNGeneral.checked
				defaultValue:	0.05
				min:			0.00001
				max:			1
				info: qsTr("Set the significance level for the Rosenthal's, Rosenberg's, and general fail-safe N tests.")
			}

			DoubleField
			{
				name:			"failSafeNTarget"
				label:			qsTr("Target effect size")
				enabled:		failSafeNOrwin.checked || failSafeNGeneral.checked
				defaultValue:	0.1
				negativeValues:	true
				info: qsTr("Set the target effect size for the Orwin's and general fail-safe N tests.")
			}

		}
	}

}
