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
import "../qml/qml_components" as MA

Form
{
	VariablesForm
	{
		preferredHeight: 400 * preferencesModel.uiScale

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
		}
		AssignedVariablesList
		{
			name:				"effectSizeStandardError"
			id:					effectSizeStandardError
			title:				qsTr("Effect Size Standard Error")
			singleVariable:		true
			allowedColumns:		["scale"]
		}

		DropDown
		{
			name:			"method"
			id:				method
			label:			qsTr("Method")
			startValue:		"restrictedML"
			values:			(function() {
				if (heterogeneityModelTerms.count == 0) {
					return [
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
						{ label: qsTr("Paule-Mandel (MU)")		, value: "pauleMandelMu"	},
						{ label: qsTr("Generalized Q-stat")		, value: "qeneralizedQStat"	},
						{ label: qsTr("Generalized Q-stat (MU)"), value: "qeneralizedQStatMu"}
					];
				} else {
					return [
						{ label: qsTr("Maximum Likelihood")		, value: "maximumLikelihood"},
						{ label: qsTr("Restricted ML")			, value: "restrictedML"		},
						{ label: qsTr("Empirical Bayes")		, value: "empiricalBayes"	}
					];
				}})()
		}

		DropDown
		{
			name:		"fixedEffectTest"
			label:		qsTr("Fixed effect test")
			startValue:	"knha"
			values:		[ "z", "t", "knha"]
		}

		AssignedVariablesList
		{
			name:				"predictors"
			id:					predictors
			title:				qsTr("Predictors")
			allowedColumns:		["nominal", "scale"]
		}

		AssignedVariablesList
		{
			name:				"clustering"
			id:					clustering
			title:				qsTr("Clustering")
			singleVariable:		true
			allowedColumns:		["nominal"]
		}
	}

	Section
	{
		title:	qsTr("Model")

		Group
		{
			title: qsTr("Effect size model")

			VariablesForm
			{
				preferredHeight:	150 * preferencesModel.uiScale

				AvailableVariablesList
				{
					name:			"effectSizeModelAvailableComponents"
					title:			qsTr("Available Components")
					source:			["predictors"]
				}

				AssignedVariablesList
				{
					name:			"effectSizeModelTerms"
					id:				effectSizeModelTerms
					title:			qsTr("Model Terms")
					listViewType:	JASP.Interaction
					allowTypeChange:false
				}
			}

			CheckBox
			{
				name:				"effectSizeModelIncludeIntercept"
				label:				qsTr("Include intercept")
				checked:			true
			}
		}

		Group
		{
			title:			qsTr("Heterogeneity model")
			columns:	2

			VariablesForm
			{
				preferredHeight:	150 * preferencesModel.uiScale

				AvailableVariablesList
				{
					name:			"heterogeneityModelAvailableComponents"
					title:			qsTr("Available Components")
					source:			["predictors"]
				}

				AssignedVariablesList
				{
					name:			"heterogeneityModelTerms"
					id:				heterogeneityModelTerms
					title:			qsTr("Model Terms")
					listViewType:	JASP.Interaction
					allowTypeChange:false
					addAvailableVariablesToAssigned: false
				}
			}

			CheckBox
			{
				name:		"heterogeneityModelIncludeIntercept";
				label:		qsTr("Include intercept")
				checked:	true
			}

			DropDown
			{
				name:		"heterogeneityModelLink"
				id:			heterogeneityModelLink
				label:		qsTr("Link")
				values:		["log", "identity"]
			}
		}
	}

	Section
	{
		title:	qsTr("Statistics")
		columns: 2

		Group
		{
			title:		qsTr("Heterogeneity")
			columns:	2
			enabled:	method.value != "fixedEffects" && method.value != "equalEffects"

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

	Section
	{
		title:		qsTr("Estimated Marginal Means")
		columns:	1

		Group
		{
			title:		qsTr("Effect size")
			enabled:	effectSizeModelTerms.count > 0

			VariablesForm
			{
				preferredHeight:	250

				AvailableVariablesList
				{
					name:			"estimatedMarginalMeansEffectSizeModelVariables"
					title:			qsTr("Model variables")
					source:			[{ name: "effectSizeModelTerms", use: "noInteraction" }]
				}

				AssignedVariablesList
				{
					id:				estimatedMarginalMeansEffectSizeSelectedVariables
					name:			"estimatedMarginalMeansEffectSizeSelectedVariables"
					title:			qsTr("Selected variables")
					allowTypeChange:false
				}
			}

			Group
			{
				columns:	2

				DoubleField
				{
					name:			"estimatedMarginalMeansEffectSizeSdFactorCovariates"
					label:			qsTr("SD factor covariates")
					defaultValue: 	1
					min:			0
					enabled:		estimatedMarginalMeansEffectSizeSelectedVariables.columnsTypes.includes("scale")
					Layout.preferredWidth: 350 * jaspTheme.uiScale
				}

				CheckBox
				{
					name:		"estimatedMarginalMeansEffectSizeAddAdjustedEstimate"
					label:		qsTr("Add adjusted estimate")
				}

				CheckBox
				{
					name:				"estimatedMarginalMeansEffectSizeTestAgainst"
					label:				qsTr("Test against")
					childrenOnSameRow:	true

					DoubleField
					{
						name:			"estimatedMarginalMeansEffectSizeTestAgainstValue"
						defaultValue:	0
					}
				}
			}
		}

		Group
		{
			title:		qsTr("Heterogeneity")
			enabled:	heterogeneityModelTerms.count > 0

			VariablesForm
			{
				preferredHeight:	250

				AvailableVariablesList
				{
					name:			"estimatedMarginalHeterogeneityModelVariables"
					title:			qsTr("Model variables")
					source:			[{ name: "heterogeneityModelTerms", use: "noInteraction" }]
				}

				AssignedVariablesList
				{
					id:				estimatedMarginalMeansHeterogeneitySelectedVariables
					name:			"estimatedMarginalMeansHeterogeneitySelectedVariables"
					title:			qsTr("Selected variables")
					allowTypeChange:false
				}
			}

			Group
			{
				columns:	2

				DoubleField
				{
					name:			"estimatedMarginalMeansHeterogeneitySdFactorCovariates"
					label:			qsTr("SD factor covariates")
					defaultValue: 	1
					min:			0
					enabled:		estimatedMarginalMeansHeterogeneitySelectedVariables.columnsTypes.includes("scale")
					Layout.preferredWidth: 350 * jaspTheme.uiScale
				}

				CheckBox
				{
					name:		"estimatedMarginalMeansHeterogeneityAddAdjustedEstimate"
					label:		qsTr("Add adjusted estimate")
				}

				DropDown
				{
					name:			"estimatedMarginalMeansHeterogeneityTransformation"
					label:			qsTr("Heterogeneity transformation")
					values:			[
							{ label: qsTr("𝜏")		, value: "tau"	},
							{ label: qsTr("𝜏²")	, value: "tau2"	}
						]
				}
			}
		}
	}

	Section
	{
		title:		qsTr("Forest Plot")
		columns:	1

		CheckBox
		{
			id:			forestPlotStudyInformation
			name: 		"forestPlotStudyInformation"
			text: 		qsTr("Study information")
		}

		VariablesForm
		{
			preferredHeight: 	150 * preferencesModel.uiScale
			enabled:			forestPlotStudyInformation.checked

			AvailableVariablesList
			{			
				name:				"forestPlotStudyInformationAllVariables"
			}

			AssignedVariablesList
			{
				name:				"forestPlotStudyInformationSelectedVariables"
				id:					forestPlotStudyInformationSelectedVariables
				title:				qsTr("Selected Variables")
				allowedColumns:		["nominal"]
			}
		}

		ComponentsList
		{
			name:				"forestPlotStudyInformationSelectedVariablesSettings"
			source:				"forestPlotStudyInformationSelectedVariables"
			enabled:			forestPlotStudyInformation.checked
			visible:			forestPlotStudyInformationSelectedVariables.count > 0
			headerLabels:		[qsTr("Title"), qsTr("Width"), qsTr("Alignment")]

			rowComponent: 			RowLayout
			{
				Text
				{
					text:					rowValue
					Layout.preferredWidth:	100 * preferencesModel.uiScale
					elide:					Text.ElideRight
				}

				TextField
				{
					label:				""
					name:				"title"
					value:				""
					fieldWidth: 		120 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}

				DoubleField
				{
					label: 				""
					name: 				"width"
					value:				"1"
					min: 				0
					inclusive: 			JASP.None
					fieldWidth:			40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder:			true
				}

				DropDown
				{
					label: 				""
					name: 				"alignment"
					values:				[
							{ label: qsTr("Left")		, value: "left"		},
							{ label: qsTr("Middle")		, value: "middle"	},
							{ label: qsTr("Right")		, value: "right"	}
						]
					fieldWidth:			40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder:			true
				}
			}
		}

		Group
		{
			enabled:	forestPlotStudyInformation.checked
			columns:	2

			CheckBox
			{
				name:		"forestPlotStudyInformationPredictedEffects"
				text:		qsTr("Predicted effects")
				enabled:	effectSize.count == 1 && effectSizeStandardError.count == 1
				checked:	false
				Layout.preferredWidth: 300 * jaspTheme.uiScale
			}

			Group
			{
				title:		qsTr("Order")
				DropDown
				{
					name:			"forestPlotStudyInformationOrderBy"
					label:			qsTr("By")
					addEmptyValue:	true
				}

				CheckBox
				{
					name:		"forestPlotStudyInformationOrderAscending"
					text:		qsTr("Ascending")
				}
			}
		}



		Divider { }

		CheckBox
		{
			name:		"forestPlotEstimatedMarginalMeans"
			id:			forestPlotEstimatedMarginalMeans
			text:		qsTr("Estimated marginal means")
			enabled:	effectSizeModelTerms.count > 0
		}

		VariablesForm
		{
			preferredHeight:	250
			enabled:			forestPlotEstimatedMarginalMeans.checked

			AvailableVariablesList
			{
				name:			"forestPlotEstimatedMarginalMeansModelVariables"
				title:			qsTr("Model variables")
				source:			[{ name: "effectSizeModelTerms", use: "noInteraction" }]
			}

			AssignedVariablesList
			{
				id:				forestPlotEstimatedMarginalMeansSelectedVariables
				name:			"forestPlotEstimatedMarginalMeansSelectedVariables"
				title:			qsTr("Selected variables")
				allowTypeChange:false
			}
		}

		Group
		{
			columns:	2

			Group
			{
				CheckBox
				{
					name:		"forestPlotEstimatedMarginalMeansTermTests"
					id:			forestPlotEstimatedMarginalMeansTermTests
					enabled:	forestPlotEstimatedMarginalMeans.checked
					label:		qsTr("Term tests")
					checked:	true
					Layout.preferredWidth: 350 * jaspTheme.uiScale
				}

				CheckBox
				{
					name:		"forestPlotEstimatedMarginalMeansCoefficientTests"
					id:			forestPlotEstimatedMarginalMeansCoefficientTests
					enabled:	forestPlotEstimatedMarginalMeans.checked
					label:		qsTr("Coefficient tests")
					checked:	true

					DoubleField
					{
						name:			"forestPlotEstimatedMarginalMeansCoefficientTestsAgainst"
						text:			qsTr("Against")
						defaultValue:	0
					}
				}

			}

			CheckBox
			{
				name:		"forestPlotEstimatedMarginalMeansAdjustedEffectSizeEstimate"
				label:		qsTr("Adjusted effect size estimate")
				enabled:	forestPlotEstimatedMarginalMeans.checked
			}
		}


		Divider { }

		CheckBox
		{
			name:		"forestPlotModelInformation"
			id:			forestPlotModelInformation
			enabled:	effectSize.count == 1 && effectSizeStandardError.count == 1
			text:		qsTr("Model information")
		}

		Group
		{
			enabled:	forestPlotModelInformation.checked
			columns:	2

			CheckBox
			{
				name:		"forestPlotPooledEffectSizeEstimate"
				text:		qsTr("Pooled effect size estimate")
				checked:	true				
				Layout.preferredWidth: 300 * jaspTheme.uiScale
			}

			CheckBox
			{
				name:		"forestPlotPooledEffectSizeTest"
				text:		qsTr("Pooled effect size test")
				checked:	true
			}

			CheckBox
			{
				name:		"forestPlotResidualHeterogeneityTest"
				text:		qsTr("Residual heterogeneity test")
				checked:	true
			}

			CheckBox
			{
				name:		"forestPlotResidualHeterogeneityEstimate"
				text:		qsTr("Residual heterogeneity estimate")
				enabled:	(method.value != "fixedEffects" || method.value != "equalEffects")
				checked:	true
			}

			CheckBox
			{
				name:		"forestPlotEffectSizeModerationTest"
				text:		qsTr("Effect size moderation test")
				enabled:	effectSizeModelTerms.count > 0
				checked:	true
			}

			CheckBox
			{
				name:		"forestPlotHeterogeneityModerationTest"
				text:		qsTr("Heterogeneity moderation test")
				enabled:	heterogeneityModelTerms.count > 0
				checked:	true
			}
		}


		Divider {}

		Text
		{
			text:	qsTr("Settings")
		}

		Group
		{
			columns:	2

			CheckBox
			{
				name:		"forestPlotPredictionIntervals"
				text:		qsTr("Prediction intervals")
				checked:	true
				Layout.preferredWidth: 300 * jaspTheme.uiScale
			}

			Group
			{
				title:	qsTr("Mapping")

				DropDown
				{
					name:			"forestPlotMappingColor"
					label:			qsTr("Color")
					addEmptyValue:	true
				}

				DropDown
				{
					name:			"forestPlotMappingShape"
					label:			qsTr("Shape")
					addEmptyValue:	true
				}
			}

			CheckBox
			{
				name:		"forestPlotRightPanel"
				text:		qsTr("Right Panel")
				checked:	true

				CheckBox
				{
					name:			"forestPlotRightPanelEstimate"
					text:			qsTr("Estimates and confidence intervals")
					checked:		true
				}

				CheckBox
				{
					name:			"forestPlotRightPanelWeights"
					text:			qsTr("Weights")
					enabled:		forestPlotStudyInformation.checked
				}
			}

			Group
			{
				title:		qsTr("Relative Size")

				DoubleField
				{
					name:			"forestPlotRelativeSizeEstimates"
					text:			qsTr("Estimates")
					defaultValue:	1
					min:			0
					inclusive: 		JASP.None
				}

				DoubleField
				{
					name:			"forestPlotRelativeSizeText"
					text:			qsTr("Text")
					defaultValue:	1
					min:			0
					inclusive: 		JASP.None
				}

				DoubleField
				{
					name:			"forestPlotRelativeSizeAxisLabels"
					text:			qsTr("Axis labels")
					defaultValue:	1
					min:			0
					inclusive: 		JASP.None
				}

				DoubleField
				{
					name:			"forestPlotRelativeSizeRow"
					text:			qsTr("Row")
					defaultValue:	1
					min:			0
					inclusive: 		JASP.None
				}

				DoubleField
				{
					name:			"forestPlotRelativeSizeLeftPanel"
					text:			qsTr("Left panel")
					defaultValue:	1
					min:			0
					inclusive: 		JASP.None
				}

				DoubleField
				{
					name:			"forestPlotRelativeSizeMiddlePanel"
					text:			qsTr("Middle panel")
					defaultValue:	1
					min:			0
					inclusive: 		JASP.None
				}

				DoubleField
				{
					name:			"forestPlotRelativeSizeRightPanel"
					text:			qsTr("Right panel")
					defaultValue:	1
					min:			0
					inclusive: 		JASP.None
				}
			}
		}

		Group
		{
			title:		qsTr("Auxiliary")

			IntegerField
			{
				name:			"forestPlotAuxiliaryDigits"
				text:			qsTr("Digits")
				min:			1
				value:			2
				inclusive: 		JASP.None
			}

			DropDown
			{
				label:		qsTr("Tests information")
				name:		"forestPlotAuxiliaryTestsInformation"
				values:		[
						{ label: qsTr("Statistic and p-value")		, value: "statisticAndPValue"	},
						{ label: qsTr("P-value")					, value: "pValue"				}
				]
			}

			DropDown
			{
				name:			"forestPlotAuxiliaryPlotColor"
				label:			qsTr("Color")
				values:			[
						{ label: qsTr("Black")		, value: "black"},
						{ label: qsTr("Blue")		, value: "blue"	},
						{ label: qsTr("Red")		, value: "red"	}
					]
			}

			CheckBox
			{
				name:				"forestPlotAuxiliaryAddVerticalLine"
				text:				qsTr("Add vertical line")
				childrenOnSameRow:	true

				DoubleField
				{
					name:			"forestPlotAuxiliaryAddVerticalLineValue"
					defaultValue:	0
					negativeValues:	true
				}
			}

			CheckBox
			{
				name:				"forestPlotAuxiliaryAddVerticalLine2"
				text:				qsTr("Add vertical line (2)")
				childrenOnSameRow:	true

				DoubleField
				{
					name:			"forestPlotAuxiliaryAddVerticalLineValue2"
					defaultValue:	0
					negativeValues:	true
				}
			}

			TextField
			{
				name:			"forestPlotAuxiliaryEffectLabel"
				text:			qsTr("X-axis label")
				value:			"Effect Size"
			}

			CheckBox
			{
				name:			"forestPlotAuxiliarySetXAxisLimit"
				text:			qsTr("X-axis limits")
				childrenOnSameRow:	true

				DoubleField
				{
					name:			"forestPlotAuxiliarySetXAxisLimitLower"
					id:				forestPlotAuxiliarySetXAxisLimitLower
					text:			qsTr("Lower")
					defaultValue:	-1
					negativeValues:	true
					max:			forestPlotAuxiliarySetXAxisLimitUpper
					inclusive: 		JASP.None
				}

				DoubleField
				{
					name:			"forestPlotAuxiliarySetXAxisLimitUpper"
					id:				forestPlotAuxiliarySetXAxisLimitUpper
					text:			qsTr("Upper")
					defaultValue:	1
					min:			forestPlotAuxiliarySetXAxisLimitLower			
					inclusive: 		JASP.None
				}
			}

			
			CheckBox
			{
				name:			"forestPlotAuxiliaryGuessTextWidth"
				text:			qsTr("Guess text width")
				checked:		true
			}
		}




	}

	MA.ClassicalMetaAnalysisDiagnostics{}

	Section
	{
		title:	qsTr("Advanced")

		CheckBox
		{
			name:		"weightedEstimation"
			text:		qsTr("Weighted estimation")
			checked:	true
		}

		Group
		{
			title:		qsTr("Clustering")
			enabled:	clustering.count == 1

			CheckBox
			{
				name:		"clusteringUseClubSandwich"
				text:		qsTr("Use clubSandwich")
				checked:	true
			}

			CheckBox
			{
				name:		"clusteringSmallSampleCorrection"
				text:		qsTr("Small sample correction")
				checked:	true
			}
		}

		Group
		{
			title:	qsTr("Fix Parameters")

			CheckBox
			{
				name:	"fixParametersTau2"
				text:	qsTr("𝜏²")
				enabled:			heterogeneityModelTerms.count == 0
				childrenOnSameRow:	true

				FormulaField
				{
					label: 				""
					name: 				"fixParametersTau2Value"
					value:				"1"
					min: 				0
					inclusive: 			JASP.None
				}
			}

			CheckBox
			{
				name:	"fixParametersWeights"
				text:	qsTr("Weights")
				childrenOnSameRow:	true

				DropDown
				{
					label: 				""
					name: 				"fixParametersWeightsVariable"
					source:				"allVariables"
				}
			}
		}

		Group
		{
			title:		qsTr("Add Omibus Moderator Test")
			enabled:	effectSizeModelTerms.count > 0 || heterogeneityModelTerms.count > 0

			CheckBox
			{
				text:	qsTr("Effect size coefficients")
				name:	"addOmnibusModeratorTestEffectSizeCoefficients"
				enabled:			effectSizeModelTerms.count > 0
				childrenOnSameRow:	false

				TextField
				{
					label: 				""
					name: 				"addOmnibusModeratorTestEffectSizeCoefficientsValues"
					value:				"c(1, 2)"
				}
			}

			CheckBox
			{
				text:	qsTr("Heterogeneity coefficients")
				name:	"addOmnibusModeratorTestHeterogeneityCoefficients"
				enabled:			heterogeneityModelTerms.count > 0
				childrenOnSameRow:	false

				TextField
				{
					label: 				""
					name: 				"addOmnibusModeratorTestHeterogeneityCoefficientsValues"
					value:				"c(1, 2)"
				}
			}
		}

		Group
		{
			title:		qsTr("Optimizer")
			enabled:	method.value == "restrictedML" || method.value == "maximumLikelihood" || method.value == "empiricalBayes" ||
						method.value == "pauleMandel" || method.value == "pauleMandelMu" || method.value == "qeneralizedQStatMu" ||
						method.value == "sidikJonkman"

			DropDown
			{
				name:		"optimizerMethod"
				label:		qsTr("Method") // TODO: switch default value on heterogeneityModelLink change
				values:		{
					if (heterogeneityModelLink.value == "log")
						["nlminb", "BFGS", "Nelder-Mead", "uobyqa", "newuoa", "bobyqa", "nloptr", "nlm"]
					else
						["constrOptim", "nlminb", "BFGS", "Nelder-Mead", "uobyqa", "newuoa", "bobyqa", "nloptr", "nlm"]
				}
				visible:	heterogeneityModelTerms.count > 0
			}

			CheckBox
			{
				name:		"optimizerInitialTau2"
				text:		qsTr("Initial 𝜏²")
				checked:	false
				childrenOnSameRow:	true
				visible:	(method.value == "restrictedML" || method.value == "maximumLikelihood" || method.value == "empiricalBayes" ||
							method.value == "sidikJonkman") && heterogeneityModelTerms.count == 0

				DoubleField
				{
					label: 				""
					name:				"optimizerInitialTau2Value"
					defaultValue:		1
					min: 				0
					inclusive: 			JASP.None
				}
			}

			CheckBox
			{
				name:		"optimizerMinimumTau2"
				text:		qsTr("Minimum 𝜏²")
				checked:	false
				childrenOnSameRow:	true
				visible:	(method.value == "pauleMandel" || method.value == "pauleMandelMu" || method.value == "qeneralizedQStatMu") &&
							heterogeneityModelTerms.count == 0

				DoubleField
				{
					label: 				""
					name: 				"optimizerMinimumTau2Value"
					id:					optimizerMinimumTau2Value
					defaultValue:		1e-6
					min: 				0
					max: 				optimizerMaximumTau2Value.value
				}
			}

			CheckBox
			{
				name:		"optimizerMaximumTau2"
				text:		qsTr("Maximum 𝜏²")
				checked:	false
				childrenOnSameRow:	true
				visible:	(method.value == "pauleMandel" || method.value == "pauleMandelMu" || method.value == "qeneralizedQStatMu") &&
							heterogeneityModelTerms.count == 0

				DoubleField
				{
					label: 				""
					name: 				"optimizerMaximumTau2Value"
					id:					optimizerMaximumTau2Value
					defaultValue:		100
					min: 				optimizerMinimumTau2Value.value
					inclusive: 			JASP.None
				}
			}

			CheckBox
			{
				name:		"optimizerMaximumIterations"
				text:		qsTr("Maximum iterations")
				checked:	false
				childrenOnSameRow:	true
				visible:	method.value == "restrictedML" || method.value == "maximumLikelihood" || method.value == "empiricalBayes" ||
							method.value == "pauleMandel" || method.value == "pauleMandelMu" || method.value == "qeneralizedQStatMu"

				IntegerField
				{
					label: 				""
					name: 				"optimizerMaximumIterationsValue"
					value:				{
						if (heterogeneityModelTerms.count == 0)
							100
						else
							1000
					}
					min: 				1
					inclusive: 			JASP.None
				}
			}

			CheckBox
			{
				name:		"optimizerConvergenceTolerance"
				text:		qsTr("Convergence tolerance")
				checked:	false
				childrenOnSameRow:	true
				visible:	(method.value == "restrictedML" || method.value == "maximumLikelihood" || method.value == "empiricalBayes" ||
							method.value == "pauleMandel" || method.value == "pauleMandelMu" || method.value == "qeneralizedQStatMu") &&
							heterogeneityModelTerms.count == 0

				DoubleField
				{
					label: 				""
					name: 				"optimizerConvergenceToleranceValue"
					defaultValue:		{
						if (method.value == "restrictedML" || method.value == "maximumLikelihood" || method.value == "empiricalBayes")
							1e-5
						else if (method.value == "pauleMandel" || method.value == "pauleMandelMu" || method.value == "qeneralizedQStatMu")
							1e-4
						else
							1
					}
					min: 				0
					inclusive: 			JASP.None
				}
			}

			CheckBox
			{
				name:		"optimizerConvergenceRelativeTolerance"
				text:		qsTr("Convergence relative tolerance")
				checked:	false
				childrenOnSameRow:	true
				visible:	heterogeneityModelTerms.count > 0

				DoubleField
				{
					label: 				""
					name: 				"optimizerConvergenceRelativeToleranceValue"
					defaultValue:		1e-8
					min: 				0
					inclusive: 			JASP.None
				}
			}

			CheckBox
			{
				name:		"optimizerStepAdjustment"
				text:		qsTr("Step adjustment")
				checked:	false
				childrenOnSameRow:	true
				visible:	(method.value == "restrictedML" || method.value == "maximumLikelihood" || method.value == "empiricalBayes") &&
							heterogeneityModelTerms.count == 0

				DoubleField
				{
					label: 				""
					name: 				"optimizerStepAdjustmentValue"
					defaultValue:		1
					min: 				0
					inclusive: 			JASP.None
				}
			}
		}
	}
}
