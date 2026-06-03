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
	info: qsTr("Create a forest plot from pre-computed effect sizes and confidence intervals without fitting a meta-analytic model. " +
	"This allows visualizing results from any source — external software, individual studies, or custom computations.")

	VariablesForm
	{
		preferredHeight: 500 * preferencesModel.uiScale

		AvailableVariablesList
		{
			name:	"allVariables"
		}

		AssignedVariablesList
		{
			name:				"effectSize"
			title:				qsTr("Effect Size")
			singleVariable:		true
			allowedColumns:		["scale"]
			info: qsTr("Variable containing the effect size estimates.")
		}

		AssignedVariablesList
		{
			name:				"effectSizeStandardError"
			title:				qsTr("Effect Size Standard Error")
			singleVariable:		true
			allowedColumns:		["scale"]
			info: qsTr("Variable containing the standard errors. Used to compute confidence intervals when not provided directly, and to determine point sizes via inverse-variance weights when no weight variable is specified.")
		}

		AssignedPairsVariablesList
		{
			name:				"confidenceInterval"
			title:				qsTr("Confidence Interval")
			singleVariable:		true
			allowedColumns:		["scale"]
			info: qsTr("Two variables specifying the lower and upper bounds of the confidence interval. If not provided, intervals are computed from the standard error at the 95%% level.")
		}

		AssignedPairsVariablesList
		{
			name:				"predictionInterval"
			title:				qsTr("Prediction Interval")
			singleVariable:		true
			allowedColumns:		["scale"]
			info: qsTr("Two variables specifying the lower and upper bounds of the prediction interval. Displayed as rectangles behind estimate diamonds.")
		}

		AssignedVariablesList
		{
			name:				"weight"
			id:					weight
			title:				qsTr("Weight")
			singleVariable:		true
			allowedColumns:		["scale"]
			info: qsTr("Variable containing study weight. When specified, study points are scaled by these weights and the study weights can be shown in the study information panel.")
		}

		AssignedVariablesList
		{
			name:				"row"
			title:				qsTr("Row")
			singleVariable:		true
			allowedColumns:		["scale"]
			info: qsTr("Variable controlling the vertical positioning of rows. The first position appears at the top of the figure. If not provided, the dataset row order is used.")
		}

		AssignedVariablesList
		{
			name:				"visualizationType"
			id:					visualizationType
			title:				qsTr("Visualization Type")
			singleVariable:		true
			allowedColumns:		["nominal"]
			info: qsTr("Nominal variable whose levels distinguish study rows (displayed as points with intervals) from estimate rows (displayed as diamonds).")
		}

		DropDown
		{
			name:				"visualizationTypeStudy"
			label:				qsTr("Study")
			source:				[{name: "visualizationType", use: "levels"}]
			onCountChanged:		currentIndex = 0
			info: qsTr("Specify the value in the Visualization Type variable that indicates the study visualization type.")
		}

		DropDown
		{
			name:				"visualizationTypeEstimate"
			label:				qsTr("Estimate")
			source:				[{name: "visualizationType", use: "levels"}]
			onCountChanged:		currentIndex = 1
			info: qsTr("Specify the value in the Visualization Type variable that indicates the estimate visualization type.")
		}
		
		AssignedVariablesList
		{
			name:				"subgroup"
			id:					subgroup
			title:				qsTr("Subgroup")
			singleVariable:		true
			allowedColumns:		["nominal"]
			info: qsTr("Nominal variable defining subgroups. Each subgroup is displayed as a separate panel in the forest plot.")
		}
	}

	DropDown
	{
		name:		"transformEffectSize"
		id:			transformEffectSize
		label:		qsTr("Transform effect size")
		info: qsTr("Apply a transformation to the effect sizes and confidence intervals for display.")
		values:	[
			{ label: qsTr("None"),								value: "none"						},
			{ label: qsTr("Fisher's z to r"),					value: "fishersZToCorrelation"		},
			{ label: qsTr("Exponential"),						value: "exponential"				},
			{ label: qsTr("Log odds to proportions"),			value: "logOddsToProportions"		},
			{ label: qsTr("Log odds to SMD (normal)"),			value: "logOddsToSmdNormal"			},
			{ label: qsTr("Log odds to SMD (logistic)"),		value: "logOddsToSmdLogistic"		},
			{ label: qsTr("SMD to log odds (normal)"),			value: "smdToLogOddsNormal"			},
			{ label: qsTr("SMD to log odds (logistic)"),		value: "smdToLogOddsLogistic"		},
			{ label: qsTr("Hakstian & Whalen inverse α"),		value: "hakstianAndWhalenInverseAlpha" },
			{ label: qsTr("Bonett inverse α"),					value: "bonettInverseAlpha"			},
			{ label: qsTr("Z to R²"),							value: "zToR2"						},
			{ label: qsTr("SMD to Cohen's U₁"),				value: "smdToCohensU1"				},
			{ label: qsTr("SMD to Cohen's U₂"),				value: "smdToCohensU2"				},
			{ label: qsTr("SMD to Cohen's U₃"),				value: "smdToCohensU3"				},
			{ label: qsTr("SMD to CLES, Pr(superiority)"),		value: "smdToCles"					}
		]
	}


	Divider { }

	Text
	{
		text:	qsTr("Study Information")
	}

	MA.ForestPlotStudyInformation
	{
		showPredictedEffects:	false
		showStudyWeights:		weight.count > 0
		showSecondaryCI:		false
		showAggregate:			true
		showWeightDisplayOptions:	true
	}


	Divider { }

	Text
	{
		text:	qsTr("Estimate Information")
	}

	MA.ForestPlotEstimateInformation {}


	Divider { }

	Text
	{
		text:	qsTr("Settings")
	}

	MA.ForestPlotSettings
	{
		transformEffectSizeValue:	transformEffectSize.value
		enableSubgroupSettings:		subgroup.count > 0
	}
}
