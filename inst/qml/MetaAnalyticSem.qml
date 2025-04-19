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

	VariablesForm
	{
		preferredHeight: 425 * preferencesModel.uiScale

		AvailableVariablesList
		{
			name:				"allVariables"
		}

		AssignedVariablesList
		{
			name:				"correlationCovarianceMatrix"
			title:				qsTr("Correlation/Covariance Matrix")
			allowedColumns:		["scale"]
			height:				250 * preferencesModel.uiScale
			info: qsTr("Variables containing the correlations/covariances between the variables. " +
			"The variable name must be in a form `x_y` where `x` and `y` corresponds to the variables between which the correlation/covariance is reported. " +
			"The separator used in the variable names (defaults to `_`) can be changed via the `Variable name separator` option.")
		}

		AssignedVariablesList
		{
			name:				"sampleSize"
			title:				qsTr("Sample Size")
			singleVariable:		true
			allowedColumns:		["scale"]
			info: qsTr("Variable containing sample sizes for the studies")
		}

		AssignedVariablesList
		{
			name:				"means"
			id:					means
			title:				qsTr("Means")
			allowedColumns:		["scale"]
			height:				100 * preferencesModel.uiScale
			info: qsTr("Variables containing the means of the variables. The variable name must be `x` where `x` corresponds to the variable names in the correlation/covariance matrix input. This input is required only when meta-analytic sem with means is requested.")
		}
/*
		AssignedVariablesList
		{
			name:				"predictors"
			id:					predictors
			title:				qsTr("Predictors")
			allowedColumns:		["nominal", "scale"]
			allowTypeChange:	true
			info: qsTr("Variables to include as predictors (moderators) in the meta-regression model.")
		}

		AssignedVariablesList
		{
			name:				"studyLabels"
			title:				qsTr("Study Labels")
			singleVariable:		true
			allowedColumns:		["nominal"]
			info: qsTr("Variable containing labels for the studies. Used for labeling outputs and plots.")
		}
*/
	}

	Group
	{
		RadioButtonGroup
		{

			name:		"dataInputType"
			id:			dataInputType
			title:		qsTr("Data Input Type")

			RadioButton
			{
				name:		"correlation"
				label:		qsTr("Correlation")
				checked:	true
				info:		qsTr("Select this option for a correlation input.")
			}

			RadioButton
			{
				name:		"covariance"
				label:		qsTr("Covariance")
				info:		qsTr("Select this option for a covariance input.")
			}
		}

		DropDown
		{
			name:			"variableNameSeparator"
			label:			qsTr("Variable name separator")
			startValue:		"_"
			info: qsTr("Separator used in the variable names in the correlation/covariance matrix input. The variable name must be in a form `x_y` where `x` and `y` corresponds to the variables between which the correlation/covariance is reported.")
			values:
			[
				{ label: qsTr("_"),		value: "_" },
				{ label: qsTr("."),		value: "." },
				{ label: qsTr("-"),		value: "-" },
				{ label: qsTr(" "),		value: " " }
			]
		}
	}

	Group
	{
		CheckBox
		{
			name:		"availableVariableNames"
			label:		qsTr("Available variable names")
			checked:	true
			info:		qsTr("Show a summary of the available variable names in the correlation/covariance matrix input.")
		}

		Group
		{
			title: qsTr("Descriptives")

			CheckBox
			{
				name:		"numberOfEstimates"
				label:		qsTr("Number of estimates")
				info:		qsTr("Show a frequency table of the estimates in the correlation/covariance matrix input.")
			}

			CheckBox
			{
				name:		"numberOfObservations"
				label:		qsTr("Number of observations")
				info:		qsTr("Show a frequency table of the observations in the correlation/covariance matrix input.")
			}
		}

		CheckBox
		{
			label:		qsTr("Pooled correlation/covariance matrix")
			name:		"pooledCorrelationCovarianceMatrix"
			checked:	false
			info:		qsTr("Show the pooled correlation/covariance matrix. The pooled correlation/covariance matrix is from the first stage of two-stage meta-analytic SEM. ")

			DropDown
			{
				name:		"pooledCorrelationCovarianceMatrixRandomEffects"
				label:		qsTr("Random effects")
				values:
				[
					{ label: qsTr("Diagonal"),	value: "diagonal" },
					{ label: qsTr("Symmetric"),	value: "symmetric" },
					{ label: qsTr("Zero"),		value: "zero" }
				]
				info: qsTr("Type of the random effects of the correlation or covariance vectors.")
			}
		}
	}




	TabView
	{
		name:			"models"
		maximumItems:	9
		newItemName:	qsTr("Model 1")
		content: Column
		{
			spacing: 5
			TextArea
			{
				name:		        "syntax"
				id:			        syntax
				textType:	        JASP.TextTypeMetaSem
                variableSeparator:  "_"
				info:		        qsTr("Specify model using a lavaan style syntax.")
			}

			Group
			{
				columns: 2

				Group
				{
					title: qsTr("Model Settings")

					CheckBox
					{
						text:		qsTr("Replace constraints")
						name:		"replaceConstraints"
						checked:	false
						info:		qsTr("Replace constraints in the model.")
					}

					CheckBox
					{
						text:		qsTr("Fix latent variance to 1")
						name:		"fixLatentVarianceTo1"
						enabled:	dataInputType.value == "covariance"
						checked:	true
						info:		qsTr("Fix the variance of latent variables to 1. Only available for covariance input.")
					}
				}

				Group
				{
					title: qsTr("Random Effects")

					DropDown
					{
						name:		"randomEffectsSigma"
						label:		qsTr("Sigma")
						values:
						[
							{ label: qsTr("Diagonal"),	value: "diagonal" },
							{ label: qsTr("Symmetric"),	value: "symmetric" },
							{ label: qsTr("Zero"),		value: "zero" }
						]
						info: qsTr("Type of the random effects of the correlation or covariance vectors.")
					}

					DropDown
					{
						name:		"randomEffectsMu"
						label:		qsTr("Mu")
						enabled:	means.count > 0
						values:
						[
							{ label: qsTr("Symmetric"),	value: "symmetric" },
							{ label: qsTr("Diagonal"),	value: "diagonal" },
							{ label: qsTr("Zero"),		value: "zero" }
						]
						info: qsTr("Type of the random effects of the mean vectors.")
					}

					DropDown
					{
						name:		"randomEffectsSigmaMu"
						label:		qsTr("Sigma - Mu")
						enabled:	means.count > 0
						values:
						[
							{ label: qsTr("Zero"),	value: "zero" },
							{ label: qsTr("Full"),	value: "full" }
						]
						info: qsTr("Type of the random effects between the correlation/covariance vectors and the mean vectors.")
					}

				}
			}
		}

	}


	Group
	{

		CheckBox
		{
			text:		qsTr("Additional fit measures")
			name:		"additionalFitMeasures"
			checked:	false
			info:		qsTr("Show a summary of the goodness-of-fit statistics.")
		}

		CheckBox
		{
			text:		qsTr("Pairwise model comparison")
			name:		"pairwiseModelComparison"
			checked:	false
			info:		qsTr("Show a pairwise model comparison table.")
		}

		CheckBox
		{
			text:		qsTr("Model summary")
			name:		"modelSummary"
			checked:	false
			info:		qsTr("Show a summary of the model coefficients and computed estimates.")

			CheckBox
			{
				name:		"modelSummaryRegression"
				label:		qsTr("Regression")
				checked:	true
				info:		qsTr("Show regression estimates (A-Matrix) in the model summary.")
			}

			CheckBox
			{
				name:		"modelSummaryCovariances"
				label:		qsTr("Covariances")
				checked:	false
				info:		qsTr("Show covariance estimates (S-Matrix) in the model summary.")
			}

			CheckBox
			{
				name:		"modelSummaryRandomEffects"
				label:		qsTr("Random effects")
				checked:	false
				info:		qsTr("Show random effects in the model summary.")
			}

			DropDown
			{
				name:		"modelSummaryConfidenceIntervalType"
				label:		qsTr("Confidence interval type")
				values:
				[
					{ label: qsTr("Standard errors")	, value: "standardErrors"	},
					{ label: qsTr("Likelihood based")   , value: "likelihoodBased"	}
				]
				info:		qsTr("Method for computing confidence interval.")
			}

			CheckBox
			{
				label:		qsTr("Show matrix indices")
				name:		"modelSummaryShowMatrixIndices"
				checked:	false
				info:		qsTr("Show matrix indices (rows/columns) in the model summary.")
			}
		}
	}

	CheckBox
	{
		text:		qsTr("Path diagram")
		name:		"pathDiagram"
		checked:	false
		info:		qsTr("Show a path diagram of the model.")

		DropDown
		{
			name:	"pathDiagramLayout"
			label:	qsTr("Layout")
			values:
			[
				{ label: qsTr("Tree"),		value: "tree" },
				{ label: qsTr("Circle"),	value: "circle" },
				{ label: qsTr("Spring"),	value: "spring" },
				{ label: qsTr("Tree2"),		value: "tree2" },
				{ label: qsTr("Circle2"),	value: "circle2" }
			]
			info:	qsTr("Layout of the path diagram.")
		}

		CheckBox
		{
			name:		"pathDiagramShowParameters"
			label:		qsTr("Show parameters")
			checked:	false
			info:		qsTr("Show parameter names instead of the estimates in the path diagram.")
		}

		Group
		{
			DoubleField
			{
				name: "pathDiagramManifestNodeWidth"
				label: qsTr("Manifest node width")
				value: 6
				info: qsTr("Width of manifest nodes.")
			}

			DoubleField
			{
				name: "pathDiagramLatentNodeWidth"
				label: qsTr("Latent node width")
				value: 8
				info: qsTr("Width of latent nodes.")
			}

			DoubleField
			{
				name: "pathDiagramUnitVectorNodeWidth"
				label: qsTr("Unit vector node width")
				value: 8
				info: qsTr("Width of unit vector nodes.")
			}

			DoubleField
			{
				name: "pathDiagramLabelSize"
				label: qsTr("Label size")
				value: 1.3
				info: qsTr("Size of the labels.")
			}

			DoubleField
			{
				name: "pathDiagramEdgeLabelSize"
				label: qsTr("Edge label size")
				value: 0.9
				info: qsTr("Size of the edge labels.")
			}

			IntegerField
			{
				name: "pathDiagramNumberOfDigits"
				label: qsTr("Number of digits")
				value: 4
				info: qsTr("Number of digits for rounding estimates.")
			}

		}

	}

}
