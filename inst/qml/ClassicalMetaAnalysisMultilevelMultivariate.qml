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
		preferredHeight: 450 * preferencesModel.uiScale

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
			info: qsTr("Variable containing the observed effect sizes.")
		}

		AssignedVariablesList
		{
			name:				"effectSizeStandardError"
			id:					effectSizeStandardError
			title:				qsTr("Effect Size Standard Error")
			singleVariable:		true
			allowedColumns:		["scale"]
			info: qsTr("Variable containing the standard errors corresponding to the effect sizes.")
		}

		DropDown
		{
			name:			"method"
			id:				method
			label:			qsTr("Method")
			startValue:		"restrictedML"
			info: qsTr("Method used to estimate heterogeneity in the meta-analysis.")
			values:			[
						{ label: qsTr("Maximum Likelihood")		, value: "maximumLikelihood"},
						{ label: qsTr("Restricted ML")			, value: "restrictedML"		}
					]
		}

		DropDown
		{
			name:				"fixedEffectTest"
			label:				qsTr("Fixed effect test")
			startValue:			"t"
			values:				[ "z", "t"]
			info: qsTr("Method for testing the model coefficients: 'z' uses standard normal approximation, 't' uses t-distribution.")
		}

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
			name:				"clustering"
			id:					clustering
			title:				qsTr("Clustering")
			singleVariable:		true
			allowedColumns:		["nominal"]
			info: qsTr("Variable indicating clustering of effect sizes. This option is disabled when permutation tests are selected.")
		}

		AssignedVariablesList
		{
			name:				"studyLabels"
			title:				qsTr("Study Labels")
			singleVariable:		true
			allowedColumns:		["nominal"]
			info: qsTr("Variable containing labels for the studies. Used for labeling outputs and plots.")
		}
	}


	Section
	{	
		title:		qsTr("Random Effects / Model Structure")
		expanded:	true
		info: qsTr("Options for specifying the random effects structure for the meta-analysis model, including the types of random effects and their associated variables. Allows modeling of complex data structures such as multilevel, multivariate, autoregressive, spatial, and other forms of dependency.")

		ComponentsList
		{
			id:				randomEffects
			name:			"randomEffects"
			headerLabels:	["Type"]
			defaultValues:	[{"type": "nested"}]

			rowComponent: RowLayout
			{
				property string typeValue:				type.value
				property string structureValue:			structure.value
				property string spatialInputTypeValue:	spatialInputType.value
				property string typeLabel:				type.currentLabel
				property string structureLabel:			structure.currentLabel
				property string structureCounterValue:	structureCounter.text

				Text
				{
					text:	qsTr("Structure %1").arg((rowIndex + 1))
					id:		structureCounter
				}
				
				DropDown
				{
					id:			type
					name:		"type"
					info: qsTr("Type of random effect to include in the model.")
					values: [
					{ label: qsTr("Simple"),				value: "simple"},
					{ label: qsTr("Nested (multilevel)"),	value: "nested"},
					{ label: qsTr("Random slopes"),			value: "randomSlopes"},
					{ label: qsTr("Structured"),			value: "structured"},
					{ label: qsTr("Autoregressive"),		value: "autoregressive"},
					{ label: qsTr("Spatial"),				value: "spatial"},
					{ label: qsTr("Known correlation"),		value: "knownCorrelation"}
					]
				}

				DropDown
				{
					id:			structure
					name:		"structure"
					label:		qsTr("Structure")
					visible:	type.value == "structured" || type.value == "autoregressive" || type.value == "spatial"
					info: qsTr("Structure of the random effect when the type is 'Structured', 'Autoregressive', or 'Spatial'. Available structures depend on the selected type.")
					values: (function() {
						if (type.value == "structured") {
							return [
								{ label: qsTr("Compound symmetry"),						value: "compoundSymmetry"},
								{ label: qsTr("Heteroscedastic compound symmetry"),		value: "heteroscedasticCompoundSymmetry"},
								{ label: qsTr("Unstructured "),							value: "unstructured"},
								{ label: qsTr("Identity"),								value: "identity"},
								{ label: qsTr("Diagonal"),								value: "diagonal"}
							];
						} else if (type.value == "autoregressive") {
							return [
								{ label: qsTr("AR(1)"),					value: "ar1"},
								{ label: qsTr("Heteroscedastic AR(1)"),	value: "heteroskedasticAr1"},
								{ label: qsTr("Continuous-time AR"),	value: "continuousTimeAr"}
							];
						} else if (type.value == "spatial") {
							return [
								{ label: qsTr("Exponential"),			value: "exponential"},
								{ label: qsTr("Gaussian"),				value: "gaussian"},
								{ label: qsTr("Linear"),				value: "linear"},
								{ label: qsTr("Rational quadratic"),	value: "rationalQuadratic"},
								{ label: qsTr("Spherical"),				value: "spherical"}
							];
						} else {
							return [];
						}
					})()
				}
			}
		}

		ComponentsList
		{
			name:		"randomEffectsSpecification"
			source:		"randomEffects"
			title:		qsTr("Specification")
			visible:	true
			rowSpacing: 20

			rowComponent: 	ColumnLayout
			{
				property var typeValue:				randomEffects.rowAt(rowIndex).typeValue
				property var structureValue:		randomEffects.rowAt(rowIndex).structureValue
				property var spatialInputTypeValue:	randomEffects.rowAt(rowIndex).spatialInputTypeValue
				property var typeLabel:				randomEffects.rowAt(rowIndex).typeLabel
				property var structureLabel:		randomEffects.rowAt(rowIndex).structureLabel
				property var structureCounterValue:	randomEffects.rowAt(rowIndex).structureCounterValue

				VariablesForm
				{
					removeInvisibles:	true
					preferredHeight:	(typeValue == "nested" || typeValue == "randomSlopes" || (typeValue == "spatial" && distanceMetric.value != "greatCircle")) ? 250 * preferencesModel.uiScale : 200 * preferencesModel.uiScale
					visible:			typeValue == "simple" || typeValue == "nested" || typeValue == "randomSlopes" || typeValue == "structured" || typeValue == "autoregressive" || typeValue == "spatial" || typeValue == "knownCorrelation"

					AvailableVariablesList
					{
						name:		"allVars"
						title:		structureCounterValue + ": " + typeLabel
					}
					
					AssignedVariablesList
					{
						name:				"randomSlopeTerms"
						title:				qsTr("Random Slope Terms")
						visible:			typeValue == "randomSlopes"
						listViewType:		JASP.Interaction
						allowedColumns:		["nominal", "scale"] // this should be choose on assignment 
						info: qsTr("Variables to include as random slope terms in the model. Available when the random effect type is 'Random slopes'.")
						addAvailableVariablesToAssigned: false
					}

					AssignedVariablesList
					{
						name:				"factorLevels"
						title:				qsTr("Factor Levels")
						visible:			typeValue == "structured"
						singleVariable:		true
						info: qsTr("Variable indicating the factor levels ('Inner Term') for the structured random effect. Available when the random effect type is 'Structured'.")
						allowedColumns:		["nominal"]
					}


					AssignedVariablesList
					{
						name:				"level1"
						title:				qsTr("Level 1")
						visible:			typeValue == "nested"
						singleVariable:		true
						allowedColumns:		["nominal"]
					}

					AssignedVariablesList
					{
						name:				"level2"
						title:				qsTr("Level 2")
						visible:			typeValue == "nested"
						singleVariable:		true
						allowedColumns:		["nominal"]
					}

					AssignedVariablesList
					{
						name:				"level3"
						title:				qsTr("Level 3")
						visible:			typeValue == "nested"
						singleVariable:		true
						allowedColumns:		["nominal"]
					}

					AssignedVariablesList
					{
						name:				"level4"
						title:				qsTr("Level 4")
						visible:			typeValue == "nested"
						singleVariable:		true
						allowedColumns:		["nominal"]
					}

					AssignedVariablesList
					{
						name:				"level5"
						title:				qsTr("Level 5")
						visible:			typeValue == "nested"
						singleVariable:		true
						allowedColumns:		["nominal"]
					}

					AssignedVariablesList
					{
						name:				"time"
						title:				qsTr("Time")
						visible:			typeValue == "autoregressive"
						singleVariable:		true
						allowedColumns:		["ordinal", "scale"] // scale for continuous time AR otherwise ordinal
						info: qsTr("Variable indicating time points for an autoregressive random effects structure. Available when the random effect type is 'Autoregressive'.")
					}

					AssignedVariablesList
					{
						name:				"spatialCoordinates"
						title:				qsTr("Spatial Coordinates")
						visible:			typeValue == "spatial" && distanceMetric.value != "greatCircle" && distanceMetric.value != "loadFromFile"
						allowedColumns:		["scale"] 
						info: qsTr("Variables representing spatial coordinates for a spatial random effects structure. Available when the random effect type is 'Spatial' and the distance metric is not 'Great-circle' or prespecified in a file.")
					}

					AssignedVariablesList
					{
						name:				"longitude"
						title:				qsTr("Longitude")
						visible:			typeValue == "spatial" && distanceMetric.value == "greatCircle"
						allowedColumns:		["scale"] 
						singleVariable:		true
						info: qsTr("Variable representing longitude (in decimal degrees, with minus signs for West) for a spatial random effects structure using the 'Great-circle' distance metric. Available when the random effect type is 'Spatial' and the distance metric is 'Great-circle'.")
					}

					AssignedVariablesList
					{
						name:				"latitude"
						title:				qsTr("Latitude")
						visible:			typeValue == "spatial" && distanceMetric.value == "greatCircle"
						allowedColumns:		["scale"] 
						singleVariable:		true
						info: qsTr("Variable representing latitude (in decimal degrees, with minus signs for South) for a spatial random effects structure using the 'Great-circle' distance metric. Available when the random effect type is 'Spatial' and the distance metric is 'Great-circle'.")
					}

					AssignedVariablesList
					{
						name:				"locationIdentifier"
						title:				qsTr("Location Identifier")
						visible:			typeValue == "spatial" && distanceMetric.value == "loadFromFile"
						allowedColumns:		["nominal"] 
						singleVariable:		true
						info: qsTr("Variable identifying locations when loading distances matrix from a file for a spatial random effects structure. The location corresponds to the row and column names of the distance matrix. The names cannot start with a number.")
					}

					AssignedVariablesList
					{
						name:				"groupingFactor"
						title:				qsTr("Grouping Factor")
						visible:			typeValue != "nested"
						singleVariable:		true
						allowedColumns:		["nominal"]
						info: qsTr("Grouping variable specifying which observations share the same random effect ('Outer Term'). Available for random effect types other than 'Nested (Multilevel)'. The 'Grouping Factor' is not required for 'Spatial' Random Effects.")
					}
				}

				DropDown
				{
					name:		"distanceMetric"
					id:			distanceMetric
					label:		qsTr("Distance metric")
					visible:	typeValue == "spatial"
					info: qsTr("Distance metric used to calculate distances in a spatial random effects structure. Available when the random effect type is 'Spatial'.")
					values:		[
						{ label: qsTr("Euclidean"),			value: "euclidean" },
						{ label: qsTr("Manhattan"),			value: "manhattan" },
						{ label: qsTr("Maximum"),			value: "maximum" },
						{ label: qsTr("Great-circle"),		value: "greatCircle"},
						{ label: qsTr("Load from file"),	value: "loadFromFile"}
					]
				}

				FileSelector
				{
					name:		"distanceMatrixFile"
					label:		qsTr("Distance matrix file")
					visible:	typeValue == "spatial" && distanceMetric.value == "loadFromFile"
					filter:		"*.csv"
					save:		false
					info: qsTr("CSV file containing the distance matrix for the spatial random effects structure. The first row and the first column of the file must contain names that map the matrix entries to the 'Location Identifier' (the names cannot start with a number). Available when the random effect type is 'Spatial' and the distance metric is loaded from a file.")

				}

				FileSelector
				{
					name:		"correlationMatrixFile"
					label:		qsTr("Correlation matrix file")
					visible:	typeValue == "knownCorrelation"
					filter:		"*.csv"
					save:		false
					info: qsTr("CSV file containing the known correlation matrix for the random effects structure. The first row and the first column of the file must contain names that map the matrix entries to the 'Grouping Factor' (the names cannot start with a number). Available when the random effect type is 'Known correlation'.")
				}

				Divider { }

			}
		}
	}

	MA.ClassicalMetaAnalysisModel
	{
		id:			sectionModel
		module:		"metaAnalysisMultilevelMultivariate"
	}

	MA.ClassicalMetaAnalysisStatistics 
	{
		module:		"metaAnalysisMultilevelMultivariate"
	}

	MA.ClassicalMetaAnalysisEstimatedMarginalMeans
	{
		module:		"metaAnalysisMultilevelMultivariate"
	}

	MA.ClassicalMetaAnalysisForestPlot
	{
		module:		"metaAnalysisMultilevelMultivariate"
	}

	MA.ClassicalMetaAnalysisBubblePlot {}

	MA.ClassicalMetaAnalysisDiagnostics
	{
		module:		"metaAnalysisMultilevelMultivariate"
	}

	MA.ClassicalMetaAnalysisAdvanced
	{
		module:		"metaAnalysisMultilevelMultivariate"
	}
}
