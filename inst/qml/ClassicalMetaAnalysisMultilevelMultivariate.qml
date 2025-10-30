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
	info: qsTr("Classical multilevel/multivariate meta-analysis allows you to conduct a meta-analysis adjusting for the dependency between effect sizes using the classical approach. " + 
	"The effect size dependency can be adjusted for by specifying the 'Effect Size Variance-Covariance Matrix' (interfacing the 'vcalc' function) and specifying the 'Random Effects/Model Components' (interfacing the 'random' argument in 'rma.mv' function). " +
	"It provides options for fixed and random effects models, as well as meta-regression, and subgroup analysis. " +
	"Additional options include the ability to specify clustering for robust variance estimation, permutation tests, and generating the metafor package R code. " +
	"The results include estimates of effect sizes, heterogeneity, moderation, and various plots to visualize the results.\n\n" + 
	"See [this tutorial](https://doi.org/10.48550/arXiv.2509.09845) for a detailed introduction to the module.")
	infoBottom: "## " + qsTr("References") + "\n" +
	"- Bartoš F, Wagenmakers EJ, & Viechtbauer W (2025). “Meta-analysis with JASP, Part I: Classical approaches.” _ArXiv Preprint_. https://doi.org/10.48550/arXiv.2509.09845\n" + 
	"- Viechtbauer W (2010). “Conducting meta-analyses in R with the metafor package.” _Journal of Statistical Software, 36_(3), 1–48. https://doi.org/10.18637/jss.v036.i03\n" +
	"- Viechtbauer W, López-López JA, Sánchez-Meca J, Marín-Martínez F (2015). “A comparison of procedures to test for moderators in mixed-effects meta-regression models.” _Psychological Methods, 20_(3), 360–374. https://doi.org/10.1037/met0000023\n" +
	"- Viechtbauer W (2025). _metafor: Meta-Analysis Package for R_. R package version 4.8-0 Available at: <https://CRAN.R-project.org/package=metafor>.\n" +
	"## " + qsTr("R Packages") + "\n" +
	"- metafor"

	VariablesForm
	{
		preferredHeight: 525 * preferencesModel.uiScale

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
		
		AssignedVariablesList
		{
			name:				"subgroup"
			id:					subgroup
			title:				qsTr("Subgroup")
			singleVariable:		true
			allowedColumns:		["nominal"]
			info: qsTr("Variable indicating subgroup stratification. For each subgroup, an independent model is fitted to the corresponding data set subset.")
		}
	}

	Section
	{	
		title:		qsTr("Effect Size Variance-Covariance Matrix")
		expanded:	false
		info: qsTr("Options for specifying the approximate variance-covariance matrix of the effect sizes. This matrix is used to account for the correlation between effect sizes when they are not independent.")

		RadioButtonGroup
		{
			id:			varianceCovarianceMatrixType
			name:		"varianceCovarianceMatrixType"
			title:		qsTr("Type")
			info: qsTr("Type of variance-covariance matrix input method.")
			columns:	3

			RadioButton
			{
				id:			varianceCovarianceMatrixCorrelationMatrix
				name:		"correlationMatrix"
				label:		qsTr("Correlation matrix")
				checked:	true
				info: qsTr("Use a list of variables to specify the correlation matrix of studies corresponding to the same cluster. Corresponds to the `rvars` option in the metafor's 'vcalc' function.")
			}

			RadioButton
			{
				id:			varianceCovarianceMatrixConstructsGroupsTimes
				name:		"constructsGroupsTimes"
				label:		qsTr("Constructs, groups, and times")
				info: qsTr("Specify constructs, groups, and times of the measurement to specify the correlation matrix of studies corresponding to the same cluster.")
			}

			RadioButton
			{
				id:			varianceCovarianceMatrixPrecomputed
				name:		"precomputed"
				label:		qsTr("Precomputed")
				info: qsTr("Load a csv file containing the precomputed variance-covariance matrix.")
			}
		}
		
		FileSelector
		{
			name:		"varianceCovarianceMatrixFile"
			label:		qsTr("Effect size variance-covariance matrix file")
			visible:	varianceCovarianceMatrixPrecomputed.checked
			filter:		"*.csv"
			save:		false
			info: qsTr("CSV file containing the precomputed effect size variance-covariance matrix. The matrix needs to match the dimensions of the data set and cannot contain any other variables or names.")
		}


		VariablesForm
		{
			removeInvisibles:	true
			preferredHeight: 	(varianceCovarianceMatrixCorrelationMatrix.checked ? 250 : 525) * preferencesModel.uiScale
			visible:			varianceCovarianceMatrixConstructsGroupsTimes.checked || varianceCovarianceMatrixCorrelationMatrix.checked	

			AvailableVariablesList
			{
				name:				"varianceCovarianceMatrixAllVariables"
			}
				
			AssignedVariablesList
			{
				name:				"varianceCovarianceMatrixCorrelationMatrix"
				title:				qsTr("Correlation Matrix")
				allowedColumns:		["scale"]
				allowTypeChange:	true
				singleVariable:		false
				visible:			varianceCovarianceMatrixCorrelationMatrix.checked
				property bool active:	varianceCovarianceMatrixCorrelationMatrix.checked
				onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
				info: qsTr("Variable specifying the correlation between the individual estimates within cluster. The column order of the variable correspond to the row order of effect size estimates within cluster. Only the lower triangle needs to be specified. Corresponds to the `rvars` option in the metafor's 'vcalc' function.")
			}

			AssignedVariablesList
			{
				name:				"varianceCovarianceMatrixConstruct"
				id:					varianceCovarianceMatrixConstruct
				title:				qsTr("Construct")
				allowedColumns:		["nominal"]
				allowTypeChange:	true
				singleVariable:		true
				visible:			varianceCovarianceMatrixConstructsGroupsTimes.checked
				property bool active:	varianceCovarianceMatrixConstructsGroupsTimes.checked
				onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
				info: qsTr("Variable specifying the construct measured by the effect size. Corresponds to the `obs` option in the metafor's 'vcalc' function.")
			}

			AssignedVariablesList
			{
				name:				"varianceCovarianceMatrixConstructType"
				id:					varianceCovarianceMatrixConstructType
				title:				qsTr("Construct Type")
				allowedColumns:		["nominal"]
				allowTypeChange:	true
				singleVariable:		true
				visible:			varianceCovarianceMatrixConstructsGroupsTimes.checked
				property bool active:	varianceCovarianceMatrixConstructsGroupsTimes.checked
				onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
				info: qsTr("Variable specifying the type of construct measured by the effect size. Construct Type corresponds to a higher level grouping of the Constructs. Corresponds to the `type` option in the metafor's 'vcalc' function.")
			}

			AssignedVariablesList
			{
				name:				"varianceCovarianceMatrixTime1"
				id:					varianceCovarianceMatrixTime1
				title:				qsTr("Time 1")
				allowedColumns:		["scale"]
				allowTypeChange:	true
				singleVariable:		true
				visible:			varianceCovarianceMatrixConstructsGroupsTimes.checked
				property bool active:	varianceCovarianceMatrixConstructsGroupsTimes.checked
				onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
				info: qsTr("Variable specifying the time point of the effect size measurement. In case multiple time points are specified, the first time corresponds to the first codntion.")
			}			

			AssignedVariablesList
			{
				name:				"varianceCovarianceMatrixTime2"
				enabled:			varianceCovarianceMatrixTime1.count != 0
				title:				qsTr("Time 2")
				allowedColumns:		["scale"]
				allowTypeChange:	true
				singleVariable:		true
				visible:			varianceCovarianceMatrixConstructsGroupsTimes.checked
				property bool active:	varianceCovarianceMatrixConstructsGroupsTimes.checked
				onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
				info: qsTr("Variable specifying the time point of the effect size measurement. In case multiple time points are specified, the second time corresponds to the second condition.")
			}			

			AssignedVariablesList
			{
				name:				"varianceCovarianceMatrixGroup1"
				id:					varianceCovarianceMatrixGroup1
				title:				qsTr("Group 1")
				allowedColumns:		["nominal"]
				allowTypeChange:	true
				singleVariable:		true
				visible:			varianceCovarianceMatrixConstructsGroupsTimes.checked
				property bool active:	varianceCovarianceMatrixConstructsGroupsTimes.checked
				onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
				info: qsTr("Variable to specify the group of the first condition when the observed effect sizes or outcomes represent contrasts between two conditions. Corresponds to the `grp1` argument in the metafor's 'vcalc' function.")
			}

			AssignedVariablesList
			{
				name:				"varianceCovarianceMatrixGroup2"
				id:					varianceCovarianceMatrixGroup2
				enabled:			varianceCovarianceMatrixGroup1.count != 0
				title:				qsTr("Group 2")
				allowedColumns:		["nominal"]
				allowTypeChange:	true
				singleVariable:		true
				visible:			varianceCovarianceMatrixConstructsGroupsTimes.checked
				property bool active:	varianceCovarianceMatrixConstructsGroupsTimes.checked
				onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
				info: qsTr("Variable to specify the group of the second condition when the observed effect sizes or outcomes represent contrasts between two conditions. Corresponds to the `grp1` argument in the metafor's 'vcalc' function.")
			}

			AssignedVariablesList
			{
				name:				"varianceCovarianceMatrixGroupSize1"
				enabled:			varianceCovarianceMatrixGroup1.count != 0
				title:				qsTr("Group Size 1")
				allowedColumns:		["scale"]
				allowTypeChange:	true
				singleVariable:		true
				visible:			varianceCovarianceMatrixConstructsGroupsTimes.checked
				property bool active:	varianceCovarianceMatrixConstructsGroupsTimes.checked
				onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
				info: qsTr("Variable to specify the size of the group (or more generally, the inverse-sampling variance weight) of the first condition when the observed effect sizes or outcomes represent contrasts between two conditions. Corresponds to the `w1` argument in the metafor's 'vcalc' function.")
			}

			AssignedVariablesList
			{
				name:				"varianceCovarianceMatrixGroupSize2"
				enabled:			varianceCovarianceMatrixGroup2.count != 0
				title:				qsTr("Group Size 2")
				allowedColumns:		["scale"]
				allowTypeChange:	true
				singleVariable:		true
				visible:			varianceCovarianceMatrixConstructsGroupsTimes.checked
				property bool active:	varianceCovarianceMatrixConstructsGroupsTimes.checked
				onActiveChanged: 		if (!active && count > 0) itemDoubleClicked(0)
				info: qsTr("Optional numeric vector to specify the size of the group (or more generally, the inverse-sampling variance weight) of the second condition when the observed effect sizes or outcomes represent contrasts between two conditions. Corresponds to the `w2` argument in the metafor's 'vcalc' function.")
			}

			AssignedVariablesList
			{
				name:				"varianceCovarianceMatrixSubcluster"
				enabled:			varianceCovarianceMatrixCluster.count != 0
				title:				qsTr("Subcluster")
				singleVariable:		true
				allowedColumns:		["nominal"]
				info: qsTr("Variable specifying additional structure of the subgroups. Effect sizes within the same cluster with different values of the cluster variable are assumed to be independent. Note that this input corresponds to the 'subgroup' option in the metafor's 'vcalc' function and is renamed for differentiation from subgroup analysis. ")
			}

			AssignedVariablesList
			{
				name:				"varianceCovarianceMatrixCluster"
				id:					varianceCovarianceMatrixCluster
				title:				qsTr("Cluster")
				singleVariable:		true
				allowedColumns:		["nominal"]
				info: qsTr("Variable specifying clustering of the effect sizes for computing the variance covariance matrix. Effect sizes with different values of the cluster variable are assumed to be independent. Note that this input differs from the 'Clustering' option in the data input which is used to specify cluster-robust standard error. In most cases however, both input should contain the same variable.")
			}
		}

		RadioButtonGroup
		{
			title:		qsTr("Construct Correlation Matrix")
			name: 		"varianceCovarianceMatrixConstructCorrelationMatrix"
			visible:	varianceCovarianceMatrixConstructsGroupsTimes.checked
			enabled:	varianceCovarianceMatrixConstructsGroupsTimes.checked && varianceCovarianceMatrixConstruct.count != 0
			columns:	1

			RadioButton
			{
				name:		"commonCorrelation"
				label:		qsTr("Common correlation")
				info: qsTr("Specify the correlation between the same construct levels.")
				childrenOnSameRow: true
				checked: true

				DoubleField
				{
					name:				"varianceCovarianceMatrixConstructCorrelationMatrixValue"
					defaultValue:		0
					min: 				-1
					max: 				1
					inclusive: 			JASP.None
				}
			}

			RadioButton
			{
				name:		"correlationMatrix"
				label:		qsTr("Correlation matrix")
				info: qsTr("CSV file containing the correlation matrix between the constructs levels. The first row and the first column of the file must contain names that map the matrix entries to the construct level names (the names cannot start with a number).")
				childrenOnSameRow: true

				FileSelector
				{
					name:		"varianceCovarianceMatrixConstructCorrelationMatrixFilePath"
					filter:		"*.csv"
					save:		false
				}
			}
		}

		RadioButtonGroup
		{
			title:		qsTr("Construct Type Correlation Matrix")
			name: 		"varianceCovarianceMatrixConstructTypeCorrelationMatrix"
			visible:	varianceCovarianceMatrixConstructsGroupsTimes.checked
			enabled:	varianceCovarianceMatrixConstructType.count != 0
			columns:	1

			RadioButton
			{
				name:		"commonCorrelation"
				label:		qsTr("Common correlation")
				info: qsTr("Specify the correlation between the same construct levels.")
				childrenOnSameRow: true
				checked: true

				DoubleField
				{
					name:				"varianceCovarianceMatrixConstructTypeCorrelationMatrixValue"
					defaultValue:		0
					min: 				-1
					max: 				1
					inclusive: 			JASP.None
				}
			}

			RadioButton
			{
				name:		"correlationMatrix"
				label:		qsTr("Correlation matrix")
				info: qsTr("CSV file containing the correlation matrix between the constructs type level. The first row and the first column of the file must contain names that map the matrix entries to the construct level type names (the names cannot start with a number).")
				childrenOnSameRow: true

				FileSelector
				{
					name:		"varianceCovarianceMatrixConstructTypeCorrelationMatrixFilePath"
					filter:		"*.csv"
					save:		false
				}
			}
		}

		DoubleField
		{
			label:				qsTr("Time lag 1 correlation")
			visible:			varianceCovarianceMatrixConstructsGroupsTimes.checked
			enabled:			varianceCovarianceMatrixTime1.count != 0
			name:				"varianceCovarianceMatrixTimeLag1Correlation"
			defaultValue:		0
			min: 				-1
			max: 				1
			inclusive: 			JASP.None
		}

		Group
		{
			CheckBox
			{
				label:		qsTr("Check positive definiteness")
				name:		"varianceCovarianceMatrixCheckPositiveDefiniteness"
				checked: 	true
				info: qsTr("Check if the variance-covariance matrix is symmetric.")
			}

			CheckBox
			{
				label:		qsTr("Force positive definiteness")
				name:		"varianceCovarianceMatrixForcePositiveDefiniteness"
				checked: 	false
				info: qsTr("Force the variance-covariance matrix to be positive definite. This option shuld be used with caution as non-positive definite matricies often indicate input misspecification.")
			}
		}

		FileSelector
		{
			label:		qsTr("Save computed variance-covariance matrix")
			visible:	varianceCovarianceMatrixConstructsGroupsTimes.checked || varianceCovarianceMatrixCorrelationMatrix.checked
			name:		"varianceCovarianceMatrixSaveComputedVarianceCovarianceMatrix"
			filter:		"*.csv"
			save:		true
		}
	}


	Section
	{	
		title:		qsTr("Random Effects / Model Components")
		expanded:	true
		info: qsTr("Options for specifying the random effects components for the meta-analysis model, including the types of random effects and their associated variables. Allows modeling of complex data structures such as multilevel, multivariate, autoregressive, spatial, and other forms of dependency.")

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
					text:	qsTr("Component %1").arg((rowIndex + 1))
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
					visible:	type.value === "structured" || type.value === "autoregressive" || type.value === "spatial"
					info: qsTr("Structure of the random effect when the type is 'Structured', 'Autoregressive', or 'Spatial'. Available structures depend on the selected type.")
					values: (function() {
						if (type.value === "structured") {
							return [
								{ label: qsTr("Compound symmetry"),						value: "compoundSymmetry"},
								{ label: qsTr("Heteroscedastic compound symmetry"),		value: "heteroscedasticCompoundSymmetry"},
								{ label: qsTr("Unstructured "),							value: "unstructured"},
								{ label: qsTr("Identity"),								value: "identity"},
								{ label: qsTr("Diagonal"),								value: "diagonal"}
							];
						} else if (type.value === "autoregressive") {
							return [
								{ label: qsTr("AR(1)"),					value: "ar1"},
								{ label: qsTr("Heteroscedastic AR(1)"),	value: "heteroskedasticAr1"},
								{ label: qsTr("Continuous-time AR"),	value: "continuousTimeAr"}
							];
						} else if (type.value === "spatial") {
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
					preferredHeight:	(typeValue === "nested" || typeValue === "randomSlopes" || (typeValue === "spatial" && distanceMetric.value != "greatCircle")) ? 250 * preferencesModel.uiScale : 200 * preferencesModel.uiScale
					visible:			typeValue === "simple" || typeValue === "nested" || typeValue === "randomSlopes" || typeValue === "structured" || typeValue === "autoregressive" || typeValue === "spatial" || typeValue === "knownCorrelation"

					AvailableVariablesList
					{
						name:		"allVars"
						title:		structureCounterValue + ": " + typeLabel
					}
					
					AssignedVariablesList
					{
						name:				"randomSlopeTerms"
						title:				qsTr("Random Slope Terms")
						visible:			typeValue === "randomSlopes"
						listViewType:		JASP.Interaction
						allowedColumns:		["nominal", "scale"] // this should be choose on assignment 
						info: qsTr("Variables to include as random slope terms in the model. Available when the random effect type is 'Random slopes'.")
						addAvailableVariablesToAssigned: false
					}

					AssignedVariablesList
					{
						name:				"factorLevels"
						title:				qsTr("Factor Levels")
						visible:			typeValue === "structured"
						singleVariable:		true
						info: qsTr("Variable indicating the factor levels ('Inner Term') for the structured random effect. Available when the random effect type is 'Structured'.")
						allowedColumns:		["nominal"]
					}


					AssignedVariablesList
					{
						name:				"level1"
						title:				qsTr("Level 1")
						visible:			typeValue === "nested"
						singleVariable:		true
						allowedColumns:		["nominal"]
					}

					AssignedVariablesList
					{
						name:				"level2"
						title:				qsTr("Level 2")
						visible:			typeValue === "nested"
						singleVariable:		true
						allowedColumns:		["nominal"]
					}

					AssignedVariablesList
					{
						name:				"level3"
						title:				qsTr("Level 3")
						visible:			typeValue === "nested"
						singleVariable:		true
						allowedColumns:		["nominal"]
					}

					AssignedVariablesList
					{
						name:				"level4"
						title:				qsTr("Level 4")
						visible:			typeValue === "nested"
						singleVariable:		true
						allowedColumns:		["nominal"]
					}

					AssignedVariablesList
					{
						name:				"level5"
						title:				qsTr("Level 5")
						visible:			typeValue === "nested"
						singleVariable:		true
						allowedColumns:		["nominal"]
					}

					AssignedVariablesList
					{
						name:				"time"
						title:				qsTr("Time")
						visible:			typeValue === "autoregressive"
						singleVariable:		true
						allowedColumns:		["ordinal", "scale"] // scale for continuous time AR otherwise ordinal
						info: qsTr("Variable indicating time points for an autoregressive random effects structure. Available when the random effect type is 'Autoregressive'.")
					}

					AssignedVariablesList
					{
						name:				"spatialCoordinates"
						title:				qsTr("Spatial Coordinates")
						visible:			typeValue === "spatial" && distanceMetric.value != "greatCircle" && distanceMetric.value != "loadFromFile"
						allowedColumns:		["scale"] 
						info: qsTr("Variables representing spatial coordinates for a spatial random effects structure. Available when the random effect type is 'Spatial' and the distance metric is not 'Great-circle' or prespecified in a file.")
					}

					AssignedVariablesList
					{
						name:				"longitude"
						title:				qsTr("Longitude")
						visible:			typeValue === "spatial" && distanceMetric.value === "greatCircle"
						allowedColumns:		["scale"] 
						singleVariable:		true
						info: qsTr("Variable representing longitude (in decimal degrees, with minus signs for West) for a spatial random effects structure using the 'Great-circle' distance metric. Available when the random effect type is 'Spatial' and the distance metric is 'Great-circle'.")
					}

					AssignedVariablesList
					{
						name:				"latitude"
						title:				qsTr("Latitude")
						visible:			typeValue === "spatial" && distanceMetric.value === "greatCircle"
						allowedColumns:		["scale"] 
						singleVariable:		true
						info: qsTr("Variable representing latitude (in decimal degrees, with minus signs for South) for a spatial random effects structure using the 'Great-circle' distance metric. Available when the random effect type is 'Spatial' and the distance metric is 'Great-circle'.")
					}

					AssignedVariablesList
					{
						name:				"locationIdentifier"
						title:				qsTr("Location Identifier")
						visible:			typeValue === "spatial" && distanceMetric.value === "loadFromFile"
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
					visible:	typeValue === "spatial"
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
					visible:	typeValue === "spatial" && distanceMetric.value === "loadFromFile"
					filter:		"*.csv"
					save:		false
					info: qsTr("CSV file containing the distance matrix for the spatial random effects structure. The first row and the first column of the file must contain names that map the matrix entries to the 'Location Identifier' (the names cannot start with a number). Available when the random effect type is 'Spatial' and the distance metric is loaded from a file.")

				}

				FileSelector
				{
					name:		"correlationMatrixFile"
					label:		qsTr("Correlation matrix file")
					visible:	typeValue === "knownCorrelation"
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
		id:				sectionModel
		analysisType:	"metaAnalysisMultilevelMultivariate"
	}

	MA.ClassicalMetaAnalysisStatistics 
	{
		analysisType:	"metaAnalysisMultilevelMultivariate"
	}

	MA.ClassicalMetaAnalysisEstimatedMarginalMeans
	{
		analysisType:	"metaAnalysisMultilevelMultivariate"
	}

	MA.ForestPlot
	{
		analysisType:	"metaAnalysisMultilevelMultivariate"
	}

	MA.BubblePlot {}

	MA.ClassicalMetaAnalysisDiagnostics
	{
		analysisType:	"metaAnalysisMultilevelMultivariate"
	}

	MA.ClassicalMetaAnalysisAdvanced
	{
		analysisType:	"metaAnalysisMultilevelMultivariate"
	}
}
