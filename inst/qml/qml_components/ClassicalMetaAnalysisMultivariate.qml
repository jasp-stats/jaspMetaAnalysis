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
	title:							qsTr("Effect Size Variance-Covariance Matrix")
	property string analysisType:	"metaAnalysisMultilevelMultivariate"
	expanded:						analysisType === "effectSizeAggregation"
	info: qsTr("Options for specifying the approximate variance-covariance matrix of the effect sizes. This matrix is used to account for the correlation between effect sizes when they are not independent.")

	RadioButtonGroup
	{
		id:			varianceCovarianceMatrixType
		name:		"varianceCovarianceMatrixType"
		title:		qsTr("Type")
		info: qsTr("Type of variance-covariance matrix input method.")
		columns:	4

		RadioButton
		{
			id:			varianceCovarianceMatrixSimple
			name:		"simple"
			label:		qsTr("Simple")
			visible:	analysisType === "effectSizeAggregation"
			checked:	analysisType === "effectSizeAggregation"
			info:		qsTr("Specify a simple parametric correlation structure for sampling errors within clusters.")
		}

		RadioButton
		{
			id:			varianceCovarianceMatrixCorrelationMatrix
			name:		"correlationMatrix"
			label:		qsTr("Correlation matrix")
			checked:	analysisType === "metaAnalysisMultilevelMultivariate"
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

	Group
	{
		visible:	varianceCovarianceMatrixSimple.checked

		DropDown
		{
			name:			"varianceCovarianceMatrixSimpleStructure"
			id:				varianceCovarianceMatrixSimpleStructure
			label:			qsTr("Structure")
			startValue:		"ID"
			values:
			[
				{ label: qsTr("Independent"),									value: "ID"		},
				{ label: qsTr("Compound symmetry"),								value: "CS"		},
				{ label: qsTr("Autoregressive"),								value: "CAR"	},
				{ label: qsTr("Compound symmetry + autoregressive"),			value: "CS+CAR"	},
				{ label: qsTr("Compound symmetry × autoregressive"),			value: "CS*CAR"	}
			]
			info: qsTr("Parametric correlation structure for sampling errors within clusters. 'Compound symmetry + autoregressive' assumes a constant baseline correlation plus additional correlation that decays over time. 'Compound symmetry × autoregressive' assumes the constant correlation and the time-based decay act jointly, so correlation vanishes at large time lags.")
		}

		DoubleField
		{
			name:			"varianceCovarianceMatrixSimpleWithinClusterCorrelation"
			label:			qsTr("Within cluster correlation")
			defaultValue:	0
			min:			-1
			max:			1
			inclusive:		JASP.None
			enabled:		varianceCovarianceMatrixSimpleStructure.value === "CS" || varianceCovarianceMatrixSimpleStructure.value === "CS+CAR" || varianceCovarianceMatrixSimpleStructure.value === "CS*CAR"
			info:			qsTr("Assumed correlation between sampling errors within clusters (compound symmetry).")
		}

		Group
		{
			title:			qsTr("Time")
			enabled:		varianceCovarianceMatrixSimpleStructure.value === "CAR" || varianceCovarianceMatrixSimpleStructure.value === "CS+CAR" || varianceCovarianceMatrixSimpleStructure.value === "CS*CAR"

			DropDown
			{
				name:			"varianceCovarianceMatrixSimpleTimeVariable"
				label:			qsTr("Variable")
				source:			"allVariables"
				addEmptyValue:	true
				allowedColumns:	["scale"]
				info:			qsTr("Variable specifying the time point of each effect size, required for autoregressive (CAR) structures.")
			}

			DoubleField
			{
				name:			"varianceCovarianceMatrixSimpleTimeLag1Correlation"
				label:			qsTr("Lag 1 correlation")
				defaultValue:	0
				min:			-1
				max:			1
				inclusive:		JASP.None
				info:			qsTr("Correlation for lag-1 time points in autoregressive structures.")
			}
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
			info: qsTr("Variable specifying the time point of the effect size measurement. In case multiple time points are specified, the first time corresponds to the first condition.")
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
		visible:	!varianceCovarianceMatrixSimple.checked

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
