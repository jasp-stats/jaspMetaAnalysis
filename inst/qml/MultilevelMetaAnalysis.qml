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
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0

// All Analysis forms must be built with the From QML item
Form
{
	

	ComponentsList
	{
		id:		randomEffects
		name:	"randomEffects"
		title:	qsTr("Random effects")

		rowComponent: RowLayout
		{
			property string typeValue:		type.value
			property string structureValue:	structure.value
			property string spatialInputTypeValue:	spatialInputType.value

			DropDown
			{
				id:			type
				name:		"type"
				label:		qsTr("Type")
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
				values: (function() {
					if (type.value == "structured") {
						return [
							{ label: qsTr("Compound symmetry"),						value: "compoundSymmetry"},
							{ label: qsTr("Heteroscedastic compound symmetry"),		value: "heteroscedasticCompoundSymmetry"},
							{ label: qsTr("Unstructured "),							value: "Unstructured"},
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

			DropDown
			{
				id:			spatialInputType
				name:		"spatialInputType"
				label:		qsTr("Spatial input type")
				visible:	type.value == "spatial"
				values: (function() {
					if (type.value == "spatial") {
						return [
							{ label: qsTr("Compute from variables"),	value: "computeFromVariables"},
							{ label: qsTr("Load from file"),			value: "loadFromFile"}
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
		title:		qsTr("Random effects specification")
		rowSpacing: 20

		rowComponent: 	ColumnLayout
		{
			property var typeValue:				randomEffects.rowAt(rowIndex).typeValue
			property var structureValue:		randomEffects.rowAt(rowIndex).structureValue
			property var spatialInputTypeValue:	randomEffects.rowAt(rowIndex).spatialInputTypeValue

			VariablesForm
			{
				removeInvisibles:	true
				preferredHeight:	(typeValue == "randomSlopes" || typeValue == "spatial") ? 250 * preferencesModel.uiScale : 200 * preferencesModel.uiScale
				visible:			typeValue == "simple" || typeValue == "randomSlopes" || typeValue == "structured" || typeValue == "autoregressive" || (typeValue == "spatial" && spatialInputTypeValue == "computeFromVariables") || typeValue == "knownCorrelation"

				AvailableVariablesList
				{
					name:		"allVars"
					title:		typeValue + ": " + structureValue
				}

				AssignedVariablesList
				{
					name:				"randomSlopeTerms"
					title:				qsTr("Random Slope Terms")
					visible:			typeValue == "randomSlopes"
					listViewType:		JASP.Interaction
					suggestedColumns:	["nominal", "scale"] // this should be choose on assignment 
				}

				AssignedVariablesList
				{
					name:				"randomLevels"
					title:				qsTr("Random Levels")
					visible:			typeValue == "structured"
					singleVariable:		true
					suggestedColumns:	["nominal"]
				}

				AssignedVariablesList
				{
					name:				"time"
					title:				qsTr("Time")
					visible:			typeValue == "autoregressive"
					singleVariable:		true
					suggestedColumns:	["ordinal", "scale"] // scale for continuous time AR otherwise ordinal
				}

				AssignedVariablesList
				{
					name:				"spatialCoordinates"
					title:				qsTr("Spatial Coordinates")
					visible:			typeValue == "spatial" && spatialInputTypeValue == "computeFromVariables"
					suggestedColumns:	["nominal"] 
				}

				AssignedVariablesList
				{
					name:				"groupingFactor"
					title:				qsTr("Grouping Factor")
					singleVariable:		true
					suggestedColumns:	["nominal"]
				}
			}

			// TODO: Bruno -- adding variable crashes the qml
			// TODO: Bruno -- allow single variable only, set-type to nominal
			FactorsForm
			{
				name:				"nestedGroupingFactors"
				id:					nestedGroupingFactors
				title:				qsTr("Nested Grouping Factors")
				preferredHeight:	200 * preferencesModel.uiScale 
				initNumberFactors:	1
				allowAll:			true
				visible:			typeValue == "nested"
			}

			DropDown
			{
				name:		"distanceMetric"
				id:			distanceMetric
				label:		qsTr("Distance metric")
				visible:	typeValue == "spatial" && spatialInputTypeValue == "computeFromVariables"
				values:		[
					{ label: qsTr("Euclidean"),			value: "euclidean" },
					{ label: qsTr("Manhattan"),			value: "manhattan" },
					{ label: qsTr("Maximum"),			value: "maximum" },
					{ label: qsTr("Great-circle"),		value: "greatCircle"}
				]
			}

			FileSelector
			{
				name:		"distanceMatrixFile"
				label:		qsTr("Distance matrix file")
				visible:	typeValue == "spatial" && spatialInputTypeValue == "loadFromFile"
				filter:		"*.csv"
			}

			FileSelector
			{
				name:		"knownCorrelationMatrixFile"
				label:		qsTr("Known correlation matrix file")
				visible:	typeValue == "knownCorrelation"
				filter:		"*.csv"
			}

		}
	}




}
