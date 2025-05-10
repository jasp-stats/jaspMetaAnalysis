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

	ComponentsList
	{
		id:		effectSizeType
		name:	"effectSizeType"
		headerLabels:	["Design", "Measurement", "Effect size"]
		defaultValues:	[{"design": "independentGroups"}]

		rowComponent: RowLayout
		{

			property string designValue:		design.value
			property string measurementValue:	measurement.value
			property string effectSizeValue:	effectSize.value
			property string stepCounterValue:	stepCounter.text
			property string designLabel:		design.currentLabel
			property string measurementLabel:	measurement.currentLabel

			Text
			{
				text:	qsTr("Step %1").arg((rowIndex + 1))
				id:		stepCounter
			}

			DropDown
			{
				id:			design
				name:		"design"
				values: [
				{ label: qsTr("Independent groups"),		value: "independentGroups"},
				{ label: qsTr("Variable association"),		value: "variableAssociation"},
				{ label: qsTr("Single group"),				value: "singleGroup"},
				{ label: qsTr("Repeated measures"),			value: "repeatedMeasures"},
				{ label: qsTr("Other"),						value: "other"},
				{ label: qsTr("Reported effect sizes"),		value: "reportedEffectSizes"}
			]
			}

			DropDown
			{
				id:			measurement
				name:		"measurement"
				visible:	design.value != "reportedEffectSizes"
				values: (function() {
					if (design.value === "independentGroups") {
						return [
							{ label: qsTr("Quantitative"), value: "quantitative"},
							{ label: qsTr("Binary"), value: "binary"},
							{ label: qsTr("Counts per time"), value: "countsPerTime"},
							{ label: qsTr("Mixed"), value: "mixed"}
						];
					} else if (design.value === "variableAssociation") {
						return [
							{ label: qsTr("Quantitative"), value: "quantitative"},
							{ label: qsTr("Binary"), value: "binary"},
							{ label: qsTr("Mixed"), value: "mixed"}
						];
					} else if (design.value === "singleGroup") {
						return [
							{ label: qsTr("Quantitative"), value: "quantitative"},
							{ label: qsTr("Binary"), value: "binary"},
							{ label: qsTr("Counts per time"), value: "countsPerTime"}
						];
					} else if (design.value === "repeatedMeasures") {
						return [
							{ label: qsTr("Quantitative"), value: "quantitative"},
							{ label: qsTr("Binary"), value: "binary"},
							{ label: qsTr("Binary (marginal)"), value: "binaryMarginal"}
						];
					} else if (design.value === "other") {
						return [
							{ label: qsTr("Reliability"), value: "reliability"},
							{ label: qsTr("Partial correlation"), value: "partialCorrelation"},
							{ label: qsTr("Model fit"), value: "modelFit"},
							{ label: qsTr("Heterozygosity"), value: "heterozygosity"}
						];
					} else {
						return [];
					}
				})()
			}

			DropDown
			{
				id:			effectSize
				name:		"effectSize"
				visible:	design.value != "reportedEffectSizes"
				indexDefaultValue: (function() {
					if (design.value === "independentGroups" && measurement.value === "quantitative")
						return 1;
					else if (design.value === "independentGroups" && measurement.value === "binary")
						return 1;
					else if (design.value === "variableAssociation" && measurement.value === "quantitative")
						return 2;
					else if (design.value === "variableAssociation" && measurement.value === "mixed")
						return 2;
					else if (design.value === "singleGroup")
						return 1;
					else if (design.value === "repeatedMeasures" && measurement.value === "quantitative")
						return 1;
					else if (design.value === "repeatedMeasures" && measurement.value === "binary")
						return 1;
					else if (design.value === "other" && measurement.value === "reliability")
						return 1;
					else if (design.value === "other" && measurement.value === "partialCorrelation")
						return 1;
					else if (design.value === "other" && measurement.value === "modelFit")
						return 1;
					else
						return 0;
				})()
				values: (function() {
					if (design.value === "independentGroups" && measurement.value === "quantitative") {
						return [
							{ label: qsTr("MD"), value: "MD"},
							{ label: qsTr("SMD"), value: "SMD"},
							{ label: qsTr("SMDH"), value: "SMDH"},
							{ label: qsTr("SMD1"), value: "SMD1"},
							{ label: qsTr("SMD1H"), value: "SMD1H"},
							{ label: qsTr("ROM"), value: "ROM"},
							{ label: qsTr("CVR"), value: "CVR"},
							{ label: qsTr("VR"), value: "VR"}
						];
					} else if (design.value === "independentGroups" && measurement.value === "binary") {
						return [
							{ label: qsTr("RR"), value: "RR"},
							{ label: qsTr("OR"), value: "OR"},
							{ label: qsTr("RD"), value: "RD"},
							{ label: qsTr("AS"), value: "AS"},
							{ label: qsTr("PETO"), value: "PETO"}
						];
					} else if (design.value === "independentGroups" && measurement.value === "countsPerTime") {
						return [
							{ label: qsTr("IRR"), value: "IRR"},
							{ label: qsTr("IRD"), value: "IRD"},
							{ label: qsTr("IRSD"), value: "IRSD"}
						];
					} else if (design.value === "independentGroups" && measurement.value === "mixed") {
						return [
							{ label: qsTr("D2ORN"), value: "D2ORN"},
							{ label: qsTr("D2ORL"), value: "D2ORL"},
							{ label: qsTr("PBIT"), value: "PBIT"},
							{ label: qsTr("OR2DN"), value: "OR2DN"},
							{ label: qsTr("OR2DL"), value: "OR2DL"}
						];
					} else if (design.value === "variableAssociation" && measurement.value === "quantitative") {
						return [
							{ label: qsTr("COR"), value: "COR"},
							{ label: qsTr("UCOR"), value: "UCOR"},
							{ label: qsTr("ZCOR"), value: "ZCOR"}
						];
					} else if (design.value === "variableAssociation" && measurement.value === "binary") {
						return [
							{ label: qsTr("OR"), value: "OR"},
							{ label: qsTr("PHI"), value: "PHI"},
							{ label: qsTr("YUQ"), value: "YUQ"},
							{ label: qsTr("YUY"), value: "YUY"},
							{ label: qsTr("RTET"), value: "RTET"},
							{ label: qsTr("ZPHI"), value: "ZPHI"},
							{ label: qsTr("ZTET"), value: "ZTET"}
						];
					} else if (design.value === "variableAssociation" && measurement.value === "mixed") {
						return [
							{ label: qsTr("RPB"), value: "RPB"},
							{ label: qsTr("RBIS"), value: "RBIS"},
							{ label: qsTr("ZPB"), value: "ZPB"},
							{ label: qsTr("ZBIS"), value: "ZBIS"}
						];
					} else if (design.value === "singleGroup" && measurement.value === "quantitative") {
						return [
							{ label: qsTr("MN"), value: "MN"},
							{ label: qsTr("SMN"), value: "SMN"},
							{ label: qsTr("MNLN"), value: "MNLN"},
							{ label: qsTr("CVLN"), value: "CVLN"},
							{ label: qsTr("SDLN"), value: "SDLN"}
						];
					} else if (design.value === "singleGroup" && measurement.value === "binary") {
						return [
							{ label: qsTr("PR"), value: "PR"},
							{ label: qsTr("PLN"), value: "PLN"},
							{ label: qsTr("PLO"), value: "PLO"},
							{ label: qsTr("PAS"), value: "PAS"},
							{ label: qsTr("PFT"), value: "PFT"}
						];
					} else if (design.value === "singleGroup" && measurement.value === "countsPerTime") {
						return [
							{ label: qsTr("IR"), value: "IR"},
							{ label: qsTr("IRLN"), value: "IRLN"},
							{ label: qsTr("IRS"), value: "IRS"},
							{ label: qsTr("IRFT"), value: "IRFT"}
						];
					} else if (design.value === "repeatedMeasures" && measurement.value === "quantitative") {
						return [
							{ label: qsTr("MC"), value: "MC"},
							{ label: qsTr("SMCC"), value: "SMCC"},
							{ label: qsTr("SMCR"), value: "SMCR"},
							{ label: qsTr("SMCRH"), value: "SMCRH"},
							{ label: qsTr("SMCRP"), value: "SMCRP"},
							{ label: qsTr("SMCRPH"), value: "SMCRPH"},
							{ label: qsTr("ROMC"), value: "ROMC"},
							{ label: qsTr("CVRC"), value: "CVRC"},
							{ label: qsTr("VRC"), value: "VRC"}
						];
					} else if (design.value === "repeatedMeasures" && measurement.value === "binary") {
						return [
							{ label: qsTr("MPRR"), value: "MPRR"},
							{ label: qsTr("MPOR"), value: "MPOR"},
							{ label: qsTr("MPRD"), value: "MPRD"},
							{ label: qsTr("MPORC"), value: "MPORC"},
							{ label: qsTr("MPPETO"), value: "MPPETO"}
						];
					} else if (design.value === "repeatedMeasures" && measurement.value === "binaryMarginal") {
						return [
							{ label: qsTr("MPORM"), value: "MPORM"}
						];
					}else if (design.value === "other" && measurement.value === "reliability") {
						return [
							{ label: qsTr("ARAW"), value: "ARAW"},
							{ label: qsTr("AHW"), value: "AHW"},
							{ label: qsTr("ABT"), value: "ABT"}
						];
					} else if (design.value === "other" && measurement.value === "partialCorrelation") {
						return [
							{ label: qsTr("PCOR"), value: "PCOR"},
							{ label: qsTr("ZPCOR"), value: "ZPCOR"},
							{ label: qsTr("SPCOR"), value: "SPCOR"},
							{ label: qsTr("ZSPCOR"), value: "ZSPCOR"}
						];
					} else if (design.value === "other" && measurement.value === "modelFit") {
						return [
							{ label: qsTr("R2"), value: "R2"},
							{ label: qsTr("ZR2"), value: "ZR2"}
						];
					} else if (design.value === "other" && measurement.value === "heterozygosity") {
						return [
							{ label: qsTr("REH"), value: "REH"}
						];
					}  else {
						return [];
					}
				})();
			}

		}
	}


	ComponentsList
	{
		name:		"variables"
		source:		"effectSizeType"
		rowSpacing: 20

		rowComponent: 	ColumnLayout
		{
			property var designValue:		effectSizeType.rowAt(rowIndex).designValue
			property var measurementValue:	effectSizeType.rowAt(rowIndex).measurementValue
			property var effectSizeValue:	effectSizeType.rowAt(rowIndex).effectSizeValue
			property var designLabel:		effectSizeType.rowAt(rowIndex).designLabel
			property var measurementLabel:	effectSizeType.rowAt(rowIndex).measurementLabel
			property var effectSizeLabel:	effectSizeType.rowAt(rowIndex).effectSizeLabel
			property var stepCounterValue:	effectSizeType.rowAt(rowIndex).stepCounterValue

			VariablesForm
			{
				// TODO: dynamically set proper height
				removeInvisibles:	true
				preferredWidth:		parent.width - 6 * jaspTheme.contentMargin
				preferredHeight:	(function() {
					if ((designValue === "variableAssociation" && measurementValue === "mixed" && samplingVarianceType.value === "mixed")) {
						return 12 * 50 * preferencesModel.uiScale
					} else if (effectSizeValue === "SMD" || effectSizeValue === "D2ORL" || effectSizeValue === "D2ORN" || effectSizeValue === "SMCC" || 
						(designValue === "variableAssociation" && measurementValue === "mixed")) {
						return 11 * 50 * preferencesModel.uiScale
					} else if (effectSizeValue === "CVR" || effectSizeValue === "VR" || effectSizeValue === "CVRC"  || effectSizeValue === "VRC" || 
						(designValue === "independentGroups" && measurementValue === "countsPerTime") ||
						(designValue === "repeatedMeasures" && measurementValue === "binary") ||
						(designValue === "variableAssociation" && measurementValue === "quantitative") ||
						(designValue === "reportedEffectSizes")) {
						return  6 * 50 * preferencesModel.uiScale
					} else if (effectSizeValue === "SDLN" || (designValue === "singleGroup" && measurementValue === "countsPerTime")) {
						return  4 * 50 * preferencesModel.uiScale
					} else if (effectSizeValue === "SMD1" || effectSizeValue === "SMCR" || effectSizeValue === "PCOR" || effectSizeValue === "ZPCOR" ||
						(designValue === "other" && measurementValue === "modelFit")) {
						return  7 * 50 * preferencesModel.uiScale
					} else if ((designValue === "independentGroups" && measurementValue === "quantitative") ||
						(designValue === "other" && measurementValue === "partialCorrelation")) {
						return  8 * 50 * preferencesModel.uiScale
					} else if ((designValue === "singleGroup" && (measurementValue === "quantitative" || measurementValue === "binary")) ||
						(designValue === "other" && (measurementValue === "reliability" ||  measurementValue === "heterozygosity"))) {
						return  5 * 50 * preferencesModel.uiScale
					} else {
						return 8 * 50 * preferencesModel.uiScale
					}
				})()

				AvailableVariablesList
				{
					name:		"allVars"
					title:		stepCounterValue + ": " + effectSizeValue + " (" + designLabel + "/" + measurementLabel + ")"
				}

				AssignedVariablesList
				{ // metafor: ai
					name: "group1OutcomePlus"
					title: qsTr("Group 1/Outcome +")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "independentGroups" && measurementValue === "binary") ||
						(designValue === "independentGroups" && measurementValue === "mixed" && (effectSizeValue === "PBIT" || effectSizeValue === "OR2DN" || effectSizeValue === "OR2DL"))
				}

				AssignedVariablesList
				{ // metafor: ai
					name: "time1OutcomePlus"
					title: qsTr("Time 1/Outcome +")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "repeatedMeasures" && measurementValue === "binaryMarginal")
				}

				AssignedVariablesList
				{ // metafor: ai
					name: "outcomePlusPlus"
					title: qsTr("Outcome +/+")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "variableAssociation" && measurementValue === "binary") ||
						(designValue === "repeatedMeasures" && measurementValue === "binary")
				}

				AssignedVariablesList
				{ // metafor: ai
					name: "coefficientAlpha"
					title: qsTr("Coefficient α")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "other" &&  measurementValue === "reliability")
				}

				AssignedVariablesList
				{ // metafor: ai
					name: "homozygousDominantAlleles"
					title: qsTr("Homozygous Dominant Alleles")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "other" && measurementValue === "heterozygosity")
				}

				AssignedVariablesList
				{ // metafor: bi
					name: "group1OutcomeMinus"
					title: qsTr("Group 1/Outcome -")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "independentGroups" && measurementValue === "binary") ||
						(designValue === "independentGroups" && measurementValue === "mixed" && (effectSizeValue === "PBIT" || effectSizeValue === "OR2DN" || effectSizeValue === "OR2DL"))
				}

				AssignedVariablesList
				{ // metafor: bi
					name: "time1OutcomeMinus"
					title: qsTr("Time 1/Outcome -")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "repeatedMeasures" && measurementValue === "binaryMarginal")
				}

				AssignedVariablesList
				{ // metafor: bi
					name: "outcomePlusMinus"
					title: qsTr("Outcome +/-")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "variableAssociation" && measurementValue === "binary") ||
						(designValue === "repeatedMeasures" && measurementValue === "binary")
				}

				AssignedVariablesList
				{ // metafor: bi
					name: "heterozygousAlleles"
					title: qsTr("Heterozygous Alleles")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "other" && measurementValue === "heterozygosity")
				}

				AssignedVariablesList
				{ // metafor: ci
					name: "group2OutcomePlus"
					title: qsTr("Group 2/Outcome +")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "independentGroups" && measurementValue === "binary") ||
						(designValue === "independentGroups" && measurementValue === "mixed" && (effectSizeValue === "PBIT" || effectSizeValue === "OR2DN" || effectSizeValue === "OR2DL"))
				}

				AssignedVariablesList
				{ // metafor: ci
					name: "time2OutcomePlus"
					title: qsTr("Time 2/Outcome +")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "repeatedMeasures" && measurementValue === "binaryMarginal")
				}

				AssignedVariablesList
				{ // metafor: ci
					name: "outcomeMinusPlus"
					title: qsTr("Outcome -/+")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "variableAssociation" && measurementValue === "binary") ||
						(designValue === "repeatedMeasures" && measurementValue === "binary")
				}

				AssignedVariablesList
				{ // metafor: ci
					name: "homozygousRecessiveAlleles"
					title: qsTr("Homozygous Recessive Alleles")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "other" && measurementValue === "heterozygosity")
				}

				AssignedVariablesList
				{ // metafor: di
					name: "group2OutcomeMinus"
					title: qsTr("Group 2/Outcome Minus")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "independentGroups" && measurementValue === "binary") ||
						(designValue === "independentGroups" && measurementValue === "mixed" && (effectSizeValue === "PBIT" || effectSizeValue === "OR2DN" || effectSizeValue === "OR2DL"))
				}

				AssignedVariablesList
				{ // metafor: di
					name: "time2OutcomeMinus"
					title: qsTr("Time 2/Outcome -")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "repeatedMeasures" && measurementValue === "binaryMarginal")
				}

				AssignedVariablesList
				{ // metafor: di
					name: "outcomeMinusMinus"
					title: qsTr("Outcome -/-")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "variableAssociation" && measurementValue === "binary") ||
						(designValue === "repeatedMeasures" && measurementValue === "binary")
				}

				AssignedVariablesList
				{ // metafor: n1i
					name: "outcomePlusPlusAndPlusMinus"
					title: qsTr("Outcome +/+ and +/-")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "variableAssociation" && measurementValue === "binary")
				}

				AssignedVariablesList
				{ // metafor: n2i
					name: "outcomeMinusPlusAndMinusMinus"
					title: qsTr("Outcome -/+ and -/-")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "variableAssociation" && measurementValue === "binary")
				}

				AssignedVariablesList
				{ // metafor: x1i
					name: "eventsGroup1"
					title: qsTr("Events Group 1")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "independentGroups" && measurementValue === "countsPerTime")
				}

				AssignedVariablesList
				{ // metafor: xi
					name: "events"
					title: qsTr("Events")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "singleGroup" && (measurementValue === "binary" || measurementValue === "countsPerTime"))
				}

				AssignedVariablesList
				{ // metafor: mi
					name: "nonEvents"
					title: qsTr("Non-Events")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "singleGroup" && measurementValue === "binary")
				}

				AssignedVariablesList
				{ // metafor: mi
					name: "items"
					title: qsTr("Items")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "other" &&  measurementValue === "reliability")
				}

				AssignedVariablesList
				{ // metafor: mi
					name: "predictors"
					title: qsTr("Predictors")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "other" &&  measurementValue === "partialCorrelation") ||
						(designValue === "other" && measurementValue === "modelFit")
				}

				AssignedVariablesList
				{ // metafor: x2i
					name: "eventsGroup2"
					title: qsTr("Events Group 2")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "independentGroups" && measurementValue === "countsPerTime")
				}

				AssignedVariablesList
				{ // metafor: t1i
					name: "personTimeGroup1"
					title: qsTr("Person-Time Group 1")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "independentGroups" && measurementValue === "countsPerTime")
				}

				AssignedVariablesList
				{ // metafor: ti
					name: "personTime"
					title: qsTr("Person-Time")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "singleGroup" && measurementValue === "countsPerTime")
				}

				AssignedVariablesList
				{ // metafor: t2i
					name: "personTimeGroup2"
					title: qsTr("Person-Time Group 2")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "independentGroups" && measurementValue === "countsPerTime")
				}

				AssignedVariablesList
				{ // metafor: m1i
					name: "meanGroup1"
					title: qsTr("Mean Group 1")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "independentGroups" && measurementValue === "quantitative" && (effectSizeValue != "CVR" && effectSizeValue != "VR")) || 
						(designValue === "independentGroups" && measurementValue === "mixed" && (effectSizeValue === "D2ORN" || effectSizeValue === "D2ORL")) ||
						(designValue === "variableAssociation" && measurementValue === "mixed")
				}

				AssignedVariablesList
				{ // metafor: m1i
					name: "meanTime1"
					title: qsTr("Mean Time 1 (or Group 1)")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "repeatedMeasures" && measurementValue === "quantitative" && (effectSizeValue != "CVRC" && effectSizeValue != "VRC"))
				}

				AssignedVariablesList
				{ // metafor: m2i
					name: "meanGroup2"
					title: qsTr("Mean Group 2")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "independentGroups" && measurementValue === "quantitative" && (effectSizeValue != "CVR" && effectSizeValue != "VR")) || 
						(designValue === "independentGroups" && measurementValue === "mixed" && (effectSizeValue === "D2ORN" || effectSizeValue === "D2ORL")) ||
						(designValue === "variableAssociation" && measurementValue === "mixed")
				}

				AssignedVariablesList
				{ // metafor: m2i
					name: "meanTime2"
					title: qsTr("Mean Time 2 (or Group 2)")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "repeatedMeasures" && measurementValue === "quantitative" && (effectSizeValue != "CVRC" && effectSizeValue != "VRC"))
				}

				AssignedVariablesList
				{ // metafor: mi
					name: "mean"
					title: qsTr("Mean")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "singleGroup" && measurementValue === "quantitative" && effectSizeValue != "SDLN")
				}

				AssignedVariablesList
				{ // metafor: sd1i
					name: "sdGroup1"
					title: qsTr("SD Group 1")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "independentGroups" && measurementValue === "quantitative" && (effectSizeValue != "SMD1" && effectSizeValue != "SMDH1")) || 
						(designValue === "independentGroups" && measurementValue === "mixed" && (effectSizeValue === "D2ORN" || effectSizeValue === "D2ORL")) ||
						(designValue === "variableAssociation" && measurementValue === "mixed")
				}

				AssignedVariablesList
				{ // metafor: sd1i
					name: "sdTime1"
					title: qsTr("SD Time 1 (or Group 1)")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "repeatedMeasures" && measurementValue === "quantitative")
				}

				AssignedVariablesList
				{ // metafor: sd2i
					name: "sdGroup2"
					title: qsTr("SD Group 2")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "independentGroups" && measurementValue === "quantitative") || 
						(designValue === "independentGroups" && measurementValue === "mixed" && (effectSizeValue === "D2ORN" || effectSizeValue === "D2ORL")) ||
						(designValue === "variableAssociation" && measurementValue === "mixed")
				}

				AssignedVariablesList
				{ // metafor: sd2i
					name: "sdTime2"
					title: qsTr("SD Time 2 (or Group 2)")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "repeatedMeasures" && measurementValue === "quantitative"  && effectSizeValue != "SMCR")
				}

				AssignedVariablesList
				{ // metafor: sdi
					name: "sd"
					title: qsTr("SD")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "singleGroup" && measurementValue === "quantitative")
				}
				AssignedVariablesList
				{ // metafor: n1i
					name: "sampleSizeGroup1"
					title: qsTr("Sample Size Group 1")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "independentGroups" && measurementValue != "countsPerTime") ||
						(designValue === "variableAssociation" && measurementValue === "mixed")
				}
				
				AssignedVariablesList
				{ // metafor: n2i
					name: "sampleSizeGroup2"
					title: qsTr("Sample Size Group 2")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "independentGroups" && measurementValue != "countsPerTime") ||
						(designValue === "variableAssociation" && measurementValue === "mixed")
				}
				
				AssignedVariablesList
				{ // metafor: ri
					name: "correlation"
					title: qsTr("Correlation")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "variableAssociation" && measurementValue === "quantitative") ||
						(designValue === "repeatedMeasures" && measurementValue === "quantitative") ||
						(designValue === "repeatedMeasures" && measurementValue === "binaryMarginal")
				}

				AssignedVariablesList
				{ // metafor: pi
					name: "proportionPlusPlus"
					title: qsTr("Proportion +/+")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "repeatedMeasures" && measurementValue === "binaryMarginal")
				}

				AssignedVariablesList
				{ // metafor: ni
					name: "sampleSize"
					title: qsTr("Sample Size")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "variableAssociation" && measurementValue === "quantitative") ||
						(designValue === "singleGroup" && (measurementValue === "quantitative" || measurementValue === "binary")) ||
						(designValue === "repeatedMeasures" && measurementValue === "quantitative") ||
						(designValue === "other" && measurementValue === "reliability") ||
						(designValue === "other" && measurementValue === "partialCorrelation") ||
						(designValue === "other" && measurementValue === "modelFit")

				}

				AssignedVariablesList
				{ // metafor: di
					name: "cohensD"
					title: qsTr("Cohen's d")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "independentGroups" && measurementValue === "quantitative" && effectSizeValue === "SMD") ||
						(designValue === "independentGroups" && measurementValue === "mixed" && (effectSizeValue === "D2ORN" || effectSizeValue === "D2ORL")) ||
						(designValue === "variableAssociation" && measurementValue === "mixed") ||
						(designValue === "repeatedMeasures" && measurementValue === "quantitative" && effectSizeValue === "SMCC")
				}

				AssignedVariablesList
				{ // metafor: r2i
					name: "rSquared"
					title: qsTr("R-Squared")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "other" && measurementValue === "partialCorrelation" && (effectSizeValue === "SPCOR" || effectSizeValue === "ZSPCOR")) ||
						(designValue === "other" && measurementValue === "modelFit")
				}

				AssignedVariablesList
				{ // metafor: ti
					name: "tStatistic"
					title: qsTr("T-Statistic")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "independentGroups" && measurementValue === "quantitative" && effectSizeValue === "SMD") ||
						(designValue === "independentGroups" && measurementValue === "mixed" && (effectSizeValue === "D2ORN" || effectSizeValue === "D2ORL")) ||
						(designValue === "variableAssociation" && measurementValue === "quantitative") ||
						(designValue === "variableAssociation" && measurementValue === "mixed") ||
						(designValue === "repeatedMeasures" && measurementValue === "quantitative" && effectSizeValue === "SMCC") ||
						(designValue === "other" && measurementValue === "partialCorrelation")
				}

				AssignedVariablesList
				{ // metafor: fi
					name: "fStatistic"
					title: qsTr("F-Statistic")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "other" && measurementValue === "modelFit")
				}

				AssignedVariablesList
				{ // metafor: ri
					name: "semipartialCorrelation"
					title: qsTr("(Semi)Partial Correlation")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "other" && measurementValue === "partialCorrelation")
				}

				AssignedVariablesList
				{ // metafor: pi
					name: "pValue"
					title: qsTr("P-Value")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: (designValue === "independentGroups" && measurementValue === "quantitative" && effectSizeValue === "SMD") ||
						(designValue === "independentGroups" && measurementValue === "mixed" && (effectSizeValue === "D2ORN" || effectSizeValue === "D2ORL")) ||
						(designValue === "variableAssociation" && measurementValue === "quantitative") ||
						(designValue === "variableAssociation" && measurementValue === "mixed") ||
						(designValue === "repeatedMeasures" && measurementValue === "quantitative" && effectSizeValue === "SMCC") ||
						(designValue === "other" && measurementValue === "partialCorrelation") ||
						(designValue === "other" && measurementValue === "modelFit")
				}


				AssignedVariablesList
				{ // metafor: yi
					name: "effectSize"
					title: qsTr("Effect Size")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: designValue === "reportedEffectSizes"
				}

				AssignedVariablesList
				{ // metafor: sei
					name: "standardError"
					title: qsTr("Standard Error")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: designValue === "reportedEffectSizes"
				}

				AssignedVariablesList
				{ // metafor: vi
					name: "samplingVariance"
					title: qsTr("Sampling Variance")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: designValue === "reportedEffectSizes"
				}

				AssignedPairsVariablesList
				{
					name: "confidenceInterval"
					title: qsTr("95% Confidence Interval")
					singleVariable: true
					allowedColumns:	["scale"]
					visible: designValue === "reportedEffectSizes"
				}

				AssignedVariablesList
				{ // metafor: vtype for mixed designs with PHI or ZPHI
					name: "samplingVarianceTypeMixed"
					title: qsTr("Sampling Variance Type Mixed")
					singleVariable: true
					allowedColumns:	["scale"]
					visible:  (designValue === "variableAssociation" && measurementValue === "binary" && (effectSizeValue === "PHI" || effectSizeValue === "ZPHI") && samplingVarianceType.value === "mixed") ||
						(designValue === "variableAssociation" && measurementValue === "mixed" && (effectSizeValue === "RPB" || effectSizeValue === "ZPB") && samplingVarianceType.value === "mixed")
				}

				AssignedVariablesList
				{
					name: "subset"
					id: subset
					title: qsTr("Subset")
					singleVariable: true
					allowedColumns:	["nominal"]
				}

				DropDown
				{
					name:				"subsetLevel"
					label:				qsTr("Subset Level")
					values:				subset.levels
				}
			}

			Group
			{
				title: qsTr("Frequency/event cell adjustment")
				columns: 3
				visible: (designValue === "independentGroups" && measurementValue === "binary") ||
						(designValue === "independentGroups" && measurementValue === "countsPerTime") ||
						(designValue === "independentGroups" && measurementValue === "mixed"  && (effectSizeValue === "PBIT" || effectSizeValue === "OR2DN" || effectSizeValue === "OR2DL")) ||
						(designValue === "variableAssociation" && measurementValue === "binary") ||
						(designValue === "singleGroup" && measurementValue === "binary") ||
						(designValue === "singleGroup" && measurementValue === "countsPerTime")

				DoubleField
				{
					label: qsTr("Add")
					name: "add"
					enabled: to.value != "none"
					defaultValue: (effectSizeValue === "AS" || effectSizeValue === "PHI" || effectSizeValue === "ZPHI" || 
					effectSizeValue === "RTET" || effectSizeValue === "ZTET" || effectSizeValue === "IRSD" ||
					effectSizeValue === "PAS" || effectSizeValue === "PFT" || effectSizeValue === "IRS"  || effectSizeValue === "IRFT" ) ? 0 : 0.5
				}

				DropDown
				{
					name: "to"
					id: to
					label: qsTr("To")
					startValue: "onlyZero"
					values: [
						{ label: qsTr("All"), value: "all" },
						{ label: qsTr("Only zero"), value: "onlyZero" },
						{ label: qsTr("If any zero"), value: "ifAnyZero" },
						{ label: qsTr("None"), value: "none" }
					]
					//TODO: defaultValue: "onlyZero"
				}

				RadioButtonGroup
				{
					name: "dropStudiesWithNoCasesOrEvents"
					title: qsTr("Drop studies with no cases/events")
					columns: 2
					radioButtonsOnSameRow: true

					RadioButton
					{
						value: "yes"
						label: qsTr("Yes")
					}

					RadioButton
					{
						value: "no"
						label: qsTr("No")
						checked: true
					}
				}
			}

			DropDown
			{
				name:		"samplingVarianceType"
				id:			samplingVarianceType
				label:		qsTr("Sampling variance type")
				values: (function() {
					if (designValue === "independentGroups" && measurementValue === "quantitative") {
						if (effectSizeValue === "MD") {
							return [
									{ label: qsTr("LS"), value: "LS" },
									{ label: qsTr("HO"), value: "HO" }
								];	
						} else if (effectSizeValue === "SMD" || effectSizeValue === "SMD1") {
							return [
									{ label: qsTr("LS"), value: "LS" },
									{ label: qsTr("LS2"), value: "LS2" },
									{ label: qsTr("UB"), value: "UB" },
									{ label: qsTr("AV"), value: "AV" }
								];	
						}  else if (effectSizeValue === "ROM") {
							return [
									{ label: qsTr("LS"), value: "LS" },
									{ label: qsTr("HO"), value: "HO" },
									{ label: qsTr("AV"), value: "AV" },
									{ label: qsTr("AVHO"), value: "AVHO" }
								];	
						} else {
							return [];
						}
					} else if (designValue === "variableAssociation" && measurementValue === "quantitative") {
						if (effectSizeValue === "COR" || effectSizeValue === "ZCOR") {
							return [
									{ label: qsTr("LS"), value: "LS" },
									{ label: qsTr("AV"), value: "AV" }
								];	
						} else if (effectSizeValue === "UCOR") {
							return [
									{ label: qsTr("LS"), value: "LS" },
									{ label: qsTr("UB"), value: "UB" },
									{ label: qsTr("AV"), value: "AV" }
								];
						} else {
							return [];
						} 
					} else if (designValue === "variableAssociation" && measurementValue === "binary") {
						if (effectSizeValue === "PHI" || effectSizeValue === "ZPHI") {
							return [
									{ label: qsTr("ST"), value: "ST" },
									{ label: qsTr("CS"), value: "CS" },
									{ label: qsTr("mixed"), value: "mixed" }
								];	
						} else {
							return [];
						} 
					} else if (designValue === "variableAssociation" && measurementValue === "mixed") {
						if (effectSizeValue === "RPB" || effectSizeValue === "ZPB") {
							return [
									{ label: qsTr("ST"), value: "ST" },
									{ label: qsTr("CS"), value: "CS" },
									{ label: qsTr("mixed"), value: "mixed" }
								];	
						} else {
							return [];
						}
					} else if (designValue === "other" && measurementValue === "modelFit") {
						return [
									{ label: qsTr("LS"), value: "LS" },
									{ label: qsTr("LS2"), value: "LS2" },
									{ label: qsTr("AV"), value: "AV" },
									{ label: qsTr("AV2"), value: "AV2" }
								];	
					} else {
						return [];
					}
				})()
				visible: (designValue === "independentGroups" && measurementValue === "quantitative" && (effectSizeValue === "MD" || effectSizeValue === "SMD" || effectSizeValue === "SMD1" || effectSizeValue === "ROM")) ||
						(designValue === "variableAssociation" && measurementValue === "quantitative" && (effectSizeValue === "COR" || effectSizeValue === "ZCOR" || effectSizeValue === "UCOR")) ||
						(designValue === "variableAssociation" && measurementValue === "binary"  && (effectSizeValue === "PHI" || effectSizeValue === "ZPHI")) ||
						(designValue === "variableAssociation" && measurementValue === "mixed"  && (effectSizeValue === "RPB" || effectSizeValue === "ZPB")) ||
						(designValue === "other" && measurementValue === "modelFit")
			}
			
			Divider { }

		}

	}

	Section
	{
		title: qsTr("Options")

		CheckBox
		{
			id:			computeSamplingVariance
			name:		"computeSamplingVariance"
			text:		qsTr("Compute sampling variance")
			checked:	false
		}

		Group
		{
			title:	qsTr("Computed Columns Names")

			TextField
			{
				name:			"computedColumnsNamesEffectSize"
				label:			qsTr("Effect size")
				defaultValue:	"computed effect size"
			}

			TextField
			{
				name:			"computedcolumnsNamesStandardError"
				label:			qsTr("Standard error")
				defaultValue:	"computed standard error"
				visible:		!computeSamplingVariance.checked
			}

			TextField
			{
				name:			"computedcolumnsNamesSamplingVariance"
				label:			qsTr("Sampling variance")
				defaultValue:	"computed sampling variance"
				visible:		computeSamplingVariance.checked
			}

			TextField
			{
				name:			"computedColumnsNamesEffectSizeType"
				label:			qsTr("Effect size type")
				defaultValue:	"computed effect size type"
			}

		}

	}

}
