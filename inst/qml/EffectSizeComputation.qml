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

	ComponentsList
	{
		id:		effectSizeType
		name:	"effectSizeType"

		rowComponent: RowLayout
		{

			property string designValue:		design.value
			property string measurementValue:	measurement.value
			property string effectSizeValue:	effectSize.value
			property string designLabel:		design.label
			property string measurementLabel:	measurement.label
			property string effectSizeLabel:	effectSize.label

			DropDown
			{
				id:			design
				name:		"design"
				values: [
				{ label: qsTr("Independent groups"),				value: "independentGroups"},
				{ label: qsTr("Variable association"),				value: "variableAssociation"},
				{ label: qsTr("Single group"),						value: "singleGroup"},
				{ label: qsTr("Repeated measures/matched groups"),	value: "repeatedMeasuresOrMatchedGroups"},
				{ label: qsTr("Other"),								value: "other"}
			]
			}

			DropDown
			{
				id:			measurement
				name:		"measurement"
				enabled:	design.value != "other"
				values: (function() {
					if (design.value == "independentGroups") {
						return [
							{ label: qsTr("Quantitative"), value: "quantitative"},
							{ label: qsTr("Binary"), value: "binary"},
							{ label: qsTr("Counts per time"), value: "countsPerTime"},
							{ label: qsTr("Mixed"), value: "mixed"}
						];
					} else if (design.value == "variableAssociation") {
						return [
							{ label: qsTr("Quantitative"), value: "quantitative"},
							{ label: qsTr("Binary"), value: "binary"},
							{ label: qsTr("Mixed"), value: "mixed"}
						];
					} else if (design.value == "singleGroup") {
						return [
							{ label: qsTr("Quantitative"), value: "quantitative"},
							{ label: qsTr("Binary"), value: "binary"},
							{ label: qsTr("Counts per time"), value: "countsPerTime"}
						];
					} else if (design.value == "repeatedMeasuresOrMatchedGroups") {
						return [
							{ label: qsTr("Quantitative"), value: "quantitative"},
							{ label: qsTr("Binary"), value: "binary"}
						];
					} else {
						return [];
					}
				})()
			}

			DropDown
			{
				id: effectSize
				name: "effectSize"
				values: (function() {
					if (design.value == "independentGroups" && measurement.value == "quantitative") {
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
					} else if (design.value == "independentGroups" && measurement.value == "binary") {
						return [
							{ label: qsTr("RR"), value: "RR"},
							{ label: qsTr("OR"), value: "OR"},
							{ label: qsTr("RD"), value: "RD"},
							{ label: qsTr("AS"), value: "AS"},
							{ label: qsTr("PETO"), value: "PETO"}
						];
					} else if (design.value == "independentGroups" && measurement.value == "countsPerTime") {
						return [
							{ label: qsTr("IRR"), value: "IRR"},
							{ label: qsTr("IRD"), value: "IRD"},
							{ label: qsTr("IRSD"), value: "IRSD"}
						];
					} else if (design.value == "independentGroups" && measurement.value == "mixed") {
						return [
							{ label: qsTr("D2ORN"), value: "D2ORN"},
							{ label: qsTr("D2ORL"), value: "D2ORL"},
							{ label: qsTr("PBIT"), value: "PBIT"},
							{ label: qsTr("OR2DN"), value: "OR2DN"},
							{ label: qsTr("OR2DL"), value: "OR2DL"}
						];
					} else if (design.value == "variableAssociation" && measurement.value == "quantitative") {
						return [
							{ label: qsTr("COR"), value: "COR"},
							{ label: qsTr("UCOR"), value: "UCOR"},
							{ label: qsTr("ZCOR"), value: "ZCOR"}
						];
					} else if (design.value == "variableAssociation" && measurement.value == "binary") {
						return [
							{ label: qsTr("OR"), value: "OR"},
							{ label: qsTr("PHI"), value: "PHI"},
							{ label: qsTr("YUQ"), value: "YUQ"},
							{ label: qsTr("YUY"), value: "YUY"},
							{ label: qsTr("RTET"), value: "RTET"}
						];
					} else if (design.value == "variableAssociation" && measurement.value == "mixed") {
						return [
							{ label: qsTr("RPB"), value: "RPB"},
							{ label: qsTr("RBIS"), value: "RBIS"},
							{ label: qsTr("ZPB"), value: "ZPB"},
							{ label: qsTr("ZBIS"), value: "ZBIS"}
						];
					} else if (design.value == "singleGroup" && measurement.value == "quantitative") {
						return [
							{ label: qsTr("MN"), value: "MN"},
							{ label: qsTr("SMN"), value: "SMN"},
							{ label: qsTr("MNLN"), value: "MNLN"},
							{ label: qsTr("CVLN"), value: "CVLN"},
							{ label: qsTr("SDLN"), value: "SDLN"}
						];
					} else if (design.value == "singleGroup" && measurement.value == "binary") {
						return [
							{ label: qsTr("PR"), value: "PR"},
							{ label: qsTr("PLN"), value: "PLN"},
							{ label: qsTr("PLO"), value: "PLO"},
							{ label: qsTr("PAS"), value: "PAS"},
							{ label: qsTr("PFT"), value: "PFT"}
						];
					} else if (design.value == "singleGroup" && measurement.value == "countsPerTime") {
						return [
							{ label: qsTr("IR"), value: "IR"},
							{ label: qsTr("IRLN"), value: "IRLN"},
							{ label: qsTr("IRS"), value: "IRS"},
							{ label: qsTr("IRFT"), value: "IRFT"}
						];
					} else if (design.value == "repeatedMeasuresOrMatchedGroups" && measurement.value == "quantitative") {
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
					} else if (design.value == "repeatedMeasuresOrMatchedGroups" && measurement.value == "binary") {
						return [
							{ label: qsTr("MPRR"), value: "MPRR"},
							{ label: qsTr("MPOR"), value: "MPOR"},
							{ label: qsTr("MPRD"), value: "MPRD"},
							{ label: qsTr("MPORC"), value: "MPORC"},
							{ label: qsTr("MPPETO"), value: "MPPETO"},
							{ label: qsTr("MPORM"), value: "MPORM"}
						];
					} else if (design.value == "other") {
						return [
							{ label: qsTr("ARAW"), value: "ARAW"},
							{ label: qsTr("AHW"), value: "AHW"},
							{ label: qsTr("ABT"), value: "ABT"},
							{ label: qsTr("PCOR"), value: "PCOR"},
							{ label: qsTr("ZPCOR"), value: "ZPCOR"},
							{ label: qsTr("SPCOR"), value: "SPCOR"},
							{ label: qsTr("ZSPCOR"), value: "ZSPCOR"},
							{ label: qsTr("R2"), value: "R2"},
							{ label: qsTr("ZR2"), value: "ZR2"},
							{ label: qsTr("REH"), value: "REH"}
						];
					} else {
						return [];
					}
				})();
			}

		}
	}


	ComponentsList
	{
		name:		"vars"
		source:		"effectSizeType"

		rowComponent: 	VariablesForm
		{
			property var designValue:		effectSizeType.rowAt(rowIndex).designValue
			property var measurementValue:	effectSizeType.rowAt(rowIndex).measurementValue
			property var effectSizeValue:	effectSizeType.rowAt(rowIndex).effectSizeValue
			property var designLabel:		effectSizeType.rowAt(rowIndex).designLabel
			property var measurementLabel:	effectSizeType.rowAt(rowIndex).measurementLabel
			property var effectSizeLabel:	effectSizeType.rowAt(rowIndex).effectSizeLabel

			AvailableVariablesList
			{
				name:		"allVars"
				title:		"" + effectSizeValue + " (" + designValue + "/" + measurementValue + ")"
			}

			AssignedVariablesList
			{ // metafor: ai
				name: "nGroup1Outcome1"
				title: qsTr("N: Group 1/Outcome 1")
				singleVariable: true
				visible: (designValue == "independentGroups" && measurementValue == "binary") ||
						(designValue == "independentGroups" && measurementValue == "mixed" && (effectSizeValue == "D2ORN" || effectSizeValue == "D2ORL"))
			}

			AssignedVariablesList
			{ // metafor: ai
				name: "nVariable1Outcome+"
				title: qsTr("N: Variable 1/Outcome +")
				singleVariable: true
				visible: (designValue == "variableAssociation" && measurementValue == "binary")
			}

			AssignedVariablesList
			{ // metafor: ai
				name: "nTreatment1Outcome1Treatment2Outcome2+"
				title: qsTr("N: Treatment 1 + Outcome 1 / Treatment 2 + Outcome 1")
				singleVariable: true
				visible: (designValue == "repeatedMeasuresOrMatchedGroups" && measurementValue == "binary")
			}

			AssignedVariablesList
			{ // metafor: ai
				name: "alpha"
				title: qsTr("Alpha")
				singleVariable: true
				visible: (designValue == "other" && (effectSizeValue == "ARAW" || effectSizeValue == "AHW" || effectSizeValue == "ABT"))
			}

			AssignedVariablesList
			{ // metafor: ai
				name: "nHomozygousDominantAlleles"
				title: qsTr("N: Homozygous Dominant Alleles")
				singleVariable: true
				visible: (designValue == "other" && effectSizeValue == "REH")
			}

			AssignedVariablesList
			{ // metafor: bi
				name: "nGroup1Outcome2"
				title: qsTr("N: Group 1/Outcome 2")
				singleVariable: true
				visible: (designValue == "independentGroups" && measurementValue == "binary") ||
						(designValue == "independentGroups" && measurementValue == "mixed" && (effectSizeValue == "D2ORN" || effectSizeValue == "D2ORL"))
			}

			AssignedVariablesList
			{ // metafor: bi
				name: "nVariable1Outcome-"
				title: qsTr("N: Variable 1/Outcome -")
				singleVariable: true
				visible: (designValue == "variableAssociation" && measurementValue == "binary")
			}

			AssignedVariablesList
			{ // metafor: bi
				name: "nTreatment1Outcome2Treatment2Outcome1+"
				title: qsTr("N: Treatment 1 + Outcome 2 / Treatment 2 + Outcome 1")
				singleVariable: true
				visible: (designValue == "repeatedMeasuresOrMatchedGroups" && measurementValue == "binary")
			}

			AssignedVariablesList
			{ // metafor: bi
				name: "beta"
				title: qsTr("Beta")
				singleVariable: true
				visible: (designValue == "other" && (effectSizeValue == "ARAW" || effectSizeValue == "AHW" || effectSizeValue == "ABT"))
			}

			AssignedVariablesList
			{ // metafor: bi
				name: "nHomozygousRecessiveAlleles"
				title: qsTr("N: Homozygous Recessive Alleles")
				singleVariable: true
				visible: (designValue == "other" && effectSizeValue == "REH")
			}

			AssignedVariablesList
			{ // metafor: ci
				name: "nGroup2Outcome1"
				title: qsTr("N: Group 2/Outcome 1")
				singleVariable: true
				visible: (designValue == "independentGroups" && measurementValue == "binary") ||
						(designValue == "independentGroups" && measurementValue == "mixed" && (effectSizeValue == "D2ORN" || effectSizeValue == "D2ORL"))
			}

			AssignedVariablesList
			{ // metafor: ci
				name: "nVariable2Outcome+"
				title: qsTr("N: Variable 2/Outcome +")
				singleVariable: true
				visible: (designValue == "variableAssociation" && measurementValue == "binary")
			}

			AssignedVariablesList
			{ // metafor: ci
				name: "nTreatment2Outcome1Treatment1Outcome2+"
				title: qsTr("N: Treatment 2 + Outcome 1 / Treatment 1 + Outcome 2")
				singleVariable: true
				visible: (designValue == "repeatedMeasuresOrMatchedGroups" && measurementValue == "binary")
			}

			AssignedVariablesList
			{ // metafor: ci
				name: "gamma"
				title: qsTr("Gamma")
				singleVariable: true
				visible: (designValue == "other" && (effectSizeValue == "ARAW" || effectSizeValue == "AHW" || effectSizeValue == "ABT"))
			}

			AssignedVariablesList
			{ // metafor: ci
				name: "nHeterozygousAlleles"
				title: qsTr("N: Heterozygous Alleles")
				singleVariable: true
				visible: (designValue == "other" && effectSizeValue == "REH")
			}

			AssignedVariablesList
			{ // metafor: di
				name: "nGroup2Outcome2"
				title: qsTr("N: Group 2/Outcome 2")
				singleVariable: true
				visible: (designValue == "independentGroups" && measurementValue == "binary") ||
						(designValue == "independentGroups" && measurementValue == "mixed" && (effectSizeValue == "D2ORN" || effectSizeValue == "D2ORL"))
			}

			AssignedVariablesList
			{ // metafor: di
				name: "nVariable2Outcome-"
				title: qsTr("N: Variable 2/Outcome -")
				singleVariable: true
				visible: (designValue == "variableAssociation" && measurementValue == "binary")
			}

			AssignedVariablesList
			{ // metafor: di
				name: "nTreatment2Outcome2Treatment1Outcome1+"
				title: qsTr("N: Treatment 2 + Outcome 2 / Treatment 1 + Outcome 1")
				singleVariable: true
				visible: (designValue == "repeatedMeasuresOrMatchedGroups" && measurementValue == "binary")
			}

			AssignedVariablesList
			{ // metafor: di
				name: "delta"
				title: qsTr("Delta")
				singleVariable: true
				visible: (designValue == "other" && (effectSizeValue == "ARAW" || effectSizeValue == "AHW" || effectSizeValue == "ABT"))
			}

			AssignedVariablesList
			{ // metafor: n1i
				name: "nGroup1"
				title: qsTr("N: Group 1")
				singleVariable: true
				visible: (designValue == "independentGroups" && measurementValue == "quantitative") ||
				 (designValue == "independentGroups" && measurementValue == "binary")  || 
				 (designValue == "independentGroups" && measurementValue == "countsPerTime") || 
				 (designValue == "independentGroups" && measurementValue == "mixed")
			}

			AssignedVariablesList
			{ // metafor: n1i
				name: "nRow1"
				title: qsTr("N: Row 1")
				singleVariable: true
				visible: (designValue == "variableAssociation" && measurementValue == "binary")
			}

			AssignedVariablesList
			{ // metafor: n1i
				name: "nSample1"
				title: qsTr("N: Sample 1")
				singleVariable: true
				visible: (designValue == "singleGroup" && (measurementValue == "quantitative" || measurementValue == "binary" || measurementValue == "countsPerTime"))
			}

			AssignedVariablesList
			{ // metafor: n1i
				name: "nTimePoint1"
				title: qsTr("N: Time Point 1")
				singleVariable: true
				visible: (designValue == "repeatedMeasuresOrMatchedGroups" && (measurementValue == "quantitative" || measurementValue == "binary"))
			}

			AssignedVariablesList
			{ // metafor: n2i
				name: "nGroup2"
				title: qsTr("N: Group 2")
				singleVariable: true
				visible: (designValue == "independentGroups" && measurementValue == "quantitative") ||
				 (designValue == "independentGroups" && measurementValue == "binary")  || 
				 (designValue == "independentGroups" && measurementValue == "countsPerTime") || 
				 (designValue == "independentGroups" && measurementValue == "mixed")
			}

			AssignedVariablesList
			{ // metafor: n2i
				name: "nRow2"
				title: qsTr("N: Row 2")
				singleVariable: true
				visible: (designValue == "variableAssociation" && measurementValue == "binary")
			}

			AssignedVariablesList
			{ // metafor: n2i
				name: "nSample2"
				title: qsTr("N: Sample 2")
				singleVariable: true
				visible: (designValue == "singleGroup" && (measurementValue == "quantitative" || measurementValue == "binary" || measurementValue == "countsPerTime"))
			}

			AssignedVariablesList
			{ // metafor: n2i
				name: "nTimePoint2"
				title: qsTr("N: Time Point 2")
				singleVariable: true
				visible: (designValue == "repeatedMeasuresOrMatchedGroups" && (measurementValue == "quantitative" || measurementValue == "binary"))
			}

			AssignedVariablesList
			{ // metafor: x1i
				name: "eventsGroup1"
				title: qsTr("Events: Group 1")
				singleVariable: true
				visible: (designValue == "independentGroups" && (measurementValue == "binary" || measurementValue == "countsPerTime" || measurementValue == "mixed"))
			}

			AssignedVariablesList
			{ // metafor: x1i
				name: "eventsSample1"
				title: qsTr("Events: Sample 1")
				singleVariable: true
				visible: (designValue == "singleGroup" && (measurementValue == "binary" || measurementValue == "countsPerTime"))
			}

			AssignedVariablesList
			{ // metafor: x1i
				name: "eventsTimePoint1"
				title: qsTr("Events: Time Point 1")
				singleVariable: true
				visible: (designValue == "repeatedMeasuresOrMatchedGroups" && measurementValue == "binary")
			}

			AssignedVariablesList
			{ // metafor: x2i
				name: "eventsGroup2"
				title: qsTr("Events: Group 2")
				singleVariable: true
				visible: (designValue == "independentGroups" && (measurementValue == "binary" || measurementValue == "countsPerTime" || measurementValue == "mixed"))
			}

			AssignedVariablesList
			{ // metafor: x2i
				name: "eventsSample2"
				title: qsTr("Events: Sample 2")
				singleVariable: true
				visible: (designValue == "singleGroup" && (measurementValue == "binary" || measurementValue == "countsPerTime"))
			}

			AssignedVariablesList
			{ // metafor: x2i
				name: "eventsTimePoint2"
				title: qsTr("Events: Time Point 2")
				singleVariable: true
				visible: (designValue == "repeatedMeasuresOrMatchedGroups" && measurementValue == "binary")
			}

			AssignedVariablesList
			{ // metafor: t1i
				name: "personTimeGroup1"
				title: qsTr("Total Person-Time: Group 1")
				singleVariable: true
				visible: (designValue == "independentGroups" && measurementValue == "countsPerTime")
			}

			AssignedVariablesList
			{ // metafor: t1i
				name: "personTimeSample1"
				title: qsTr("Total Person-Time: Sample 1")
				singleVariable: true
				visible: (designValue == "singleGroup" && measurementValue == "countsPerTime")
			}

			AssignedVariablesList
			{ // metafor: t2i
				name: "personTimeGroup2"
				title: qsTr("Total Person-Time: Group 2")
				singleVariable: true
				visible: (designValue == "independentGroups" && measurementValue == "countsPerTime")
			}

			AssignedVariablesList
			{ // metafor: t2i
				name: "personTimeSample2"
				title: qsTr("Total Person-Time: Sample 2")
				singleVariable: true
				visible: (designValue == "singleGroup" && measurementValue == "countsPerTime")
			}

			AssignedVariablesList
			{ // metafor: m1i
				name: "meanGroup1"
				title: qsTr("Mean: Group 1")
				singleVariable: true
				visible: (designValue == "independentGroups" && measurementValue == "quantitative") || 
				(designValue == "independentGroups" && measurementValue == "mixed" && (effectSizeValue == "PBIT" || effectSizeValue == "OR2DN" || effectSizeValue == "OR2DL"))
			}

			AssignedVariablesList
			{ // metafor: m1i
				name: "meanSample1"
				title: qsTr("Mean: Sample 1")
				singleVariable: true
				visible: (designValue == "singleGroup" && measurementValue == "quantitative")
			}

			AssignedVariablesList
			{ // metafor: m1i
				name: "meanTimePoint1"
				title: qsTr("Mean: Time Point 1")
				singleVariable: true
				visible: (designValue == "repeatedMeasuresOrMatchedGroups" && measurementValue == "quantitative")
			}

			AssignedVariablesList
			{ // metafor: m2i
				name: "meanGroup2"
				title: qsTr("Mean: Group 2")
				singleVariable: true
				visible: (designValue == "independentGroups" && measurementValue == "quantitative") || 
				(designValue == "independentGroups" && measurementValue == "mixed" && (effectSizeValue == "PBIT" || effectSizeValue == "OR2DN" || effectSizeValue == "OR2DL"))
			}

			AssignedVariablesList
			{ // metafor: m2i
				name: "meanSample2"
				title: qsTr("Mean: Sample 2")
				singleVariable: true
				visible: (designValue == "singleGroup" && measurementValue == "quantitative")
			}

			AssignedVariablesList
			{ // metafor: m2i
				name: "meanTimePoint2"
				title: qsTr("Mean: Time Point 2")
				singleVariable: true
				visible: (designValue == "repeatedMeasuresOrMatchedGroups" && measurementValue == "quantitative")
			}

			AssignedVariablesList
			{ // metafor: sd1i
				name: "sdGroup1"
				title: qsTr("SD: Group 1")
				singleVariable: true
				visible: (designValue == "independentGroups" && measurementValue == "quantitative") || 
				(designValue == "independentGroups" && measurementValue == "mixed" && (effectSizeValue == "PBIT" || effectSizeValue == "OR2DN" || effectSizeValue == "OR2DL"))
			}

			AssignedVariablesList
			{ // metafor: sd1i
				name: "sdSample1"
				title: qsTr("SD: Sample 1")
				singleVariable: true
				visible: (designValue == "singleGroup" && measurementValue == "quantitative")
			}

			AssignedVariablesList
			{ // metafor: sd1i
				name: "sdTimePoint1"
				title: qsTr("SD: Time Point 1")
				singleVariable: true
				visible: (designValue == "repeatedMeasuresOrMatchedGroups" && measurementValue == "quantitative")
			}

			AssignedVariablesList
			{ // metafor: sd2i
				name: "sdGroup2"
				title: qsTr("SD: Group 2")
				singleVariable: true
				visible: (designValue == "independentGroups" && measurementValue == "quantitative") || 
				(designValue == "independentGroups" && measurementValue == "mixed" && (effectSizeValue == "PBIT" || effectSizeValue == "OR2DN" || effectSizeValue == "OR2DL"))
			}

			AssignedVariablesList
			{ // metafor: sd2i
				name: "sdSample2"
				title: qsTr("SD: Sample 2")
				singleVariable: true
				visible: (designValue == "singleGroup" && measurementValue == "quantitative")
			}

			AssignedVariablesList
			{ // metafor: sd2i
				name: "sdTimePoint2"
				title: qsTr("SD: Time Point 2")
				singleVariable: true
				visible: (designValue == "repeatedMeasuresOrMatchedGroups" && measurementValue == "quantitative")
			}

			AssignedVariablesList
			{ // metafor: xi
				name: "eventFrequencies"
				title: qsTr("Event Frequencies")
				singleVariable: true
				visible: (designValue == "singleGroup" && measurementValue == "binary")
			}

			AssignedVariablesList
			{ // metafor: mi
				name: "complementFrequencies"
				title: qsTr("Complement Frequencies / Group Means")
				singleVariable: true
				visible: (designValue == "singleGroup" && (measurementValue == "binary" || measurementValue == "quantitative"))
			}

			AssignedVariablesList
			{ // metafor: ri
				name: "rawCorrelations"
				title: qsTr("Raw Correlation Coefficients")
				singleVariable: true
				visible: (designValue == "variableAssociation" && measurementValue == "quantitative")
			}

			AssignedVariablesList
			{ // metafor: ti
				name: "tStatistics"
				title: qsTr("T-Test Statistics")
				singleVariable: true
				visible: (designValue == "variableAssociation" && measurementValue == "quantitative")
			}

			AssignedVariablesList
			{ // metafor: ti
				name: "totalPersonTimes"
				title: qsTr("Total Person-Times")
				singleVariable: true
				visible: (designValue == "singleGroup" && measurementValue == "countsPerTime")
			}

			AssignedVariablesList
			{ // metafor: fi
				name: "fStatistics"
				title: qsTr("F-Test Statistics")
				singleVariable: true
				visible: (designValue == "variableAssociation" && measurementValue == "quantitative")
			}

			AssignedVariablesList
			{ // metafor: pi
				name: "pValues"
				title: qsTr("P-Values")
				singleVariable: true
				visible: (designValue == "variableAssociation" && measurementValue == "quantitative")
			}

			AssignedVariablesList
			{ // metafor: sdi
				name: "standardDeviations"
				title: qsTr("Standard Deviations")
				singleVariable: true
				visible: (designValue == "singleGroup" && measurementValue == "quantitative")
			}

			AssignedVariablesList
			{ // metafor: r2i
				name: "rSquared"
				title: qsTr("R-Squared Values")
				singleVariable: true
				visible: (designValue == "other" && (effectSizeValue == "R2" || effectSizeValue == "ZR2"))
			}

			AssignedVariablesList
			{ // metafor: ni
				name: "sampleSizes"
				title: qsTr("Sample/Group Sizes")
				singleVariable: true
				visible: (designValue == "singleGroup" && (measurementValue == "quantitative" || measurementValue == "binary"))
			}

			AssignedVariablesList
			{ // metafor: yi
				name: "observedEffectSizes"
				title: qsTr("Observed Effect Sizes/Outcomes")
				singleVariable: true
				visible: (designValue == "singleGroup" && (measurementValue == "quantitative" || measurementValue == "binary"))
			}

			AssignedVariablesList
			{ // metafor: vi
				name: "samplingVariances"
				title: qsTr("Sampling Variances")
				singleVariable: true
				visible: (designValue == "singleGroup" && (measurementValue == "quantitative" || measurementValue == "binary"))
			}

			AssignedVariablesList
			{ // metafor: sei
				name: "standardErrors"
				title: qsTr("Standard Errors")
				singleVariable: true
				visible: (designValue == "singleGroup" && (measurementValue == "quantitative" || measurementValue == "binary"))
			}

			Group
			{
				title: qsTr("Frequency/event cell adjustment")

				DoubleField
				{
					label: qsTr("Add")
					name: "add"
					visible: (designValue == "independentGroups" && measurementValue == "binary") ||
							(designValue == "independentGroups" && measurementValue == "countsPerTime") ||
							(designValue == "repeatedMeasuresOrMatchedGroups" && measurementValue == "binary") ||
							(designValue == "singleGroup" && measurementValue == "binary") ||
							(designValue == "singleGroup" && measurementValue == "countsPerTime") ||
							(designValue == "variableAssociation" && measurementValue == "binary") ||
							(designValue == "independentGroups" && measurementValue == "mixed" && (effectSizeValue == "D2ORN" || effectSizeValue == "D2ORL"))

					defaultValue: (designValue == "independentGroups" && measurementValue == "binary" && effectSizeValue == "RR") ? 0.5 :
								(designValue == "independentGroups" && measurementValue == "binary" && effectSizeValue == "OR") ? 0.5 :
								(designValue == "independentGroups" && measurementValue == "binary" && effectSizeValue == "RD") ? 0 :
								(designValue == "independentGroups" && measurementValue == "binary" && effectSizeValue == "AS") ? 0 :
								(designValue == "independentGroups" && measurementValue == "binary" && effectSizeValue == "PETO") ? 0.5 :
								(designValue == "independentGroups" && measurementValue == "countsPerTime") ? 0.5 :
								(designValue == "repeatedMeasuresOrMatchedGroups" && measurementValue == "binary") ? 0.5 :
								(designValue == "singleGroup" && measurementValue == "binary" && effectSizeValue == "PR") ? 0.5 :
								(designValue == "singleGroup" && measurementValue == "binary" && effectSizeValue == "PLN") ? 0 :
								(designValue == "singleGroup" && measurementValue == "binary" && effectSizeValue == "PLO") ? 0 :
								(designValue == "singleGroup" && measurementValue == "binary" && effectSizeValue == "PAS") ? 0 :
								(designValue == "singleGroup" && measurementValue == "binary" && effectSizeValue == "PFT") ? 0.5 :
								(designValue == "singleGroup" && measurementValue == "countsPerTime") ? 0.5 :
								(designValue == "variableAssociation" && measurementValue == "binary") ? 0.5 :
								(designValue == "independentGroups" && measurementValue == "mixed" && (effectSizeValue == "D2ORN" || effectSizeValue == "D2ORL")) ? 0.5 :
								0
				}	

				DropDown
				{
					name: "to"
					label: qsTr("To")
					values: [
						{ label: qsTr("All"), value: "all" },
						{ label: qsTr("Only zero"), value: "onlyZero" },
						{ label: qsTr("If any zero"), value: "ifAnyZero" },
						{ label: qsTr("None"), value: "none" }
					]
					visible: (design.value == "independentGroups" && measurement.value == "binary") ||
							(design.value == "independentGroups" && measurement.value == "countsPerTime") ||
							(design.value == "repeatedMeasuresOrMatchedGroups" && measurement.value == "binary") ||
							(design.value == "singleGroup" && measurement.value == "binary") ||
							(design.value == "singleGroup" && measurement.value == "countsPerTime") ||
							(design.value == "variableAssociation" && measurement.value == "binary") ||
							(design.value == "independentGroups" && measurement.value == "mixed" && (effectSize.value == "D2ORN" || effectSize.value == "D2ORL"))
					//defaultValue: "onlyZero"
				}

				RadioButtonGroup
				{
					name: "dropStudiesWithNoCasesOrEvents"
					title: qsTr("Drop studies with no cases/events")
					columns: 1
					visible: (design.value == "independentGroups" && measurement.value == "binary") ||
							(design.value == "independentGroups" && measurement.value == "countsPerTime") ||
							(design.value == "repeatedMeasuresOrMatchedGroups" && measurement.value == "binary") ||
							(design.value == "singleGroup" && measurement.value == "binary") ||
							(design.value == "singleGroup" && measurement.value == "countsPerTime") ||
							(design.value == "variableAssociation" && measurement.value == "binary") ||
							(design.value == "independentGroups" && measurement.value == "mixed" && (effectSize.value == "D2ORN" || effectSize.value == "D2ORL"))

					RadioButton
					{
						value: "true"
						label: qsTr("Yes")
						checked: true
					}

					RadioButton
					{
						value: "false"
						label: qsTr("No")
					}
				}
			}

			DropDown
			{
				name: "samplingVarianceType"
				label: qsTr("Sampling variance type")
				values: (function() {
					if ((design.value == "independentGroups" && measurement.value == "quantitative") ||
						(design.value == "independentGroups" && measurement.value == "mixed" && (effectSize.value == "PBIT" || effectSize.value == "OR2DN" || effectSize.value == "OR2DL")) ||
						(design.value == "independentGroups" && measurement.value == "binary" && effectSize.value == "RR") ||
						(design.value == "independentGroups" && measurement.value == "binary" && effectSize.value == "OR") ||
						(design.value == "independentGroups" && measurement.value == "countsPerTime")) {
						return [
							{ label: qsTr("LS"), value: "LS" },
							{ label: qsTr("LS2"), value: "LS2" },
							{ label: qsTr("UB"), value: "UB" },
							{ label: qsTr("AV"), value: "AV" },
							{ label: qsTr("HO"), value: "HO" }
						];
					} else if ((design.value == "variableAssociation" && measurement.value == "quantitative") ||
							(design.value == "variableAssociation" && measurement.value == "binary") ||
							(design.value == "variableAssociation" && measurement.value == "mixed") ||
							(design.value == "singleGroup" && measurement.value == "quantitative") ||
							(design.value == "repeatedMeasuresOrMatchedGroups" && measurement.value == "quantitative") ||
							(design.value == "singleGroup" && measurement.value == "binary") ||
							(design.value == "singleGroup" && measurement.value == "countsPerTime") ||
							(design.value == "independentGroups" && measurement.value == "mixed" && (effectSize.value == "D2ORN" || effectSize.value == "D2ORL"))) {
						return [
							{ label: qsTr("LS"), value: "LS" },
							{ label: qsTr("AV"), value: "AV" }
						];
					} else if ((design.value == "repeatedMeasuresOrMatchedGroups" && measurement.value == "binary") ||
							(design.value == "singleGroup" && measurement.value == "binary" && effectSize.value == "PLN") ||
							(design.value == "singleGroup" && measurement.value == "binary" && effectSize.value == "PLO")) {
						return [
							{ label: qsTr("LS"), value: "LS" },
							{ label: qsTr("AV"), value: "AV" },
							{ label: qsTr("HO"), value: "HO" }
						];
					} else {
						return [];
					}
				})()
				visible: (design.value == "independentGroups" && measurement.value == "quantitative") ||
						(design.value == "independentGroups" && measurement.value == "binary") ||
						(design.value == "independentGroups" && measurement.value == "countsPerTime") ||
						(design.value == "independentGroups" && measurement.value == "mixed") ||
						(design.value == "variableAssociation" && measurement.value == "quantitative") ||
						(design.value == "variableAssociation" && measurement.value == "binary") ||
						(design.value == "variableAssociation" && measurement.value == "mixed") ||
						(design.value == "singleGroup" && measurement.value == "quantitative") ||
						(design.value == "repeatedMeasuresOrMatchedGroups" && measurement.value == "quantitative") ||
						(design.value == "singleGroup" && measurement.value == "binary") ||
						(design.value == "singleGroup" && measurement.value == "countsPerTime")
			}
			


		}

	}

}