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

ColumnLayout
{
	spacing: 						0
	property string componentType:	"Default type"

	Label
	{
		text:					
		{
			if (componentType == "effect")
				qsTr("Effect")
			else if (componentType == "effectNull")
				qsTr("Effect (null)")
			else if (componentType == "heterogeneity")
				qsTr("Heterogeneity")
			else if (componentType == "heterogeneityNull")
				qsTr("Heterogeneity (null)")
			else if (componentType == "pet")
				qsTr("Publication bias: PET")
			else if (componentType == "petNull")
				qsTr("Publication bias: PET (null)")
			else if (componentType == "peese")
				qsTr("Publication bias: PEESE")
			else if (componentType == "peeseNull")
				qsTr("Publication bias: PEESE (null)")
		}
		Layout.preferredHeight:	20 * preferencesModel.uiScale
	}

	RowLayout
	{
		Label { text: qsTr("Distribution"); Layout.preferredWidth: 140 * preferencesModel.uiScale; Layout.leftMargin: 5 * preferencesModel.uiScale}
		Label { text: qsTr("Parameters");	Layout.preferredWidth: 155 * preferencesModel.uiScale }
		Label { text: qsTr("Truncation");	Layout.preferredWidth: 150 * preferencesModel.uiScale }
		Label { text: qsTr("Prior weights") }
	}

	ComponentsList
	{
		name:					componentType
		optionKey:				"name"
		defaultValues:				
		{
			if (componentType == "effect")
				[{"type": "normal"}]
			else if (componentType == "effectNull")
				[{"type": "spike"}]
			else if (componentType == "heterogeneity")
				[{"type": "invgamma"}]
			else if (componentType == "heterogeneityNull")
				[{"type": "spike"}]
			else if (componentType == "pet")
				[{"type": "cauchy", "priorWeights": "1/4"}]
			else if (componentType == "petNull")
				[]
			else if (componentType == "peese")
				[{"type": "cauchy", "parScale2": "5", "priorWeights": "1/4"}]
			else if (componentType == "peeseNull")
				[]
		}
		rowComponent: 			RowLayout
		{
			Row
			{
				spacing:				4 * preferencesModel.uiScale
				Layout.preferredWidth:	140 * preferencesModel.uiScale

				DropDown
				{
					id: typeItem
					name: "type"
					useExternalBorder: true
					values:
					[
						{ label: qsTr("Normal(μ,σ)"),			value: "normal"},
						{ label: qsTr("Student-t(μ,σ,v)"),		value: "t"},
						{ label: qsTr("Cauchy(x₀,θ)"),			value: "cauchy"},
						{ label: qsTr("Gamma(α,β)"),			value: "gammaAB"},
						{ label: qsTr("Gamma(k,θ)"),			value: "gammaK0"},
						{ label: qsTr("Inverse-Gamma(α,β)"),	value: "invgamma"},
						{ label: qsTr("Log-Normal(μ,σ)"),		value: "lognormal"},
						{ label: qsTr("Beta(α,β)"),				value: "beta"},
						{ label: qsTr("Uniform(a,b)"),			value: "uniform"},
						{ label: qsTr("Spike(x₀)"),				value: "spike"},
						{ label: qsTr("None"),					value: "none"}
					]
				}
			}

			Row
			{
				spacing:				4 * preferencesModel.uiScale
				Layout.preferredWidth:	155 * preferencesModel.uiScale

				FormulaField
				{
					label:				"μ "
					name:				"parMean"
					visible:			typeItem.currentValue === "normal"		||
										typeItem.currentValue === "lognormal"	||
										typeItem.currentValue === "t"
					value:				"0"
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"x₀"
					name:				"parLocation"
					visible:			typeItem.currentValue === "cauchy"	||
										typeItem.currentValue === "spike"
					value:				"0"
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"σ"
					name:				"parScale"
					id:					parScale
					visible:			typeItem.currentValue === "normal"		||
										typeItem.currentValue === "lognormal"	||
										typeItem.currentValue === "t"
					value:				"1"
					min:				0
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"k "
					name:				"parShape"
					visible:			typeItem.currentValue === "gammaK0"
					value:				"1"
					min:				0
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
				}
				FormulaField
				{
					label:				"θ"
					name:				"parScale2"
					visible:			typeItem.currentValue === "cauchy"	||
										typeItem.currentValue === "gammaK0"
					value:				"1"
					min:				0
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"ν"
					name:				"parDf"
					visible:			typeItem.currentValue === "t"
					value:				"2"
					min:				1
					inclusive:			JASP.MinOnly
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"α "
					name:				"parAlpha"
					visible:			typeItem.currentValue === "gammaAB"	 ||
										typeItem.currentValue === "invgamma" ||
										typeItem.currentValue === "beta"
					value:				"1"
					min:				0
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"β"
					name:				"parBeta"
					visible:			typeItem.currentValue === "gammaAB"	 ||
										typeItem.currentValue === "invgamma" ||
										typeItem.currentValue === "beta"
					value:				"0.15"
					min:				0
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"a "
					name:				"parA"
					id:					parA
					visible:			typeItem.currentValue === "uniform"
					value:				"0"
					max:				parB.value
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"b"
					name:				"parB"
					id:					parB
					visible:			typeItem.currentValue === "uniform"
					value:				"1"
					min:				parA.value
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
			}

			Row
			{
				spacing:				4 * preferencesModel.uiScale
				Layout.preferredWidth:	150 * preferencesModel.uiScale

				FormulaField
				{
					id:					truncationLower
					label: 				qsTr("lower")
					name: 				"truncationLower"
					visible:			typeItem.currentValue !== "spike" && typeItem.currentValue !== "uniform"
					value:				
					{
						if(componentType == "heterogeneity" || componentType == "heterogeneityNull" || componentType == "pet" || componentType == "petNull" || componentType == "peese" || componentType == "peeseNull")
							0
						else if (typeItem.currentValue === "gammaK0" || typeItem.currentValue === "gammaAB" || typeItem.currentValue === "invgamma" || typeItem.currentValue === "lognormal" || typeItem.currentValue === "beta")
							0
						else
							"-Inf"
					}	
					min:				
					{
						if(componentType == "heterogeneity" || componentType == "heterogeneityNull" || componentType == "pet" || componentType == "petNull" || componentType == "peese" || componentType == "peeseNull")
							0
						else if (typeItem.currentValue === "gammaK0" || typeItem.currentValue === "gammaAB" || typeItem.currentValue === "invgamma" || typeItem.currentValue === "lognormal" || typeItem.currentValue === "beta")
							0
						else
							"-Inf"
					}
					max: 				truncationUpper.value
					inclusive: 			JASP.MinOnly
					fieldWidth:			40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder:			true
				}
				FormulaField
				{
					id:					truncationUpper
					label: 				qsTr("upper")
					name: 				"truncationUpper"
					visible:			typeItem.currentValue !== "spike" && typeItem.currentValue !== "uniform"
					value:
					{
						if (typeItem.currentValue === "beta")
							1
						else
							"Inf"
					}
					max:				
					{
						if (typeItem.currentValue === "beta")
							1
						else
							"Inf"
					}
					min: 				truncationLower ? truncationLower.value : 0
					inclusive: 			JASP.MaxOnly
					fieldWidth:			40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder:			true
				}
			}

			FormulaField
			{
				label: 				qsTr("Weight")
				name: 				"priorWeight"
				value:				"1"
				min: 				0
				inclusive: 			JASP.None
				fieldWidth:			40 * preferencesModel.uiScale
				useExternalBorder:	false
				showBorder:			true
			}
		}
	}
}
