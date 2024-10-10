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

Form
{

	VariablesForm
	{
		preferredHeight: 200 * preferencesModel.uiScale

		AvailableVariablesList
		{
			name: "allVariables"
		}

		AssignedVariablesList
		{
			title:				qsTr("Effect Size")
			name:				"effectSize"
			singleVariable:		true
			allowedColumns:		["scale"]
		}

		AssignedVariablesList
		{
			name:				"effectSizeStandardError"
			title:				qsTr("Effect Size Standard Error")
			singleVariable:		true
			allowedColumns:		["scale"]
		}

		AssignedVariablesList
		{
			name:				"studyLabels"
			id:					studyLabels
			title:				qsTr("Study Labels")
			singleVariable:		true
			allowedColumns:		["nominal"]
		}

	}

	CheckBox
	{
		name:		"funnelUnderH0"
		id:			funnelUnderH0
		label:		qsTr("Funnel under H₀")
		checked:	true

		Group
		{
			title:	qsTr("Parameters")

			DoubleField
			{
				text:				qsTr("μ")
				name: 				"funnelUnderH0ParametersFixedMu"
				defaultValue:		0
				negativeValues:		true
			}

			DoubleField
			{
				text:				qsTr("𝜏")
				name: 				"funnelUnderH0ParametersFixedTau"
				defaultValue:		0
				min: 				0
			}
		}

		CheckBox
		{
			name:		"funnelUnderH0FillColors"
			label:		qsTr("Fill colors")
			checked:	true
		}

		DropDown
		{
			name:		"funnelUnderH0LineType"
			label:		qsTr("Line type")
			values:		[
				{ label: qsTr("None"),		value: "none"	},
				{ label: qsTr("Solid"),		value: "solid"	},
				{ label: qsTr("Dashed"),	value: "dashed"	},
				{ label: qsTr("Dotted"),	value: "dotted"	}
			]
		}
	}

	CheckBox
	{
		name:		"funnelUnderH1"
		id:			funnelUnderH1
		label:		qsTr("Funnel under H₁")
		checked:	false

		RadioButtonGroup
		{
			name:		"funnelUnderH1Parameters"
			title:		qsTr("Parameters")
			columns:	2
			radioButtonsOnSameRow: true

			RadioButton
			{
				value:	"estimated"
				label:	qsTr("Estimated")
				checked: true
				id:		estimated
			}

			RadioButton
			{
				value:	"fixed"
				label:	qsTr("Fixed")
				id:		fixed
			}
		}

		Group
		{
			visible:	estimated.checked

			DropDown
			{
				name:			"method"
				label:			qsTr("Method")
				startValue:		"restrictedML"
				values:			[
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
				]
			}

			CheckBox
			{
				name:		"funnelUnderH1IncludeHeterogeneity"
				label:		qsTr("Include heterogeneity")
			}

			CheckBox
			{
				name:		"funnelUnderH1EstimatesTable"
				label:		qsTr("Estimates table")
			}
		}		

		Group
		{
			visible:	fixed.checked

			DoubleField
			{
				text:				qsTr("μ")
				name: 				"funnelUnderH1ParametersFixedMu"
				defaultValue:		0
				negativeValues:		true
			}

			DoubleField
			{
				text:				qsTr("𝜏")
				name: 				"funnelUnderH1ParametersFixedTau"
				defaultValue:		0
				min: 				0
			}
		}

		CheckBox
		{
			name:		"funnelUnderH1FillColors"
			label:		qsTr("Fill colors")
		}

		DropDown
		{
			name:		"funnelUnderH1LineType"
			label:		qsTr("Line type")
			startValue:	"solid"
			values:		[
				{ label: qsTr("None"),		value: "none"	},
				{ label: qsTr("Solid"),		value: "solid"	},
				{ label: qsTr("Dashed"),	value: "dashed"	},
				{ label: qsTr("Dotted"),	value: "dotted"	}
			]
		}
	}


	Group
	{
		title:	qsTr("Estimates Mapping")

		DropDown
		{
			label:		qsTr("Label")
			name:		"estimatesMappingLabel"
			enabled: 	studyLabels.count > 0
			values:		{
				if (funnelUnderH0.checked && funnelUnderH1.checked) {
					[
						{ label: qsTr("None"),			value: "none"		},
						{ label: qsTr("All"),			value: "all"		},
						{ label: qsTr("Outside H₀"),	value: "outsideH0"	},
						{ label: qsTr("Outside H₁"),	value: "outsideH1"	}
					]
				} else if (funnelUnderH0.checked) {
					[
						{ label: qsTr("None"),			value: "none"		},
						{ label: qsTr("All"),			value: "all"		},
						{ label: qsTr("Outside H₀"),	value: "outsideH0"	}
					]
				} else if (funnelUnderH1.checked) {
					[
						{ label: qsTr("None"),			value: "none"		},
						{ label: qsTr("All"),			value: "all"		},
						{ label: qsTr("Outside H₁"),	value: "outsideH1"	}
					]
				} else {
					[
						{ label: qsTr("None"),			value: "none"		},
						{ label: qsTr("All"),			value: "all"		}
					]
				}
			}
		}

		DropDown
		{
			name:			"estimatesMappingColor"
			label:			qsTr("Color")
			addEmptyValue:	true
		}

		DropDown
		{
			name:			"estimatesMappingShape"
			label:			qsTr("Shape")
			addEmptyValue:	true
		}
	}
	

	Group
	{
		TextField
		{
			label: 				qsTr("Funnel prediction interval")
			name: 				"funnelPredictionInterval"
			value:				"(0.90, 0.95, 0.99)"
			fieldWidth: 		120 * preferencesModel.uiScale
		}

		CheckBox
		{
			name:		"powerEnhancement"
			id:			powerEnhancement
			label:		qsTr("Power enhancement")

			CheckBox
			{
				name:		"powerEnhancementSecondaryAxis"
				label:		qsTr("Secondary y-axis")
			}

			TextField
			{
				label: 				qsTr("Breaks")
				name: 				"powerEnhancementBreaks"
				value:				"(0.15, 0.30, 0.50, 0.70, 0.90)"
				fieldWidth: 		120 * preferencesModel.uiScale
			}
		}

		CheckBox
		{
			name:		"invertColors"
			label:		qsTr("Invert colors")
			enabled:	!powerEnhancement.checked
		}
	}

}
