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
			name:			"effectSize"
			title:			if (measures_correlation.checked) {
				qsTr("Correlation")
			} else {
				qsTr("Effect Size")
			}
			singleVariable:	true
			allowedColumns:	["scale"]
		}

		AssignedVariablesList
		{
			name:			"effectSizeSe"
			title:			qsTr("Effect Size Standard Error")
			singleVariable:	true
			allowedColumns:	["scale"]
			visible:		active
			
			property bool active:   measures_general.checked
			onActiveChanged: if (!active && count > 0) itemDoubleClicked(0);
		}

		AssignedVariablesList
		{
			name: 			"sampleSize"
			title: 			qsTr("N")
			singleVariable: true
			allowedColumns: ["scale", "ordinal"]
			visible:		active
			
			property bool active:   measures_correlation.checked
			onActiveChanged: if (!active && count > 0) itemDoubleClicked(0);
		}

		AssignedVariablesList
		{
			name:			"pValue"
			title:			qsTr("P-value (one-sided)")
			singleVariable:	true
			allowedColumns:	["scale"]
		}
	}

	RadioButtonGroup
	{
		name:					"measures"
		title:					qsTr("Measure")

		RadioButton
		{
			label: qsTr("Effect sizes & SE")
			value: "general"
			id: 	measures_general
			checked:true
		}

		RadioButton
		{
			label: qsTr("Correlations & N")
			value: "correlation"
			id: 	measures_correlation
		}
	}

	Section
	{
		title: qsTr("Model")

		TextField
		{
			name:		"modelPValueCutoffs"
			text:		qsTr("P-value cutoffs")
			value:		"(.05, .10)"
			fieldWidth:	150
		}

		CheckBox
		{
			name:		"modelTwoSidedSelection"
			text:		qsTr("Two-sided selection")
			checked:	true
		}

		CheckBox
		{
			name:	"modelPValueFrequencyTable"
			text:	qsTr("P-value frequency table")
		}
		
		CheckBox
		{
			name:		"modelAutomaticallyJoinPValueIntervals"
			text:		qsTr("Automatically join p-value intervals")
			checked:	true
		}

		RadioButtonGroup
		{
			columns:	2
			name:		"modelExpectedDirectionOfEffectSizes"
			title:		qsTr("Expected direction of effect sizes")

			RadioButton
			{
				value:		"positive"
				label:		qsTr("Positive")
				checked: 	true
			}

			RadioButton
			{
				value:		"negative"
				label:		qsTr("Negative")
			}

		}

		DropDown
		{
			visible:	measures_correlation.checked
			label:		qsTr("Transform correlations to")
			name:		"transformCorrelationsTo"
			values:
			[
				{ label: qsTr("Cohen's d"),		value: "cohensD"},
				{ label: qsTr("Fisher's z"),	value: "fishersZ"}
			]
		}
	}

	Section
	{
		title: qsTr("Inference")

		Group
		{
			title: qsTr("Fixed Effects")
			
			CheckBox
			{
				name:	"inferenceFixedEffectsMeanEstimatesTable"
				text:	qsTr("Mean estimates table")
				checked: true
			}

			CheckBox
			{
				name:	"inferenceFixedEffectsEstimatedWeightsTable"
				text:	qsTr("Estimated weights table")
			}
		
		}
		
		Group
		{
			title: qsTr("Random Effects")
		
			CheckBox
			{
				name:	"inferenceRandomEffectsMeanEstimatesTable"
				text:	qsTr("Mean estimates table")
				checked: true
			}

			CheckBox
			{
				name:	"inferenceRandomEffectsEstimatedHeterogeneityTable"
				text:	qsTr("Estimated heterogeneity table")
			}

			CheckBox
			{
				name:	"inferenceRandomEffectsEstimatedWeightsTable"
				text:	qsTr("Estimated weights table")
			}
	
		}

	}
	Section
	{
		title: qsTr("Plots")

		Group
		{
			title: qsTr("Weight Function")

			CheckBox
			{
				name:	"plotsWeightFunctionFixedEffectsPlot"
				text:	qsTr("Fixed effects")
			}

			CheckBox
			{
				name:	"plotsWeightFunctionRandomEffectsPlot"
				text:	qsTr("Random effects")
			}

			CheckBox
			{
				name:	"plotsWeightFunctionRescaleXAxis"
				text:	qsTr("Rescale x-axis")
			}
		}

		CheckBox
		{
			name: "plotsMeanModelEstimatesPlot"
			text:	qsTr("Mean model estimates")
		}

	}
}
