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

Section
{
	columns: 	2
	title: 		qsTr("Advanced")
	
	property string modelTypeValue:			"averaging"
	property string modelDirectionValue:	"positive"

	Group
	{
		id:			priorModelProbabilityGroup
		enabled: 	modelTypeValue == "fixed" || modelTypeValue == "random" || modelTypeValue == "averaging"
		title: 		qsTr("Prior model probability")

		property double fixedEffectsHypothesisVal:	modelTypeValue == "fixed" ? 0.5 :
														modelTypeValue == "random" ? 0 :
															modelTypeValue == "averaging" ? 0.25 : 0

		property double randomEffectsHypothesisVal:	modelTypeValue == "fixed" ? 0 :
														modelTypeValue == "random" ? 0.5 :
															modelTypeValue == "averaging" ? 0.25 : 0
		function resetHypotheses() {
			priorH0FE.value = fixedEffectsHypothesisVal
			priorH1FE.value = fixedEffectsHypothesisVal
			priorH0RE.value = randomEffectsHypothesisVal
			priorH1RE.value = randomEffectsHypothesisVal

			priorH0FE.editingFinished()
			priorH1FE.editingFinished()
			priorH0RE.editingFinished()
			priorH1RE.editingFinished()
		}
		
		Group
		{
			enabled: 			modelTypeValue == "fixed" || modelTypeValue == "averaging"
			title: 				qsTr("Fixed effects")
			onEnabledChanged: 	priorModelProbabilityGroup.resetHypotheses()

			DoubleField
			{
				id: 			priorH0FE
				name: 			"priorModelProbabilityFixedNull"
				label: 			"H\u2080"
				defaultValue: 	priorModelProbabilityGroup.fixedEffectsHypothesisVal
			}

			DoubleField
			{
				id: 			priorH1FE
				name: 			"priorModelProbabilityFixedAlternative"
				label: 			"H\u2081"
				defaultValue: 	priorModelProbabilityGroup.fixedEffectsHypothesisVal
			}
		}

		Group
		{
			title: 				qsTr("Random effects")
			enabled: 			modelTypeValue == "random" || modelTypeValue == "averaging"
			onEnabledChanged: 	priorModelProbabilityGroup.resetHypotheses()

			DoubleField
			{
				id: 			priorH0RE
				name: 			"priorModelProbabilityRandomNull"
				label: 			"H\u2080"
				defaultValue: 	priorModelProbabilityGroup.randomEffectsHypothesisVal
			}

			DoubleField
			{
				id: 			priorH1RE
				name: 			"priorModelProbabilityRandomAlternative"
				label: 			"H\u2081"
				defaultValue: 	priorModelProbabilityGroup.randomEffectsHypothesisVal
			}
		}
	}

	Group
	{
		enabled: !(modelTypeValue == "constrainedRandom")

		Group
		{
			title: 		qsTr("Estimation settings (MCMC)")
			columns: 	2

			IntegerField
			{
				label: 			qsTr("samples:")
				name: 			"samples"
				defaultValue: 	!(modelTypeValue == "constrainedRandom") ? 2000 : 10000
				min:			100
				max: 			1000000
				fieldWidth: 	100
			}

			IntegerField
			{
				label: 			qsTr("chains:")
				name: 			"chains"
				defaultValue: 	4
				min:			1
				max: 			10
				fieldWidth: 	50
			}
		}

		SetSeed { }

		Group
		{
			title: qsTr("Bayes factor computation")

			RadioButtonGroup
			{
				name: "bayesFactorComputation"

				RadioButton
				{
					name: 		"integration";
					label: 		qsTr("Integration")
					checked: 	true
				}

				RadioButton
				{
					id: 				"bridge"
					name: 				"bridgeSampling"
					label: 				qsTr("Bridge sampling")
					childrenOnSameRow: 	true

					IntegerField
					{
						label:  		qsTr("iterations:")
						name:     		"bridgeSamplingSamples"
						visible:      	bridge.checked
						defaultValue: 	5000
						max:			1000000
						fieldWidth: 	100
					}
				}
			}
		}
	}
}
