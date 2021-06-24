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
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Section
{
	title:		qsTr("Inference")
	expanded:	true

	property alias modelTypeValue:			modelType.value
	property alias modelDirectionValue:		modelDirection.value

	GridLayout
	{
		columns: 2

		//// Analysis choices ////
		RadioButtonGroup
		{
			name: 	"modelSpecification"
			title: 	qsTr("Model")
			id:		modelType

			RadioButton
			{
				value: 				"FE"
				label: 				qsTr("Fixed effects")
				onCheckedChanged:	{
					if (checked)
					{
						priorModelProbabilityGroup.resetHypotheses()
						forestObserved.click()
					}
				}
			}

			RadioButton
			{
				value: 				"RE"
				label: 				qsTr("Random effects")
				onCheckedChanged:	if(checked) priorModelProbabilityGroup.resetHypotheses()
			}

			RadioButton
			{
				value: 				"BMA"
				label: 				qsTr("Model averaging")
				checked: 			true
				onCheckedChanged:	if(checked) priorModelProbabilityGroup.resetHypotheses()
			}

			RadioButton
			{
				value: 				"CRE"
				label: 				qsTr("Constrained random effects")
				onCheckedChanged:	if(checked) priorModelProbabilityGroup.resetHypotheses()

				// Constrain effect sizes to be all positive or all negative
				RadioButtonGroup
				{
					name:	"direction"
					id:		modelDirection

					columns: 2

					RadioButton
					{
						value: 		"allPos"
						label: 		qsTr("All positive")
						checked:	true
					}

					RadioButton
					{
						value: 	"allNeg"
						label: 	qsTr("All negative")
					}
				}
			}
		}

		//// Tables ////
		Group
		{
		    title: qsTr("Table")

			CheckBox
			{
				name: 	"postTable";
				label: 	qsTr("Model probabilities")
			}

			CheckBox
			{
				name: 	"esTable";
				label: 	qsTr("Effect sizes per study")
			}
		}

		//// BF ////
		BayesFactorType { }

	}
}