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
			name: 	"model"
			title: 	qsTr("Model")
			id:		modelType

			RadioButton
			{
				value: 				"fixed"
				label: 				qsTr("Fixed effects")
			}

			RadioButton
			{
				value: 				"random"
				label: 				qsTr("Random effects")
			}

			RadioButton
			{
				value: 				"averaging"
				label: 				qsTr("Model averaging")
				checked: 			true
			}

			RadioButton
			{
				value: 				"constrainedRandom"
				label: 				qsTr("Constrained random effects")

				// Constrain effect sizes to be all positive or all negative
				RadioButtonGroup
				{
					name:	"constrainedRandomDirection"
					id:		modelDirection

					columns: 2

					RadioButton
					{
						value: 		"positive"
						label: 		qsTr("All positive")
						checked:	true
					}

					RadioButton
					{
						value: 	"negative"
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
				name: 	"modelProbability";
				label: 	qsTr("Model probabilities")
			}

			CheckBox
			{
				name: 	"effectSizePerStudy";
				label: 	qsTr("Effect sizes per study")
			}
		}

		//// BF ////
		BayesFactorType { }

	}
}