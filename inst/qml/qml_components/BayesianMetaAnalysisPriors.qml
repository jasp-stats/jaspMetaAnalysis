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
	title: 		qsTr("Priors")
	columns:	1

	property string modelTypeValue:			"averaging"
	property string modelDirectionValue:	"positive"

	Group
	{
		columns: 2

		RadioButtonGroup
		{
			title: 	qsTr("Effect size")
			name: 	"priorEffectSize"

			RadioButton
			{
				id: 				cauchyInformative
				label: 				qsTr("Cauchy")
				name: 				"cauchy"
				checked: 			true
				childrenOnSameRow: 	true

				DoubleField
				{
					label: 			qsTr("location:")
					name: 			"cauchyLocation"
					visible: 		cauchyInformative.checked
					defaultValue: 	0
					negativeValues: true
				}

				DoubleField
				{
					label: 			qsTr("scale:");
					name: 			"cauchyScale"
					visible: 		cauchyInformative.checked
					defaultValue: 	0.707
					fieldWidth: 	50
				}


			}

			RadioButton
			{
				id: 				normalInformative
				label: 				qsTr("Normal")
				name: 				"normal"
				childrenOnSameRow:	true

				DoubleField
				{
					label: 			qsTr("mean:")
					name: 			"normalMean"
					visible: 		normalInformative.checked
					defaultValue: 	0
					negativeValues: true
				}

				DoubleField
				{
					label: 			qsTr("std:")
					name: 			"normalSd"
					visible: 		normalInformative.checked
					defaultValue: 	0.707
					fieldWidth: 	50
				}


			}

			RadioButton
			{
				id: 				tInformative
				label: 				qsTr("t")
				name: 				"t"
				childrenOnSameRow: 	true

				DoubleField
				{
					label: 			qsTr("location:")
					name: 			"tLocation"
					visible: 		tInformative.checked
					defaultValue: 	0
					negativeValues: true
				}

				DoubleField
				{
					label: 			qsTr("scale:")
					name: 			"tScale"
					visible: 		tInformative.checked
					defaultValue: 	0.707
					fieldWidth: 	50
				}

				IntegerField
				{
					label: 			qsTr("df:");
					name: 			"tDf";
					visible: 		tInformative.checked;
					min:			1
					defaultValue: 	1
				}


			}
		}
		Group
		{
			title: qsTr("Truncation")
			CheckBox
			{
				name: 				"truncationLowerBound"
				childrenOnSameRow: 	true
				checked: 			modelTypeValue == "constrainedRandom" && modelDirectionValue == "positive"

				DoubleField
				{
					id: 			lowerTT
					name: 			"truncationLowerBoundValue"
					label: 			qsTr("Lower bound:")
					fieldWidth: 	50
					negativeValues: !(modelTypeValue == "constrainedRandom" && modelDirectionValue == "positive")
					defaultValue: 	0
					max: 			modelTypeValue == "constrainedRandom" && modelDirectionValue == "negative" ? 0 : Infinity
				}
			}

			CheckBox
			{
				name: 				"truncationUpperBound"
				childrenOnSameRow: 	true
				checked: 			modelTypeValue == "constrainedRandom" && modelDirectionValue == "negative"

				DoubleField
				{
					id: upperTT
					name: 			"truncationUpperBoundValue"
					label: 			qsTr("Upper bound:")
					fieldWidth: 	50
					negativeValues: !(modelTypeValue == "constrainedRandom") && modelDirectionValue == "positive"
					defaultValue: 	0
					max: 			modelTypeValue == "constrainedRandom" && modelDirectionValue == "negative" ? 0 : Infinity
				}
			}
		}

	}

	RadioButtonGroup
	{
		enabled:	modelTypeValue == "random" || modelTypeValue == "constrainedRandom" || modelTypeValue == "averaging"
		title: 		qsTr("Heterogeneity (between-study SD)")
		name: 		"priorStandardError"

		RadioButton
		{
			id: 				igInformative
			name: 				"inverseGamma"
			label: 				qsTr("Inverse gamma")
			childrenOnSameRow: 	true
			checked: 			true

			DoubleField
			{
				label: 			qsTr("shape:")
				name: 			"inverseGammaShape"
				visible: 		igInformative.checked
				defaultValue: 	1
				fieldWidth: 	50
			}

			DoubleField
			{
				label: 			qsTr("scale:")
				name: 			"inverseGammaScale"
				visible: 		igInformative.checked
				defaultValue:	0.15
				fieldWidth: 	50
			}
		}

		RadioButton
		{
			id: 				halfTInformative
			name: 				"halfT"
			label: 				qsTr("Half t")
			childrenOnSameRow: 	true

			DoubleField
			{
				label: 			qsTr("scale:")
				name: 			"halfTScale"
				visible: 		halfTInformative.checked
				defaultValue: 	0.707
				fieldWidth: 	50
			}

			IntegerField
			{
				label: 			qsTr("df:")
				name: 			"halfTDf"
				visible: 		halfTInformative.checked
				min:			1
				defaultValue: 	1
			}
		}
	}

	CheckBox
	{
		name: 	"priorPlot"
		label: 	qsTr("Plot prior(s)")
	}
}
