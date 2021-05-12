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
	title: 		qsTr("Priors")
	columns:	1

	property string modelTypeValue:			"BMA"
	property string modelDirectionValue:	"allPos"

	Group
	{
		columns: 2

		RadioButtonGroup
		{
			title: 	qsTr("Effect size")
			name: 	"priorES"

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
					name: 			"informativeCauchyLocation"
					visible: 		cauchyInformative.checked
					defaultValue: 	0
					negativeValues: true
				}

				DoubleField
				{
					label: 			qsTr("scale:");
					name: 			"informativeCauchyScale"
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
					name: 			"informativeNormalMean"
					visible: 		normalInformative.checked
					defaultValue: 	0
					negativeValues: true
				}

				DoubleField
				{
					label: 			qsTr("std:")
					name: 			"informativeNormalStd"
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
					name: 			"informativeTLocation"
					visible: 		tInformative.checked
					defaultValue: 	0
					negativeValues: true
				}

				DoubleField
				{
					label: 			qsTr("scale:")
					name: 			"informativeTScale"
					visible: 		tInformative.checked
					defaultValue: 	0.707
					fieldWidth: 	50
				}

				IntegerField
				{
					label: 			qsTr("df:");
					name: 			"informativeTDf";
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
				name: 				"checkLowerPrior"
				childrenOnSameRow: 	true
				checked: 			modelTypeValue == "CRE" && modelDirectionValue == "allPos"

				DoubleField
				{
					id: 			lowerTT
					name: 			"lowerTrunc"
					label: 			qsTr("Lower bound:")
					fieldWidth: 	50
					negativeValues: !(modelTypeValue == "CRE") && modelDirectionValue == "allPos"
					defaultValue: 	0
					max: 			modelTypeValue == "CRE" && modelDirectionValue == "allNeg" ? 0 : Infinity
				}
			}

			CheckBox
			{
				name: 				"checkUpperPrior"
				childrenOnSameRow: 	true
				checked: 			modelTypeValue == "CRE" && modelDirectionValue == "allNeg"

				DoubleField
				{
					id: upperTT
					name: 			"upperTrunc"
					label: 			qsTr("Upper bound:")
					fieldWidth: 	50
					negativeValues: !(modelTypeValue == "CRE") && modelDirectionValue == "allPos"
					defaultValue: 	0
					max: 			modelTypeValue == "CRE" && modelDirectionValue == "allNeg" ? 0 : Infinity
				}
			}
		}

	}

	RadioButtonGroup
	{
		enabled:	modelTypeValue == "RE" || modelTypeValue == "CRE" || modelTypeValue == "BMA"
		title: 		qsTr("Heterogeneity (Between study SD)")
		name: 		"priorSE"

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
				name: 			"informativehalfTScale"
				visible: 		halfTInformative.checked
				defaultValue: 	0.707
				fieldWidth: 	50
			}

			IntegerField
			{
				label: 			qsTr("df:")
				name: 			"informativehalfTDf"
				visible: 		halfTInformative.checked
				min:			1
				defaultValue: 	1
			}
		}
	}

	CheckBox
	{
		name: 	"plotPrior"
		label: 	qsTr("Plot prior(s)")
	}
}