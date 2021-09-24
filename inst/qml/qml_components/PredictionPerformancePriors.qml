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
import QtQuick			2.12
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0
import QtQuick.Layouts	1.3

Section
{
	title:		qsTr("Priors")
	expanded:	false
	columns:	1
	
	property string	measure:			"OE"

	Group
	{

		title: 				if(measure == "OE"){qsTr("Summary estimate (O:E Ratio)")}else{qsTr("Summary estimate (C statistic)")}
		columns:			2

			DoubleField
			{
				label: 			qsTr("Normal   mean:")
				name: 			"priorMuNMeam"
				defaultValue: 	0
				fieldWidth: 	50
			}

			DoubleField
			{
				label: 			qsTr("sd:")
				name: 			"priorMuNSD"
				defaultValue:	if(measure == "OE"){10}else{100} 
				min:			0
				max:			if(measure == "OE"){10}else{100} 
				fieldWidth: 	50
			}
	}

	RadioButtonGroup
	{
		title: 		qsTr("Heterogeneity (Between study SD)")
		name: 		"priorTau"

		RadioButton
		{
			name: 				"priorTauU"
			label: 				qsTr("Uniform")
			childrenOnSameRow: 	true
			checked: 			true

			DoubleField
			{
				label: 			qsTr("min:")
				name: 			"priorTauUMin"
				id:				priorTauUMin
				defaultValue: 	0
				min:			0
				max:			priorTauUMax
				fieldWidth: 	50
			}

			DoubleField
			{
				label: 			qsTr("max:")
				name: 			"priorTauUMax"
				id:				priorTauUMax
				defaultValue:	2
				min:			priorTauUMin.value
				fieldWidth: 	50
			}
		}

		RadioButton
		{
			name: 				"priorTauT"
			label: 				qsTr("Half t")
			childrenOnSameRow: 	true

			DoubleField
			{
				label: 			qsTr("location:")
				name: 			"priorTauTLocation"
				defaultValue: 	0
				fieldWidth: 	50
			}

			DoubleField
			{
				label: 			qsTr("scale:")
				name: 			"priorTauTScale"
				defaultValue: 	1.5
				min:			0
				fieldWidth: 	50
			}

			IntegerField
			{
				label: 			qsTr("df:")
				name: 			"priorTauTDf"
				defaultValue: 	3
				min:			1
			}

			DoubleField
			{
				label: 			qsTr("min:")
				name: 			"priorTauTMin"
				id:				priorTauTMin
				defaultValue: 	0
				min:			0
				max:			priorTauTMax.value
				fieldWidth: 	50
			}

			DoubleField
			{
				label: 			qsTr("max:")
				name: 			"priorTauTMax"
				defaultValue:	10
				id:				priorTauTMax
				min:			priorTauTMin
				fieldWidth: 	50
			}
		}
	}

}
