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
import JASP

Section
{
	title:		qsTr("Priors")
	expanded:	false
	columns:	1
	
	property string	measure:			"oeRatio"

	Group
	{

		title: 				measure === "oeRatio" ? qsTr("mu: summary estimate (O/E Ratio)") :  qsTr("mu: summary estimate (C statistic)")
		columns:			2

			DoubleField
			{
				label: 			qsTr("Normal mean:")
				name: 			"muNormalPriorMean"
				defaultValue: 	0
				fieldWidth: 	40 * preferencesModel.uiScale
			}

			DoubleField
			{
				label: 			qsTr("sd:")
				name: 			"muNormalPriorSd"
				defaultValue:	measure === "oeRatio" ? 10 : 100
				min:			0
				max:			measure === "oeRatio" ? 10 : 100
				fieldWidth: 	40 * preferencesModel.uiScale
			}
	}

	RadioButtonGroup
	{
		title: 		qsTr("tau: heterogeneity (Between study SD)")
		name: 		"tauPrior"

		RadioButton
		{
			name: 				"uniformPrior"
			label: 				qsTr("Uniform")
			childrenOnSameRow: 	true
			checked: 			true

			DoubleField
			{
				label: 			qsTr("min:")
				name: 			"tauUniformPriorMin"
				id:				priorTauUMin
				defaultValue: 	0
				min:			0
				max:			priorTauUMax
				fieldWidth: 	40 * preferencesModel.uiScale
			}

			DoubleField
			{
				label: 			qsTr("max:")
				name: 			"tauUniformPriorMax"
				id:				priorTauUMax
				defaultValue:	2
				min:			priorTauUMin.value
				fieldWidth: 	40 * preferencesModel.uiScale
			}
		}

		RadioButton
		{
			name: 				"tPrior"
			label: 				qsTr("T")
			childrenOnSameRow: 	true

			DoubleField
			{
				label: 			qsTr("location:")
				name: 			"tauTPriorLocation"
				defaultValue: 	0
				fieldWidth: 	40 * preferencesModel.uiScale
			}

			DoubleField
			{
				label: 			qsTr("scale:")
				name: 			"tauTPriorScale"
				defaultValue: 	1.5
				min:			0
				fieldWidth: 	40 * preferencesModel.uiScale
			}

			IntegerField
			{
				label: 			qsTr("df:")
				name: 			"tauTPriorDf"
				defaultValue: 	3
				min:			1
				fieldWidth: 	40 * preferencesModel.uiScale
			}

			DoubleField
			{
				label: 			qsTr("min:")
				name: 			"tauTPriorMin"
				id:				priorTauTMin
				defaultValue: 	0
				min:			0
				max:			priorTauTMax.value
				fieldWidth: 	40 * preferencesModel.uiScale
			}

			DoubleField
			{
				label: 			qsTr("max:")
				name: 			"tauTPriorMax"
				defaultValue:	10
				id:				priorTauTMax
				min:			priorTauTMin
				fieldWidth: 	40 * preferencesModel.uiScale
			}
		}
	}

}
