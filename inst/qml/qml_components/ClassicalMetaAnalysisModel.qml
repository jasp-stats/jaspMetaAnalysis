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

Section
{
	title:						qsTr("Model")
	columns:					2
	property string module:		"metaAnalysis"

	property alias effectSizeModelTerms:				effectSizeModelTerms
	property alias effectSizeModelTermsCount:			effectSizeModelTerms.count
	property alias heterogeneityModelTermsCount:		heterogeneityModelTerms.count
	property alias heterogeneityModelLinkValue:			heterogeneityModelLink.value
	

	Group
	{
		title: qsTr("Effect size model")

		VariablesForm
		{
			preferredHeight:	150 * preferencesModel.uiScale

			AvailableVariablesList
			{
				name:			"effectSizeModelAvailableComponents"
				title:			qsTr("Available Components")
				source:			["predictors"]
			}

			AssignedVariablesList
			{
				name:			"effectSizeModelTerms"
				id:				effectSizeModelTerms
				title:			qsTr("Model Terms")
				listViewType:	JASP.Interaction
				allowTypeChange:false
			}
		}

		CheckBox
		{
			name:				"effectSizeModelIncludeIntercept"
			label:				qsTr("Include intercept")
			checked:			true
		}
	}

	Group
	{
		title:			qsTr("Heterogeneity model")
		visible:		module =="metaAnalysis"
		columns:		2

		VariablesForm
		{
			preferredHeight:	150 * preferencesModel.uiScale

			AvailableVariablesList
			{
				name:			"heterogeneityModelAvailableComponents"
				title:			qsTr("Available Components")
				source:			["predictors"]
			}

			AssignedVariablesList
			{
				name:			"heterogeneityModelTerms"
				id:				heterogeneityModelTerms
				title:			qsTr("Model Terms")
				listViewType:	JASP.Interaction
				allowTypeChange:false
				addAvailableVariablesToAssigned: false
			}
		}

		CheckBox
		{
			name:		"heterogeneityModelIncludeIntercept";
			label:		qsTr("Include intercept")
			checked:	true
		}

		DropDown
		{
			name:		"heterogeneityModelLink"
			id:			heterogeneityModelLink
			label:		qsTr("Link")
			values:		["log", "identity"]
		}
	}
}