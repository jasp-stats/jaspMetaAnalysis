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
	title:						qsTr("Model")
	columns:					2
	info: qsTr("Options for specifing the effect size and heterogeneity models based on the included predictors, including model terms, intercepts, and link functions.")

	property string analysisType: "RoBMA"
	// RoBMA: Robust Bayesian Meta-Analsis
	// BiBMA: Binomial Bayesian Meta-Analysis
	// NoBMA: Normal Bayesian Meta-Analysis

	property alias effectSizeModelTerms:				effectSizeModelTerms
	property alias effectSizeModelTermsCount:			effectSizeModelTerms.count


	Group
	{
		title: qsTr("Effect size model")
		info: qsTr("Specify the effect size model.")

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
				info: qsTr("Variables assigned as model terms in the effect size model.")
			}
		}
/* this is directly controlled by setting the prior on the effect size
		CheckBox
		{
			name:				"effectSizeModelIncludeIntercept"
			label:				qsTr("Include intercept")
			checked:			true
			info: qsTr("Include an intercept in the effect size model.")
		}
*/
	}
}
