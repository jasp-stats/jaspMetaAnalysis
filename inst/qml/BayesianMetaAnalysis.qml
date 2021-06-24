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
import "../qml/qml_components" as MA

Form
{
 	id: form

	//// Variable inputs ////
	VariablesForm
	{
		preferredHeight:	200 * preferencesModel.uiScale

		AvailableVariablesList
		{
			name: "variablesList"
		}

		AssignedVariablesList
		{
			name: 			"effectSize"
			title: 			qsTr("Effect Size")
			singleVariable: true
			allowedColumns: ["scale"]
		}

		AssignedVariablesList
		{
			id: 			standardError
			enabled: 		confidenceInterval.count < 1 // Only if no confidence interval input
			name: 			"standardError"
			title: 			qsTr("Effect Size Standard Error")
			singleVariable: true
			allowedColumns: ["scale"]
		}

		AssignedPairsVariablesList
		{
			id: 			confidenceInterval
			enabled: 		standardError.count == 0 // Only if no standard error input (only one of the two is necessary)
			name: 			"confidenceInterval"
			title: 			qsTr("95% CI Lower and Upper Bound")
			singleVariable: true
			allowedColumns: ["scale"]
		}

		AssignedVariablesList
		{
			name: 			"studyLabels"
			title: 			qsTr("Study Labels")
			singleVariable:	true
			allowedColumns: ["nominalText"]
		}
	}

	MA.BayesianMetaAnalysisInference{
		id:						bayesianMetaAnalysisInference
	}

	MA.BayesianMetaAnalysisPlots{
		modelTypeValue:			bayesianMetaAnalysisInference.modelTypeValue
		modelDirectionValue:	bayesianMetaAnalysisInference.modelDirectionValue
	}

	MA.BayesianMetaAnalysisPriors{
		modelTypeValue:			bayesianMetaAnalysisInference.modelTypeValue
		modelDirectionValue:	bayesianMetaAnalysisInference.modelDirectionValue
	}

	MA.BayesianMetaAnalysisAdvanced{
		modelTypeValue:			bayesianMetaAnalysisInference.modelTypeValue
		modelDirectionValue:	bayesianMetaAnalysisInference.modelDirectionValue
	}
	
}
