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
import "../qml/qml_components" as MA

Form
{
	VariablesForm
	{
		preferredHeight: 400 * preferencesModel.uiScale
		AvailableVariablesList { name: "allVariables" }
		AssignedVariablesList { name: "effectSize";	title: qsTr("Effect Size"); singleVariable: true; allowedColumns: ["scale"] }
		AssignedVariablesList { name: "effectSizeSe"; title: qsTr("Effect Size Standard Error"); singleVariable: true; allowedColumns: ["scale"] }
		MA.ClassicalMetaAnalysisMethod{ visible: true}
		AssignedVariablesList { name: "studyLabel"; title: qsTr("Study Labels"); singleVariable: true; allowedColumns: ["nominal"] }
		AssignedVariablesList { name: "covariates";	title: qsTr("Covariates"); allowedColumns: ["scale"] }
		AssignedVariablesList { name: "factors"; title: qsTr("Factors"); allowedColumns: ["nominal"] }
	}

	Section
	{
		title: qsTr("Model")
		VariablesForm
		{
			preferredHeight: 150 * preferencesModel.uiScale
			AvailableVariablesList { name: "modelComponents"; title: qsTr("Components"); source: ["covariates","factors"]}
			AssignedVariablesList  { name: "modelTerms"; title: qsTr("Model Terms"); listViewType: JASP.Interaction }
		}
		CheckBox { name: "interceptTerm"; label: qsTr("Include intercept"); checked: true }
	}

	MA.ClassicalMetaAnalysisStatistics{}

	MA.ClassicalMetaAnalysisDiagnostics{}
}