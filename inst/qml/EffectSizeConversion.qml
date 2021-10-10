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

Form
{
    RadioButtonGroup
    {
        name:                   "EffectSizeType"
        title:                  qsTr("Choose your prefered effect size measure!")
        radioButtonsOnSameRow:  true
        RadioButton{ value: "fisherZ"; label: qsTr("Fisher Z"); checked: true }
        RadioButton{ value: "cohenD"; label: qsTr("Cohen D") }
        RadioButton{ value: "corr"; label: qsTr("Correlation") }
        RadioButton{ value: "logOR"; label: qsTr("Log odds ratio") }
    }

    VariablesForm
    {
        AvailableVariablesList{ name: "allVariables" }
        AssignedVariablesList{ name: "fisherZs"; label: qsTr("Fisher's Z"); allowedColumns: ["scale", "ordinal"] }
        AssignedVariablesList{ name: "cohenDs"; label: qsTr("Cohen's D"); allowedColumns: ["scale", "ordinal"] }
        AssignedVariablesList{ name: "corrs"; label: qsTr("Correlation"); allowedColumns: ["scale", "ordinal"] }
        AssignedVariablesList{ name: "logORs"; label: qsTr("Log odds ratio"); allowedColumns: ["scale", "ordinal"] }
    }

}