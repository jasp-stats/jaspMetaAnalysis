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
        RadioButton{ value: "fisherZ"; label: qsTr("Fisher's Z"); checked: true }
        RadioButton{ value: "cohenD";  label: qsTr("Cohen's D")                 }
        RadioButton{ value: "corr";    label: qsTr("Correlation")               }
        RadioButton{ value: "logOR";   label: qsTr("Log odds ratio")            }
    }

    Section
    {   
        title: qsTr("Data")
        VariablesForm
        {
            height: 8000
                AvailableVariablesList{name: "allVariables"}

                AssignedVariablesList
                {
                    name:           "fisherZs"
                    label:          qsTr("Fisher's Z")
                    allowedColumns: ["scale", "ordinal"]
                    singleVariable: true
                }

                AssignedVariablesList
                { 
                    name:           "cohenDs"
                    label:          qsTr("Cohen's D")
                    allowedColumns: ["scale", "ordinal"]
                    singleVariable: true
                }

                AssignedVariablesList
                {
                    name:           "corrs"
                    label:          qsTr("Correlation")
                    allowedColumns: ["scale", "ordinal"]
                    singleVariable: true
                }

                AssignedVariablesList
                {
                    name:           "logORs"
                    label:          qsTr("Log odds ratio")
                    allowedColumns: ["scale", "ordinal"]
                    singleVariable: true
                }
                
                 AssignedVariablesList
                { 
                    name:           "varMeasures"
                    label:          qsTr("Variability Measures")
                    allowedColumns: ["scale", "ordinal"]
                    rowComponent:   DropDown
                                    {
                                        name:   "ddVarMeasures"
                                        label:  ""
                                        values: ["Standard Error", "Variance", "Sample size", "T or Z statistics", "CI Lower Bound", "CI Upper Bound"]
                                    }
                }
        }
    }
    

    Section
    {
        title: qsTr("Save")

        Group
        {
            title: qsTr("Save the new measure!")
            TextField
            {
                name:            "newName"
                label:           qsTr("Name of the variable:")
                placeholderText: qsTr("New_Variable") 
            }
        }


    }

    

}