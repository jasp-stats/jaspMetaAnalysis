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

DropDown
{
	name: "method"; label: qsTr("Method"); currentIndex: 2
	values: [
		{ label: qsTr("Fixed Effects")		, value: "Fixed Effects"		},
		{ label: qsTr("Maximum Likelihood")	, value: "Maximum Likelihood"	},
		{ label: qsTr("Restricted ML")		, value: "Restricted ML"		},
		{ label: qsTr("DerSimonian-Laird")	, value: "DerSimonian-Laird"	},
		{ label: qsTr("Hedges")				, value: "Hedges"				},
		{ label: qsTr("Hunter-Schmidt")		, value: "Hunter-Schmidt"		},
		{ label: qsTr("Sidik-Jonkman")		, value: "Sidik-Jonkman"		},
		{ label: qsTr("Empirical Bayes")	, value: "Empirical Bayes"		},
		{ label: qsTr("Paule-Mandel")		, value: "Paule-Mandel"			}
	]
}