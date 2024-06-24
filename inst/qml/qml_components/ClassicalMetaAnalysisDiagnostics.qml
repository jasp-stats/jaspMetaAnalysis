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
	title:						qsTr("Diagnostics")
	property string module:		"metaAnalysis"
	columns:					1

	Group
	{
		columns:	2

		Group
		{
			CheckBox
			{
				name:		"diagnosticsVarianceInflationFactor"
				text:		qsTr("Variace inflation factor")
				Layout.preferredWidth: 300 * jaspTheme.uiScale
				enabled:	predictors.count > 0

				CheckBox
				{
					name:		"diagnosticsVarianceInflationFactorAggregate"
					text:		qsTr("Aggregate by terms")
					checked:	true
				}
			}

			CheckBox
			{
				name:		"diagnosticsCasewiseDiagnostics"
				text:		qsTr("Casewise diagnostics")

				CheckBox
				{
					name:		"diagnosticsCasewiseDiagnosticsShowInfluentialOnly"
					text:		qsTr("Show influential only")
				}

				CheckBox
				{
					name:		"diagnosticsCasewiseDiagnosticsIncludePredictors"
					text:		qsTr("Include predictors")
				}

				CheckBox
				{
					name:		"diagnosticsCasewiseDiagnosticsDifferenceInCoefficients"
					text:		qsTr("Difference in coefficients")
				}

				CheckBox
				{
					name:		"diagnosticsCasewiseDiagnosticsExportToDataset"
					text:		qsTr("Export to dataset")

					CheckBox
					{
						name:		"diagnosticsCasewiseDiagnosticsExportToDatasetInfluentialIndicatorOnly"
						text:		qsTr("Influential indicator only")
						checked:	true
					}
				}

				CheckBox
				{
					name:		"diagnosticsCasewiseDiagnosticsRerunWithoutInfluentialCases"
					text:		qsTr("Rerun without influential cases")
					visible:	false
				}
			}
		}

		Group
		{
			title:		qsTr("Plots")

			CheckBox
			{
				name:		"diagnosticsPlotsProfileLikelihood"
				text:		qsTr("Profile likelihood")
			}

			CheckBox
			{
				name:		"diagnosticsPlotsBaujat"
				text:		qsTr("Baujat")
			}
		}
	}
}