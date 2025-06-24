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
	title:							qsTr("Diagnostics")
	property string analysisType:	"metaAnalysis"
	columns:						1
	info: qsTr("Options for evaluating the influence of individual studies and assessing model diagnostics, including variance inflation factors, casewise diagnostics, and diagnostic plots.")

	Group
	{
		columns:	2

		Group
		{
			CheckBox
			{
				name:		"diagnosticsVarianceInflationFactor"
				text:		qsTr("Variance inflation factor")
				Layout.preferredWidth: 300 * jaspTheme.uiScale
				enabled:	predictors.count > 0
				info: qsTr("Include variance inflation factors to assess multicollinearity among predictors. Available when predictors are included in the model.")

				CheckBox
				{
					name:		"diagnosticsVarianceInflationFactorAggregate"
					text:		qsTr("Aggregate by terms")
					checked:	true
					info: qsTr("Aggregate variance inflation factors by terms instead of individual coefficients.")
				}
			}

			CheckBox
			{
				name:		"diagnosticsCasewiseDiagnostics"
				text:		qsTr("Casewise diagnostics")
				info: qsTr("Include casewise diagnostics to assess the influence of individual studies on the meta-analysis results. Note that diagnostics are always based on the non-clustered model.")

				CheckBox
				{
					name:		"diagnosticsCasewiseDiagnosticsShowInfluentialOnly"
					text:		qsTr("Show influential only")
					visible:	analysisType === "metaAnalysis"
					info: qsTr("Show only the influential studies in the casewise diagnostics. Unavailable when performing multilevel/multivariate meta-analysis.")
				}

				CheckBox
				{
					name:		"diagnosticsCasewiseDiagnosticsIncludePredictors"
					text:		qsTr("Include predictors")
					info: qsTr("Include predictor variables in the casewise diagnostics output.")
				}

				CheckBox
				{
					name:		"diagnosticsCasewiseDiagnosticsDifferenceInCoefficients"
					text:		qsTr("Difference in coefficients")
					info: qsTr("Include the differences in model coefficients when each study is excluded (DFBETAS).")
				}

				CheckBox
				{
					name:		"diagnosticsCasewiseDiagnosticsExportToDataset"
					text:		qsTr("Export to dataset")
					info: qsTr("Export the casewise diagnostics results to the dataset.")

					CheckBox
					{
						name:		"diagnosticsCasewiseDiagnosticsExportToDatasetInfluentialIndicatorOnly"
						text:		qsTr("Influential indicator only")
						checked:	true
						visible:	analysisType === "metaAnalysis"
						info: qsTr("Export only the indicator of influential cases to the dataset.")
					}
				}

				/*
				CheckBox
				{
					name:		"diagnosticsCasewiseDiagnosticsRerunWithoutInfluentialCases"
					text:		qsTr("Rerun without influential cases")
					visible:	false
					info: qsTr("Option to rerun the analysis without influential cases.")
				}
				*/
			}
		}

		Group
		{
			title:		qsTr("Plots")

			CheckBox
			{
				name:		"diagnosticsPlotsProfileLikelihood"
				text:		qsTr("Profile likelihood")
				info: qsTr("Include a profile likelihood plot for the heterogeneity parameter (τ²).")
			}

			CheckBox
			{
				name:		"diagnosticsPlotsBaujat"
				text:		qsTr("Baujat")
				visible:	analysisType === "metaAnalysis"
				info: qsTr("Include a Baujat plot to detect studies contributing to heterogeneity and overall effect size. Unavailable when performing multilevel/multivariate meta-analysis. Note that Baujat plot is always based on the non-clustered model.")
			}

			CheckBox
			{
				name:		"diagnosticsResidualFunnel"
				text:		qsTr("Residual funnel")
				info: qsTr("Include a residual funnel plot. Note that residual funnel plot is always based on the non-clustered model.")
			}
		}
	}
}
