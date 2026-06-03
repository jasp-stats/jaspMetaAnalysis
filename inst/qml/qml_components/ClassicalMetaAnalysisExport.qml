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
	title:							qsTr("Export")
	property string analysisType:	"metaAnalysis"
	columns:						2
	info: qsTr("Options for exporting model-derived quantities to the dataset.")

	Group
	{
		title:		qsTr("Diagnostics")
		visible:	analysisType === "metaAnalysis" || analysisType === "multilevelMultivariateMetaAnalysis"

		CheckBox
		{
			name:		"exportDiagnosticsInfluentialCases"
			text:		qsTr("Influential cases")
			visible:	analysisType === "metaAnalysis"
			info: qsTr("Export an indicator for influential cases.")
		}

		CheckBox
		{
			name:	"exportDiagnosticsCaseDiagnostics"
			text:	qsTr("Case diagnostics")
			info: qsTr("Export casewise residual, Cook's distance, leverage, and weight diagnostics where available.")
		}

		CheckBox
		{
			name:		"exportDiagnosticsModelImpact"
			text:		qsTr("Model impact")
			visible:	analysisType === "metaAnalysis"
			info: qsTr("Export diagnostics for how individual cases affect the model fit.")
		}

		CheckBox
		{
			name:	"exportDiagnosticsCoefficientInfluence"
			text:	qsTr("Coefficient influence")
			info: qsTr("Export coefficient influence diagnostics.")
		}
	}

	Group
	{
		title:		qsTr("Residuals")

		CheckBox
		{
			name:	"exportResidualsRaw"
			text:	qsTr("Raw")
			info: qsTr("Export raw residuals.")
		}

		CheckBox
		{
			name:	"exportResidualsPearson"
			text:	qsTr("Pearson")
			info: qsTr("Export Pearson residuals.")
		}

		CheckBox
		{
			name:	"exportResidualsStandardized"
			text:	qsTr("Standardized")
			info: qsTr("Export standardized residuals.")
		}

		CheckBox
		{
			name:	"exportResidualsStudentized"
			text:	qsTr("Studentized")
			info: qsTr("Export studentized residuals.")
		}

		CheckBox
		{
			name:		"exportResidualsConditional"
			text:		qsTr("Conditional standardized")
			visible:	analysisType === "metaAnalysis"
			info: qsTr("Export conditional standardized residuals.")
		}
	}

	Group
	{
		title:		qsTr("Model Values")

		CheckBox
		{
			name:	"exportPredictedValues"
			text:	qsTr("Predicted values")
			info: qsTr("Export predicted values and interval bounds.")
		}

		CheckBox
		{
			name:		"exportTrueEffectEstimates"
			text:		qsTr("True effect estimates (BLUPs)")
			visible:	analysisType === "metaAnalysis"
			info: qsTr("Export best linear unbiased predictions of the study-specific true effects.")
		}

		CheckBox
		{
			name:	"exportRandomEffects"
			text:	qsTr("Random effects")
			info: qsTr("Export predicted random effects.")
		}

		CheckBox
		{
			name:	"exportWeights"
			text:	qsTr("Weights")
			info: qsTr("Export model fitting weights. These are not coefficient-specific contribution weights in meta-regression. For multilevel/multivariate models, diagonal weights match the default forest plot weights and row-sum weights are exported for intercept-only models.")
		}
	}
}
