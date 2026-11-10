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
import "qml_components" as MA

Form
{
	info: qsTr("Adjust for publication bias with WAAP-WLS meta-analyses fitted with metafor. WLS is the meta-analysis of all estimates; WAAP (weighted average of the adequately powered) is the meta-analysis of the estimates whose power to detect the WLS effect reaches the power criterion. WAAP is reported if enough estimates are adequately powered, WLS otherwise (Stanley, Doucouliagos, & Ioannidis, 2017). Moderators, subgroups, clustering, and the optional multilevel/multivariate model use the same specification as the classical meta-analyses. When WAAP is reported, the study-level output (e.g., forest plot, diagnostics, and exported values) includes only the retained estimates.")
	infoBottom: "## " + qsTr("References") + "\n" +
  "- Carter, EC, Schönbrodt, FD, Gervais, WM, & Hilgard, J (2019). “Correcting for bias in psychology: A comparison of meta-analytic methods.” _Advances in Methods and Practices in Psychological Science, 2_(2), 115-144. https://doi.org/10.1177/2515245919847196\n" +
  "- Ioannidis, JPA, Stanley, TD, & Doucouliagos, H (2017). “The power of bias in economics research.” _The Economic Journal, 127_(605), F236-F265. https://doi.org/10.1111/ecoj.12461\n" +
  "- Stanley, TD, & Doucouliagos, H (2017). “Neither fixed nor random: Weighted least squares meta-regression.” _Research Synthesis Methods, 8_(1), 19-42. https://doi.org/10.1002/jrsm.1211\n" +
  "- Stanley, TD, Doucouliagos, H, & Ioannidis, JPA (2017). “Finding the power to reduce publication bias.” _Statistics in Medicine, 36_(10), 1580-1598. https://doi.org/10.1002/sim.7228\n" +
	"## " + qsTr("R Packages") + "\n" +
	"- metafor"
	VariablesForm
	{
		preferredHeight: 550 * preferencesModel.uiScale
		AvailableVariablesList { name: "allVariables" }
		AssignedVariablesList
		{
			name: "effectSize"
			id: effectSize
			title: qsTr("Effect Size")
			singleVariable: true
			allowedColumns: ["scale"]
			info: qsTr("Observed effect sizes, on the scale used to fit the model.")
		}
		AssignedVariablesList
		{
			name: "effectSizeStandardError"
			id: effectSizeStandardError
			title: qsTr("Effect Size Standard Error")
			singleVariable: true
			allowedColumns: ["scale"]
			info: qsTr("Standard errors corresponding to the effect sizes. The power of each estimate is computed from its standard error.")
		}
		MA.ClassicalMetaAnalysisMethod
		{
			id: method
			analysisType: "waapWls"
			multilevelMultivariate: multilevelMultivariateCheckBox.checked
		}
		DropDown
		{
			name: "fixedEffectTest"
			label: qsTr("Fixed effects test")
			startValue: "knha"
			enabled: multilevelMultivariateCheckBox.checked || method.value !== "unrestrictedWeightedLeastSquares"
			// the multilevel/multivariate model lists 't' first: the first value replaces 'knha' when the model is selected
			values: multilevelMultivariateCheckBox.checked ? (clustering.count > 0 ? [ "t" ] : [ "t", "z" ]) : method.value === "unrestrictedWeightedLeastSquares" ? [ "knha" ] : clustering.count > 0 ? [ "t", "knha" ] : [ "z", "t", "knha" ]
			info: qsTr("Method for testing the model coefficients: 'z' uses standard normal approximation, 't' uses t-distribution, and 'knha' uses the Knapp and Hartung adjustment (default). Unrestricted weighted least squares always uses the 'knha' test; clustered models use t-distribution based tests. The multilevel/multivariate model uses 'z' or 't' tests.")
		}
		AssignedVariablesList
		{
			name: "predictors"
			id: predictors
			title: qsTr("Predictors")
			allowedColumns: ["nominal", "scale"]
			allowTypeChange: true
			info: qsTr("Continuous and categorical moderators. Specify interactions and the intercept in Model.")
		}
		AssignedVariablesList
		{
			name: "clustering"
			id: clustering
			title: qsTr("Clustering")
			singleVariable: true
			allowedColumns: ["nominal"]
			info: qsTr("Variable indicating clustering of effect sizes for robust variance estimation. If the variable is specified, cluster-robust tests and confidence intervals are provided. See the 'Advanced' section for additional clustering options.")
		}
		AssignedVariablesList
		{
			name: "studyLabels"
			title: qsTr("Study Labels")
			singleVariable: true
			allowedColumns: ["nominal"]
			info: qsTr("Labels for studies in plots. Missing labels do not exclude observations from fitting.")
		}
		AssignedVariablesList
		{
			name: "subgroup"
			id: subgroup
			title: qsTr("Subgroup")
			singleVariable: true
			allowedColumns: ["nominal"]
			info: qsTr("Fit the models separately within each subgroup. The power of the estimates and the WAAP-WLS model selection are evaluated separately for each subgroup.")
		}
		AssignedVariablesList
		{
			id: noSelection
			name: "noSelection"
			title: qsTr("No Selection")
			singleVariable: true
			allowedColumns: ["nominal"]
			allowTypeChange: true
			info: qsTr("Optional factor identifying estimates unaffected by publication bias. Choose the unaffected level below; estimates at that level are always retained in the WAAP model, regardless of their power.")
		}
		DropDown
		{
			name: "noSelectionLevel"
			label: qsTr("Unaffected Level")
			enabled: noSelection.count > 0
			source: [{ name: "noSelection", use: "levels" }]
			info: qsTr("Estimates at this level are unaffected by publication bias. The power criterion applies to all other levels of the assigned factor.")
		}
	}
	Group
	{
		PercentField
		{
			name: "powerCriterion"
			label: qsTr("Power criterion")
			defaultValue: 80
			min: 5
			max: 99
			info: qsTr("Power that an estimate needs to reach to be retained in the WAAP model (default 80%, between 5% and 99%). The power is the probability that a two-sided z-test (α = .05) with the standard error of the estimate detects the effect of the WLS model. At 5%, all estimates are retained.")
		}
		CheckBox
		{
			name: "powerConditionalOnModerators"
			label: qsTr("Power conditional on moderators")
			enabled: sectionModel.effectSizeModelTermsCount > 0
			checked: true
			info: qsTr("Evaluate the power of each estimate at its predicted effect from the WLS meta-regression, i.e., conditional on its moderator values (default). If unticked, the power of all estimates is evaluated at the pooled effect of the WLS model (at the mean of the moderators). Available when the model includes moderators.")
		}
		CheckBox
		{
			name: "multilevelMultivariate"
			id: multilevelMultivariateCheckBox
			label: qsTr("Multilevel/multivariate")
			checked: false
			// unticking returns to the default UWLS method (restricted ML would otherwise remain selected)
			onClicked: if (!checked) Qt.callLater(function() { method.currentValue = "unrestrictedWeightedLeastSquares" })
			info: qsTr("Fit the WLS and WAAP models as multilevel/multivariate models (metafor's 'rma.mv' function). Specify the dependency between the effect sizes in the 'Effect Size Variance-Covariance Matrix' and 'Random Effects / Model Components' sections. The power of each estimate is computed from its standard error.")
		}
	}
	MA.ClassicalMetaAnalysisMultivariate
	{
		analysisType: "multilevelMultivariateMetaAnalysis"
		visible: multilevelMultivariateCheckBox.checked
	}
	MA.ClassicalMetaAnalysisRandomEffects { visible: multilevelMultivariateCheckBox.checked }
	MA.ClassicalMetaAnalysisModel { id: sectionModel; analysisType: "waapWls"; methodValue: method.value }
	MA.ClassicalMetaAnalysisStatistics { id: sectionStatistics; analysisType: "waapWls"; multilevelMultivariate: multilevelMultivariateCheckBox.checked }
	MA.ClassicalMetaAnalysisEstimatedMarginalMeans { analysisType: "waapWls" }
	MA.ForestPlotSection
	{
		analysisType: "waapWls"
		transformEffectSizeValue: sectionStatistics.transformEffectSizeValue
		effectSizeReady: effectSize.count === 1 && effectSizeStandardError.count === 1
		modelInformationEnabled: effectSizeReady
		effectSizeModelTermsCount: sectionModel.effectSizeModelTermsCount
		methodValue: method.value
		subgroupSelected: subgroup.count > 0
		multilevelMultivariate: multilevelMultivariateCheckBox.checked
	}
	MA.BubblePlot { analysisType: "waapWls"; methodValue: method.value }
	MA.ClassicalMetaAnalysisDiagnostics { analysisType: "waapWls"; methodValue: method.value; multilevelMultivariate: multilevelMultivariateCheckBox.checked }
	MA.ClassicalMetaAnalysisExport { analysisType: "waapWls"; multilevelMultivariate: multilevelMultivariateCheckBox.checked }
	MA.ClassicalMetaAnalysisAdvanced
	{
		id: sectionAdvanced
		analysisType: "waapWls"
		multilevelMultivariate: multilevelMultivariateCheckBox.checked
	}
}
