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
	info: qsTr("Adjust for publication bias with PET-PEESE meta-regressions fitted with metafor. PET includes the standard errors and PEESE the sampling variances as an additional moderator; the adjusted estimates correspond to a study with a standard error of zero. Moderators, subgroups, clustering, and the optional multilevel/multivariate model use the same specification as the classical meta-analyses.")
	infoBottom: "## " + qsTr("References") + "\n" +
  "- Carter, EC, Schönbrodt, FD, Gervais, WM, & Hilgard, J (2019). “Correcting for bias in psychology: A comparison of meta-analytic methods.” _Advances in Methods and Practices in Psychological Science, 2_(2), 115-144. https://doi.org/10.1177/2515245919847196\n" +
  "- Stanley, TD, & Doucouliagos, H (2014). “Meta‐regression approximations to reduce publication selection bias.” _Research Synthesis Methods, 5_(1), 60-78. https://doi.org/10.1002/jrsm.1095\n" +
  "- Stanley, TD (2017). “Limitations of PET-PEESE and other meta-analysis methods.” _Social Psychological and Personality Science, 8_(5), 581-591. https://doi.org/10.1177/1948550617693062\n" +
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
			info: qsTr("Standard errors corresponding to the effect sizes.")
		}
		MA.ClassicalMetaAnalysisMethod
		{
			id: method
			analysisType: "petPeese"
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
			info: qsTr("Continuous and categorical moderators. Specify interactions and the intercept in Model. The bias term (standard error or sampling variance) is always included.")
		}
		AssignedVariablesList
		{
			name: "clustering"
			id: clustering
			title: qsTr("Clustering")
			singleVariable: true
			allowedColumns: ["nominal"]
			info: qsTr("Variable indicating clustering of effect sizes for robust variance estimation. If the variable is specified, cluster-robust tests and confidence intervals are provided, including the test used for the PET-PEESE model selection. See the 'Advanced' section for additional clustering options.")
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
			info: qsTr("Fit the models separately within each subgroup. The PET-PEESE model selection is performed separately for each subgroup.")
		}
		AssignedVariablesList
		{
			id: noSelection
			name: "noSelection"
			title: qsTr("No Selection")
			singleVariable: true
			allowedColumns: ["nominal"]
			allowTypeChange: true
			info: qsTr("Optional factor identifying estimates unaffected by publication bias. Choose the unaffected level below; the bias term is set to zero for estimates at that level, so only the remaining estimates inform the small-study effect. Both remain in the analysis.")
		}
		DropDown
		{
			name: "noSelectionLevel"
			label: qsTr("Unaffected Level")
			enabled: noSelection.count > 0
			source: [{ name: "noSelection", use: "levels" }]
			info: qsTr("Estimates at this level are unaffected by publication bias. The bias adjustment applies to all other levels of the assigned factor.")
		}
	}
	Group
	{
		DropDown
		{
			name: "publicationBiasAdjustment"
			id: publicationBiasAdjustment
			label: qsTr("Publication bias adjustment")
			startValue: "petPeese"
			values: [ { label: "PET-PEESE", value: "petPeese" }, { label: "PET", value: "pet" }, { label: "PEESE", value: "peese" } ]
			info: qsTr("PET adjusts for the relationship between the effect sizes and their standard errors, PEESE for the relationship with the sampling variances. PET-PEESE reports PEESE if the PET adjusted effect is significant at the PET-PEESE threshold (see 'Advanced'), and PET otherwise (Stanley & Doucouliagos, 2014).")
		}
		CheckBox
		{
			name: "multilevelMultivariate"
			id: multilevelMultivariateCheckBox
			label: qsTr("Multilevel/multivariate")
			checked: false
			// unticking returns to the default UWLS method (restricted ML would otherwise remain selected)
			onClicked: if (!checked) Qt.callLater(function() { method.currentValue = "unrestrictedWeightedLeastSquares" })
			info: qsTr("Fit the PET and PEESE meta-regressions as multilevel/multivariate models (metafor's 'rma.mv' function). Specify the dependency between the effect sizes in the 'Effect Size Variance-Covariance Matrix' and 'Random Effects / Model Components' sections. The standard errors used as the bias term are the square roots of the sampling variances.")
		}
	}
	MA.ClassicalMetaAnalysisMultivariate
	{
		analysisType: "multilevelMultivariateMetaAnalysis"
		visible: multilevelMultivariateCheckBox.checked
	}
	MA.ClassicalMetaAnalysisRandomEffects { visible: multilevelMultivariateCheckBox.checked }
	MA.ClassicalMetaAnalysisModel { id: sectionModel; analysisType: "petPeese"; methodValue: method.value }
	MA.ClassicalMetaAnalysisStatistics { id: sectionStatistics; analysisType: "petPeese"; multilevelMultivariate: multilevelMultivariateCheckBox.checked }
	MA.ClassicalMetaAnalysisEstimatedMarginalMeans { analysisType: "petPeese" }
	MA.ForestPlotSection
	{
		analysisType: "petPeese"
		transformEffectSizeValue: sectionStatistics.transformEffectSizeValue
		effectSizeReady: effectSize.count === 1 && effectSizeStandardError.count === 1
		modelInformationEnabled: effectSizeReady
		effectSizeModelTermsCount: sectionModel.effectSizeModelTermsCount
		methodValue: method.value
		publicationBiasAdjustmentValue: publicationBiasAdjustment.value
		subgroupSelected: subgroup.count > 0
		multilevelMultivariate: multilevelMultivariateCheckBox.checked
	}
	MA.BubblePlot { analysisType: "petPeese"; methodValue: method.value }
	MA.ClassicalMetaAnalysisDiagnostics { analysisType: "petPeese"; methodValue: method.value; multilevelMultivariate: multilevelMultivariateCheckBox.checked }
	MA.ClassicalMetaAnalysisExport { analysisType: "petPeese"; multilevelMultivariate: multilevelMultivariateCheckBox.checked }
	MA.ClassicalMetaAnalysisAdvanced
	{
		id: sectionAdvanced
		analysisType: "petPeese"
		multilevelMultivariate: multilevelMultivariateCheckBox.checked
		petPeeseConditional: publicationBiasAdjustment.value === "petPeese"
	}
}
