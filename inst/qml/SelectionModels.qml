import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP
import "qml_components" as MA

Form
{
	info: qsTr("Fit publication-bias selection models with metafor, including moderators and comparisons between weight functions. All candidate models use the same observations within each subgroup.")
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
			analysisType: "selectionModels"
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
			info: qsTr("Fit each candidate separately within each subgroup. Best AIC/BIC is determined separately for each subgroup.")
		}
		AssignedVariablesList
		{
			name: "pValue"
			title: qsTr("P-Value")
			singleVariable: true
			allowedColumns: ["scale"]
			info: qsTr("Optional selection p-values, greater than 0 and at most 1. Use p-values matching the sidedness of each selection function. If unassigned, p-values are calculated from effect sizes and standard errors.")
		}
		AssignedVariablesList
		{
			id: noSelection
			name: "noSelection"
			title: qsTr("No Selection")
			singleVariable: true
			allowedColumns: ["nominal"]
			allowTypeChange: true
			info: qsTr("Optional factor identifying studies unaffected by selection. Choose the unaffected level below; selection applies to all other levels. Both remain in the analysis.")
		}
		DropDown
		{
			name: "noSelectionLevel"
			label: qsTr("Unaffected Level")
			enabled: noSelection.count > 0
			source: [{ name: "noSelection", use: "levels" }]
			info: qsTr("Studies at this level are unaffected by selection. Selection applies to all other levels of the assigned factor.")
		}
	}
	Group
	{
		DropDown
		{
			name: "publicationBiasAdjustment"
			id: publicationBiasAdjustment
			label: qsTr("Publication bias adjustment")
			startValue: "4PSM"
			values: [ { label: "4PSM", value: "4PSM" }, { label: "3PSM", value: "3PSM" }, { label: qsTr("Custom"), value: "custom" } ]
			info: qsTr("4PSM uses one-sided p-value cutoffs .025 and .50; 3PSM uses .025. Custom permits multiple weight functions.")
		}
		DropDown
		{
			name: "modelExpectedDirectionOfTheEffect"
			label: qsTr("Expected direction of the effect")
			startValue: "detect"
			values: [ { label: qsTr("Detect"), value: "detect" }, { label: qsTr("Positive"), value: "positive" }, { label: qsTr("Negative"), value: "negative" } ]
			info: qsTr("Detect uses the median effect in the fitted dataset, separately per subgroup; a zero median selects positive. Two-sided functions ignore direction.")
		}
		CheckBox
		{
			name: "selectionForceOrdinality"
			label: qsTr("Force ordinality")
			checked: false
			visible: publicationBiasAdjustment.value !== "custom"
			info: qsTr("Require non-increasing step-function weights. Under Custom, specify this for each model separately. Metafor's ordinal implementation is experimental; some intervals and tests are unavailable.")
		}
	}
	MA.ClassicalMetaAnalysisModel { id: sectionModel; analysisType: "selectionModels"; methodValue: method.value }
	MA.SelectionModelsWeightfunctions
	{
		customSelected: publicationBiasAdjustment.value === "custom"
	}
	MA.ClassicalMetaAnalysisStatistics { id: sectionStatistics; analysisType: "selectionModels" }
	MA.ClassicalMetaAnalysisEstimatedMarginalMeans { analysisType: "selectionModels" }
	MA.ForestPlotSection
	{
		analysisType: "selectionModels"
		transformEffectSizeValue: sectionStatistics.transformEffectSizeValue
		effectSizeReady: effectSize.count === 1 && effectSizeStandardError.count === 1
		modelInformationEnabled: effectSizeReady
		effectSizeModelTermsCount: sectionModel.effectSizeModelTermsCount
		methodValue: method.value
		publicationBiasAdjustmentValue: publicationBiasAdjustment.value
		subgroupSelected: subgroup.count > 0
	}
	MA.BubblePlot { analysisType: "selectionModels" }
	MA.ClassicalMetaAnalysisDiagnostics { analysisType: "selectionModels" }
	MA.ClassicalMetaAnalysisExport
	{
		analysisType: "selectionModels"
	}
	MA.ClassicalMetaAnalysisAdvanced
	{
		id: sectionAdvanced
		analysisType: "selectionModels"
	}
}
