import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Section
{
	title: qsTr("Weight Functions (Custom)")
	columns: 1
	property bool customSelected: false
	enabled: customSelected
	readonly property bool multipleModels: customSelected && models.count > 1
	info: qsTr("Each element is a separate candidate model. Cutoffs use the selected p-value sidedness; truncation cutoffs use the effect-size scale. None adds the unadjusted model to the comparison.")
	DropDown
	{
		name: "showSelectionModels"
		label: qsTr("Show models")
		visible: multipleModels
		startValue: "all"
		values: {
			var choices = [
				{ label: qsTr("All"), value: "all" },
				{ label: qsTr("Best BIC"), value: "bestBIC" },
				{ label: qsTr("Best AIC"), value: "bestAIC" }
			];
			for (var i = 1; i <= models.count; i++)
				choices.push({ label: qsTr("Model %1").arg(i), value: "model" + i });
			return choices;
		}
		info: qsTr("Filter detailed output when comparing multiple custom models. Comparisons and exports always include all candidates. Ties are resolved by model order.")
	}
	CheckBox
	{
		name: "modelComparison"
		label: qsTr("Model comparison")
		checked: true
		visible: multipleModels
		info: qsTr("Compare all custom models using fit statistics and AIC, AICc, and BIC weights, separately by subgroup.")
	}
	ComponentsList
	{
		id: models
		name: "selectionModels"
		optionKey: "name"
		minimumItems: 1
		newItemValue: "model"
		defaultValues: [ { name: "model1", type: "stepfun", steps: "(.025, .50)", delta: "", sidedness: "oneSided", prec: "none", sampleSize: "", scaleprec: true, decreasing: false } ]
		rowComponent: SelectionModelsWeightfunction { modelIndex: rowIndex }
	}
}
