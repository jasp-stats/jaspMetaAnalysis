import QtQuick
import JASP.Controls

Group
{
	property int modelIndex: 0
	title: qsTr("Model %1").arg(modelIndex + 1)
	columns: 2
	readonly property bool selectionEnabled: functionType.value !== "none"
	readonly property bool truncation: functionType.value === "trunc" || functionType.value === "truncest"
	readonly property bool supportsPrecision: ["halfnorm", "negexp", "logistic", "power", "negexppow"].includes(functionType.value)
	DropDown
	{
		id: functionType
		property bool rowReady: false
		Component.onCompleted: rowReady = true
		fieldWidth: 125 * preferencesModel.uiScale
		name: "type"
		label: qsTr("Weight function")
		values: [
			{ label: qsTr("Step function"), value: "stepfun" }, { label: qsTr("Beta"), value: "beta" },
			{ label: qsTr("Half-normal"), value: "halfnorm" }, { label: qsTr("Negative exponential"), value: "negexp" },
			{ label: qsTr("Logistic"), value: "logistic" }, { label: qsTr("Power"), value: "power" },
			{ label: qsTr("Negative exponential power"), value: "negexppow" }, { label: qsTr("Truncation"), value: "trunc" },
			{ label: qsTr("Estimated truncation (experimental)"), value: "truncest" }, { label: qsTr("None"), value: "none" }
		]
		onCurrentValueChanged:
		{
			if (!rowReady || !initialized) return;
			cutoffs.value = currentValue === "stepfun" ? "(.025, .50)" : currentValue === "trunc" ? "0" : "";
			cutoffs.editingFinished();
			parameters.value = "";
			parameters.editingFinished();
		}
		info: qsTr("Choose a documented metafor selection function. Changing the function resets the cutoff and selection-parameter fields to the new function's defaults.")
	}
	DropDown
	{
		name: "sidedness"
		label: qsTr("Selection")
		visible: selectionEnabled
		values: truncation ?
			[ { label: qsTr("One-sided"), value: "oneSided" } ] :
			[ { label: qsTr("One-sided"), value: "oneSided" }, { label: qsTr("Two-sided"), value: "twoSided" } ]
		info: qsTr("Use one-sided study p-values in the expected direction, or two-sided study p-values for significance in either direction. This determines publication selection; coefficient tests remain two-sided.")
	}
	TextField
	{
		id: cutoffs
		name: "steps"
		label: functionType.value === "trunc" ? qsTr("Effect-size cutoff") : qsTr("P-value cutoffs")
		value: "(.025, .50)"
		visible: selectionEnabled && functionType.value !== "truncest"
		fieldWidth: 125 * preferencesModel.uiScale
		info: qsTr("Step functions: increasing cutoffs; 1 is appended automatically. Beta: optional lower and upper truncation thresholds. Other p-value functions: optional single threshold.")
	}
	TextField
	{
		id: parameters
		name: "delta"
		label: qsTr("Selection parameters")
		value: ""
		visible: selectionEnabled
		fieldWidth: 125 * preferencesModel.uiScale
		info: qsTr("Leave empty to estimate parameters, or enter one number/NA per parameter. For step functions, enter 1 for the first, fixed reference weight. Beta and negative exponential power have two parameters; estimated truncation uses weight and cutoff.")
	}
	DropDown
	{
		id: precision
		name: "prec"
		label: qsTr("Precision dependence")
		visible: supportsPrecision
		fieldWidth: 125 * preferencesModel.uiScale
		values: [ { label: qsTr("None"), value: "none" }, { label: qsTr("Standard error"), value: "sei" }, { label: qsTr("Variance"), value: "vi" }, { label: qsTr("Inverse sample size"), value: "ninv" }, { label: qsTr("Inverse square root sample size"), value: "sqrtninv" } ]
		info: qsTr("Allow publication probability to depend on study precision as well as p-values.")
	}
	DropDown
	{
		name: "sampleSize"
		label: qsTr("Sample size")
		source: "allVariables"
		allowedColumns: ["scale"]
		addEmptyValue: true
		visible: supportsPrecision && ["ninv", "sqrtninv"].includes(precision.value)
		fieldWidth: 125 * preferencesModel.uiScale
		info: qsTr("Select the sample-size variable used by this model's precision measure.")
	}
	CheckBox
	{
		name: "scaleprec"
		label: qsTr("Rescale precision")
		checked: true
		visible: supportsPrecision
		enabled: precision.value !== "none"
		info: qsTr("Divide the precision measure by its maximum, as in metafor's default. This changes the scale of selection parameters. Turn off to reproduce specifications using unscaled precision, including Preston et al. (2004).")
	}
	CheckBox
	{
		name: "decreasing"
		label: qsTr("Force ordinality")
		visible: functionType.value === "stepfun"
		checked: false
		info: qsTr("Constrain weights to decrease with increasing p-values. Applies only to this model.")
	}
}
