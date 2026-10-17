import QtQuick
import JASP.Controls
import JASP

Group
{
	property string analysisType: "metaAnalysis"
	property string methodValue: ""
	property bool heterogeneityModel: false
	property string heterogeneityModelLinkValue: "log"
	readonly property bool selectionModels:			analysisType === "selectionModels"
	readonly property bool metaAnalysis:			analysisType === "metaAnalysis"
	readonly property bool multilevelMultivariate:	analysisType === "multilevelMultivariateMetaAnalysis"
	readonly property bool likelihoodEstimation: ["restrictedML", "maximumLikelihood", "empiricalBayes"].includes(methodValue)
	readonly property bool qStatisticEstimation: ["pauleMandel", "pauleMandelMu", "qeneralizedQStatMu"].includes(methodValue)
	readonly property bool trustRegionOptimizer: ["uobyqa", "newuoa", "bobyqa"].includes(optimizerMethod.value)

	title:		qsTr("Optimizer")
	enabled:	selectionModels || likelihoodEstimation || qStatisticEstimation || methodValue === "sidikJonkman"
	info: selectionModels ? qsTr("Optimizer settings for fitting the selection models.") : qsTr("Optimizer settings for estimating the meta-analytic models. A more complex/unavailbe settings can be specified via the 'Extend metafor call' option.")

	DropDown
	{
		name:		"optimizerMethod"
		id:			optimizerMethod
		label:		qsTr("Method") // TODO: switch default value on heterogeneityModelLink change
		info: selectionModels ? qsTr("Select the optimizer used by metafor. The default is BFGS, including for ordinal step functions.") : qsTr("Select the optimization method to use for fitting the model. Available in multilevel/multivariate meta-analysis or when heterogeneity model terms are included.")
		values:		{
			if (selectionModels)
				return [ { label: qsTr("Default"), value: "default" }, "BFGS", "Nelder-Mead", "nlminb" ];

			var choices = ["nlminb", "BFGS", "Nelder-Mead", "uobyqa", "newuoa", "bobyqa", "nloptr", "nlm"];
			if (metaAnalysis && heterogeneityModelLinkValue !== "log")
				choices.unshift("constrOptim");
			if (multilevelMultivariate)
				choices = choices.concat(["hjk", "nmk", "mads"]);
			return choices;
		}
		visible:	selectionModels || multilevelMultivariate || heterogeneityModel
	}

	CheckBox
	{
		name:		"optimizerInitialTau2"
		text:		qsTr("Initial 𝜏²")
		checked:	false
		childrenOnSameRow:	true
		info: qsTr("Specify the initial value of 𝜏² for the optimization algorithm. Available only for specific optimization methods and unavailable in multilevel/multivariate meta-analysis or when heterogeneity model terms are included.")
		visible:	(likelihoodEstimation ||
					methodValue === "sidikJonkman") && !heterogeneityModel && metaAnalysis

		DoubleField
		{
			label: 				""
			name:				"optimizerInitialTau2Value"
			defaultValue:		1
			min: 				0
			inclusive: 			JASP.None
		}
	}

	CheckBox
	{
		name:		"optimizerMinimumTau2"
		text:		qsTr("Minimum 𝜏²")
		checked:	false
		childrenOnSameRow:	true
		info: qsTr("Specify the minimum allowable value of 𝜏² during optimization. Available only for specific optimization methods and unavailable in multilevel/multivariate meta-analysis or when heterogeneity model terms are included.")
		visible:	qStatisticEstimation &&
					!heterogeneityModel && metaAnalysis

		DoubleField
		{
			label: 				""
			name: 				"optimizerMinimumTau2Value"
			id:					optimizerMinimumTau2Value
			defaultValue:		1e-6
			min: 				0
			max: 				optimizerMaximumTau2Value.value
		}
	}

	CheckBox
	{
		name:		"optimizerMaximumTau2"
		text:		qsTr("Maximum 𝜏²")
		checked:	false
		childrenOnSameRow:	true
		info: qsTr("Specify the maximum allowable value of 𝜏² during optimization. Available only for specific optimization methods and unavailable in multilevel/multivariate meta-analysis or when heterogeneity model terms are included.")
		visible:	(qStatisticEstimation &&
					!heterogeneityModel && metaAnalysis)

		DoubleField
		{
			label: 				""
			name: 				"optimizerMaximumTau2Value"
			id:					optimizerMaximumTau2Value
			defaultValue:		100
			min: 				optimizerMinimumTau2Value.value
			inclusive: 			JASP.None
		}
	}

	CheckBox
	{
		name:		"optimizerMaximumEvaluations"
		text:		qsTr("Maximum evaluations")
		checked:	false
		childrenOnSameRow:	true
		info: qsTr("Set the maximum number of function evaluations for the optimizer. Available when using specific optimization methods in multilevel/multivariate meta-analysis.")
		visible:	multilevelMultivariate && ["nlminb", "uobyqa", "newuoa", "bobyqa", "hjk", "nmk", "mads"].includes(optimizerMethod.value)

		IntegerField
		{
			label: 				""
			name: 				"optimizerMaximumEvaluationsValue"
			value:				250
			min: 				1
			inclusive: 			JASP.None
		}
	}

	CheckBox
	{
		name:		"optimizerMaximumIterations"
		text:		qsTr("Maximum iterations")
		checked:	false
		childrenOnSameRow:	true
		info: qsTr("Set the maximum number of iterations for the optimizer. Available when using certain estimation or optimization methods.")
		visible:	selectionModels || (metaAnalysis && (likelihoodEstimation || qStatisticEstimation)) ||
					(multilevelMultivariate && ["nlminb", "Nelder-Mead", "BFGS", "nloptr", "nlm"].includes(optimizerMethod.value))

		IntegerField
		{
			label: 				""
			name: 				"optimizerMaximumIterationsValue"
			value:				selectionModels || heterogeneityModel ? 1000 : 150
			min: 				1
			inclusive: 			JASP.None
		}
	}

	CheckBox
	{
		name:		"optimizerConvergenceTolerance"
		text:		qsTr("Convergence tolerance")
		checked:	false
		childrenOnSameRow:	true
		info: qsTr("Set the convergence tolerance for the optimizer. Available when using certain methods without heterogeneity model terms or specific optimizers in multilevel/multivariate meta-analysis.")
		visible:	(metaAnalysis && !heterogeneityModel && (likelihoodEstimation || qStatisticEstimation)) ||
					(multilevelMultivariate && ["hjk", "nmk", "mads"].includes(optimizerMethod.value))

		DoubleField
		{
			label: 				""
			name: 				"optimizerConvergenceToleranceValue"
			defaultValue:		likelihoodEstimation ? 1e-5 : qStatisticEstimation ? 1e-4 : 1
			min: 				0
			inclusive: 			JASP.None
			decimals:			5
		}
	}

	CheckBox
	{
		name:		"optimizerConvergenceRelativeTolerance"
		text:		qsTr("Convergence relative tolerance")
		checked:	false
		childrenOnSameRow:	true
		info: selectionModels ? qsTr("Override the optimizer's relative convergence tolerance.") : qsTr("Set the relative convergence tolerance for the optimizer. Available when heterogeneity model terms are included or using specific optimizers in multilevel/multivariate meta-analysis.")
		visible:	selectionModels || (metaAnalysis && heterogeneityModel) ||
					(multilevelMultivariate && ["nlminb", "Nelder-Mead", "BFGS"].includes(optimizerMethod.value))

		DoubleField
		{
			label: 				""
			name: 				"optimizerConvergenceRelativeToleranceValue"
			defaultValue:		1e-8
			decimals:			8
			min: 				0
			inclusive: 			JASP.None
		}
	}

	CheckBox
	{
		name:		"optimizerStepAdjustment"
		text:		qsTr("Step adjustment")
		checked:	false
		childrenOnSameRow:	true
		info: qsTr("Set the step adjustment factor for the optimizer. Available when using certain methods without heterogeneity model terms and unavailable in multilevel/multivariate meta-analysis.")
		visible:	(likelihoodEstimation &&
					!heterogeneityModel && metaAnalysis)


		DoubleField
		{
			label: 				""
			name: 				"optimizerStepAdjustmentValue"
			defaultValue:		1
			min: 				0
			inclusive: 			JASP.None
		}
	}

	CheckBox
	{
		name:		"optimizerInitialTrustRegionRadius"
		text:		qsTr("Initial trust region radius")
		checked:	false
		childrenOnSameRow:	true
		info: qsTr("Set the initial trust region radius for the optimizer. Available when using specific optimization methods in multilevel/multivariate meta-analysis.")
		visible:	trustRegionOptimizer && multilevelMultivariate

		DoubleField
		{
			label: 				""
			name: 				"optimizerInitialTrustRegionRadiusValue"
			defaultValue:		1
			min: 				0
			inclusive: 			JASP.None
		}
	}

	CheckBox
	{
		name:		"optimizerFinalTrustRegionRadius"
		text:		qsTr("Final trust region radius")
		checked:	false
		childrenOnSameRow:	true
		info: qsTr("Set the final trust region radius for the optimizer. Available when using specific optimization methods in multilevel/multivariate meta-analysis.")
		visible:	trustRegionOptimizer && multilevelMultivariate

		DoubleField
		{
			label: 				""
			name: 				"optimizerFinalTrustRegionRadiusValue"
			defaultValue:		1
			min: 				0
			inclusive: 			JASP.None
		}
	}

	CheckBox
	{
		name:		"optimizerMaximumRestarts"
		text:		qsTr("Maximum restarts")
		checked:	false
		childrenOnSameRow:	true
		info: qsTr("Set the maximum number of restarts for the optimizer. Available when using the Nelder-Mead method ('nmk') in multilevel/multivariate meta-analysis.")
		visible:	optimizerMethod.value === "mmk" && multilevelMultivariate

		IntegerField
		{
			label: 				""
			name: 				"optimizerMaximumRestartsValue"
			defaultValue:		3
			min: 				1
			inclusive: 			JASP.None
		}
	}
}
