import QtQuick
import JASP.Controls

DropDown
{
	property string analysisType: "metaAnalysis"
	property int heterogeneityModelTermsCount: 0
	readonly property bool selectionModels: analysisType === "selectionModels"

	name:			"method"
	label:			qsTr("Method")
	startValue:		selectionModels ? "maximumLikelihood" : "restrictedML"
	info: selectionModels ? qsTr("Selection models use maximum likelihood and z/Wald coefficient tests. The same method and moderators apply to all candidates.") : qsTr("Method used for model estimation in the meta-analysis. The available methods depend on the inclusion of heterogeneity model terms.")
	readonly property var methodChoices: [
		{ label: qsTr("Equal Effects")			, value: "equalEffects"		},
		{ label: qsTr("Fixed Effects")			, value: "fixedEffects"		},
		{ label: qsTr("Maximum Likelihood")		, value: "maximumLikelihood"},
		{ label: qsTr("Restricted ML")			, value: "restrictedML"		},
		{ label: qsTr("DerSimonian-Laird")		, value: "derSimonianLaird"	},
		{ label: qsTr("Hedges")					, value: "hedges"			},
		{ label: qsTr("Hunter-Schmidt")			, value: "hunterSchmidt"	},
		{ label: qsTr("Hunter-Schmidt (SSC)")	, value: "hunterSchmidtSsc"	},
		{ label: qsTr("Sidik-Jonkman")			, value: "sidikJonkman"		},
		{ label: qsTr("Empirical Bayes")		, value: "empiricalBayes"	},
		{ label: qsTr("Paule-Mandel")			, value: "pauleMandel"		},
		{ label: qsTr("Paule-Mandel (MU)")		, value: "pauleMandelMu"	},
		{ label: qsTr("Generalized Q-stat")		, value: "qeneralizedQStat"	},
		{ label: qsTr("Generalized Q-stat (MU)"), value: "qeneralizedQStatMu"},
		{ label: qsTr("Unrestricted Weighted Least Squares (UWLS)"), value: "unrestrictedWeightedLeastSquares" },
	]

	values: methodChoices.filter(function(choice) {
		if (selectionModels)
			return ["equalEffects", "fixedEffects", "maximumLikelihood"].includes(choice.value);
		if (analysisType === "multilevelMultivariateMetaAnalysis")
			return ["maximumLikelihood", "restrictedML"].includes(choice.value);
		if (heterogeneityModelTermsCount > 0)
			return ["maximumLikelihood", "restrictedML", "empiricalBayes"].includes(choice.value);
		return true;
	})
}
