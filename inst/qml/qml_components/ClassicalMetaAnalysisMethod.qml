import QtQuick
import JASP.Controls

DropDown
{
	property string analysisType: "metaAnalysis"
	property int heterogeneityModelTermsCount: 0
	readonly property bool selectionModels: analysisType === "selectionModels"
	readonly property bool petPeese: analysisType === "petPeese"
	readonly property bool waapWls: analysisType === "waapWls"
	readonly property bool networkMetaAnalysis: analysisType === "networkMetaAnalysis"
	property bool multilevelMultivariate: analysisType === "multilevelMultivariateMetaAnalysis"
	// network meta-analysis pooling methods: Mantel-Haenszel/non-central hypergeometric (equal effects only) and penalized logistic regression (dispersion random effects)
	property bool commonEffectOnly: false
	property bool dispersionModel: false
	// network meta-analysis component model: a moment estimator of the between-study variance only (DerSimonian-Laird first: it replaces
	// another estimator when the component model is selected)
	property bool componentModel: false

	name:			"method"
	label:			qsTr("Method")
	// (the start value is also the value that replaces a method that is not offered after the choices change: the network meta-analysis keeps a random-effects model
	// when penalized logistic regression, which offers Equal Effects and Random Effects (dispersion) only, is selected)
	startValue:		selectionModels ? "maximumLikelihood" : (petPeese || waapWls) ? "unrestrictedWeightedLeastSquares" : networkMetaAnalysis ? (dispersionModel ? "randomEffects" : "derSimonianLaird") : "restrictedML"
	info: selectionModels ? qsTr("Selection models use maximum likelihood and z/Wald coefficient tests. The same method and moderators apply to all candidates.") : petPeese ? qsTr("Method used for estimating the PET and PEESE meta-regressions. Unrestricted weighted least squares (default) corresponds to the original PET-PEESE specification with multiplicative heterogeneity (Stanley & Doucouliagos, 2014); the remaining methods estimate additive heterogeneity. The multilevel/multivariate model is estimated with (restricted) maximum likelihood.") : waapWls ? qsTr("Method used for estimating the WLS and WAAP meta-analyses. Unrestricted weighted least squares (default) corresponds to the original WAAP-WLS specification with multiplicative heterogeneity (Stanley & Doucouliagos, 2017); the remaining methods estimate additive heterogeneity. The multilevel/multivariate model is estimated with (restricted) maximum likelihood.") : networkMetaAnalysis ? qsTr("Select the model for the between-study heterogeneity. 'Equal Effects' fits the common-effect network meta-analysis; the other methods fit the random-effects network meta-analysis with a common between-study variance estimated by the selected estimator. The restricted ML and maximum likelihood estimates of the between-study variance can depend on the choice of the reference treatment for contrast-based data with multi-arm studies. The component model estimates the between-study variance by the method of moments (DerSimonian-Laird). For penalized logistic regression, 'Random Effects (dispersion)' models the heterogeneity by a dispersion factor (no estimator is used), and for the Mantel-Haenszel and non-central hypergeometric methods only 'Equal Effects' is available.") : qsTr("Method used for model estimation in the meta-analysis. The available methods depend on the inclusion of heterogeneity model terms.")
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
		{ label: qsTr("Random Effects (dispersion)"), value: "randomEffects" },
	]

	// PET-PEESE and WAAP-WLS list restricted ML first: the first value replaces UWLS when the multilevel/multivariate model is selected
	readonly property var multilevelMultivariateChoices: (petPeese || waapWls) ? ["restrictedML", "maximumLikelihood"] : ["maximumLikelihood", "restrictedML"]

	values: (multilevelMultivariate || (networkMetaAnalysis && componentModel && !commonEffectOnly && !dispersionModel)) ? (multilevelMultivariate ? multilevelMultivariateChoices : ["derSimonianLaird", "equalEffects"]).map(function(value) {
		return methodChoices.find(function(choice) { return choice.value === value; });
	}) : methodChoices.filter(function(choice) {
		if (networkMetaAnalysis)
			return (commonEffectOnly ? ["equalEffects"] : dispersionModel ? ["equalEffects", "randomEffects"] : ["equalEffects", "maximumLikelihood", "restrictedML", "derSimonianLaird"]).includes(choice.value);
		if (choice.value === "randomEffects")
			return false;
		if (selectionModels)
			return ["equalEffects", "fixedEffects", "maximumLikelihood"].includes(choice.value);
		if (heterogeneityModelTermsCount > 0)
			return ["maximumLikelihood", "restrictedML", "empiricalBayes"].includes(choice.value);
		return true;
	})
}
