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
import "../qml/qml_components" as MA

Form
{
	info: qsTr("Classical multilevel/multivariate meta-analysis allows you to conduct a meta-analysis adjusting for the dependency between effect sizes using the classical approach. " + 
	"The effect size dependency can be adjusted for by specifying the 'Effect Size Variance-Covariance Matrix' (interfacing the 'vcalc' function) and specifying the 'Random Effects/Model Components' (interfacing the 'random' argument in 'rma.mv' function). " +
	"It provides options for fixed and random effects models, as well as meta-regression, and subgroup analysis. " +
	"Additional options include the ability to specify clustering for robust variance estimation, permutation tests, and generating the metafor package R code. " +
	"The results include estimates of effect sizes, heterogeneity, moderation, and various plots to visualize the results.\n\n" + 
	"See [this tutorial](https://doi.org/10.48550/arXiv.2509.09845) for a detailed introduction to the module.")
	infoBottom: "## " + qsTr("References") + "\n" +
	"- Bartoš F, Wagenmakers EJ, & Viechtbauer W (2025). “Meta-analysis with JASP, Part I: Classical approaches.” _ArXiv Preprint_. https://doi.org/10.48550/arXiv.2509.09845\n" + 
	"- Viechtbauer W (2010). “Conducting meta-analyses in R with the metafor package.” _Journal of Statistical Software, 36_(3), 1–48. https://doi.org/10.18637/jss.v036.i03\n" +
	"- Viechtbauer W, López-López JA, Sánchez-Meca J, Marín-Martínez F (2015). “A comparison of procedures to test for moderators in mixed-effects meta-regression models.” _Psychological Methods, 20_(3), 360–374. https://doi.org/10.1037/met0000023\n" +
	"- Viechtbauer W (2025). _metafor: Meta-Analysis Package for R_. R package version 4.8-0 Available at: <https://CRAN.R-project.org/package=metafor>.\n" +
	"## " + qsTr("R Packages") + "\n" +
	"- metafor"

	VariablesForm
	{
		preferredHeight: 525 * preferencesModel.uiScale

		AvailableVariablesList
		{
			name:				"allVariables"
		}

		AssignedVariablesList
		{
			name:				"effectSize"
			id:					effectSize
			title:				qsTr("Effect Size")
			singleVariable:		true
			allowedColumns:		["scale"]
			info: qsTr("Variable containing the observed effect sizes.")
		}

		AssignedVariablesList
		{
			name:				"effectSizeStandardError"
			id:					effectSizeStandardError
			title:				qsTr("Effect Size Standard Error")
			singleVariable:		true
			allowedColumns:		["scale"]
			info: qsTr("Variable containing the standard errors corresponding to the effect sizes.")
		}

		MA.ClassicalMetaAnalysisMethod
		{
			id: method
			analysisType: "multilevelMultivariateMetaAnalysis"
			info: qsTr("Method used to estimate heterogeneity in the meta-analysis.")
		}

		DropDown
		{
			name:				"fixedEffectTest"
			label:				qsTr("Fixed effect test")
			startValue:			"t"
			values:				clustering.count > 0 ? [ "t" ] : [ "z", "t"]
			info: qsTr("Method for testing the model coefficients: 'z' uses standard normal approximation, 't' uses t-distribution. Clustered models use t-distribution based tests.")
		}

		AssignedVariablesList
		{
			name:				"predictors"
			id:					predictors
			title:				qsTr("Predictors")
			allowedColumns:		["nominal", "scale"]
			allowTypeChange:	true
			info: qsTr("Variables to include as predictors (moderators) in the meta-regression model.")
		}

		AssignedVariablesList
		{
			name:				"clustering"
			id:					clustering
			title:				qsTr("Clustering")
			singleVariable:		true
			allowedColumns:		["nominal"]
			info: qsTr("Variable indicating clustering of effect sizes. This option is disabled when permutation tests are selected.")
		}

		AssignedVariablesList
		{
			name:				"studyLabels"
			title:				qsTr("Study Labels")
			singleVariable:		true
			allowedColumns:		["nominal"]
			info: qsTr("Variable containing labels for the studies. Used for labeling outputs and plots.")
		}
		
		AssignedVariablesList
		{
			name:				"subgroup"
			id:					subgroup
			title:				qsTr("Subgroup")
			singleVariable:		true
			allowedColumns:		["nominal"]
			info: qsTr("Variable indicating subgroup stratification. For each subgroup, an independent model is fitted to the corresponding data set subset.")
		}
	}


	MA.ClassicalMetaAnalysisMultivariate
	{
		analysisType:	"multilevelMultivariateMetaAnalysis"
	}

	MA.ClassicalMetaAnalysisRandomEffects {}

	MA.ClassicalMetaAnalysisModel
	{
		id:				sectionModel
		analysisType:	"multilevelMultivariateMetaAnalysis"
	}

	MA.ClassicalMetaAnalysisStatistics 
	{
		id:				sectionStatistics
		analysisType:	"multilevelMultivariateMetaAnalysis"
	}

	MA.ClassicalMetaAnalysisEstimatedMarginalMeans
	{
		analysisType:	"multilevelMultivariateMetaAnalysis"
	}

	MA.ForestPlotSection
	{
		analysisType:				"multilevelMultivariateMetaAnalysis"
		transformEffectSizeValue:	sectionStatistics.transformEffectSizeValue
		effectSizeReady:			effectSize.count == 1 && effectSizeStandardError.count == 1
		modelInformationEnabled:	effectSize.count == 1 && effectSizeStandardError.count == 1
		effectSizeModelTermsCount:	sectionModel.effectSizeModelTermsCount
		heterogeneityModelTermsCount: sectionModel.heterogeneityModelTermsCount
		methodValue:				method.value
		subgroupSelected:			subgroup.count > 0
	}

	MA.BubblePlot {}

	MA.ClassicalMetaAnalysisDiagnostics
	{
		analysisType:	"multilevelMultivariateMetaAnalysis"
	}

	MA.ClassicalMetaAnalysisExport
	{
		analysisType:	"multilevelMultivariateMetaAnalysis"
	}

	MA.ClassicalMetaAnalysisAdvanced
	{
		analysisType:	"multilevelMultivariateMetaAnalysis"
	}
}
