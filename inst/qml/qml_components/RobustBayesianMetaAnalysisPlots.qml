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

Section
{
	property string analysisType: "RoBMA"
	// RoBMA: Robust Bayesian Meta-Analsis
	// BiBMA: Binomial Bayesian Meta-Analysis
	// NoBMA: Normal Bayesian Meta-Analysis

	title: 		qsTr("Plots")


	CheckBox
	{
		columns:	2
		label:		qsTr("Forest plot")
		name:		"plotsForestPlot"

		RadioButtonGroup
		{
			name: 		"plotsForestPlotOrder"
			title:		qsTr("Order")

			RadioButton
			{
				name: 	"increasing"
				label: 	qsTr("Ascending")
			}

			RadioButton
			{
				name: 	"decreasing"
				label: 	qsTr("Descending")
			}

			RadioButton
			{
				name: 	"alphabetical"
				label: 	qsTr("Alphabetical")
				checked:true
			}
		}
	}

	Group
	{
		title:		" " // Add a line to align with the first column
		columns:	1

		RadioButtonGroup
		{
			name:				"plotsForestPlotType"
			title:				qsTr("Type")
			columns:			2

			RadioButton
			{
				value:		"averaged"
				label:		qsTr("Model averaged")
				checked:	true
			}

			RadioButton
			{
				value:		"conditional"
				label:		qsTr("Conditional")
			}

		}
	}

	Divider { }

	Group
	{
		title:	qsTr("Pooled estimates")
		columns: 1

		CheckBox
		{
			label:	qsTr("Effect")
			name:	"plotsPooledEstimatesEffect"
		}

		CheckBox
		{
			label:	qsTr("Heterogeneity")
			name:	"plotsPooledEstimatesHeterogeneity"
		}

		CheckBox
		{
			label:		qsTr("Weight function")
			name:		"plotsPooledEstimatesWeightFunction"
			visible:	analysisType === "RoBMA"

			CheckBox
			{
				name:		"plotsPooledEstimatesWeightFunctionRescaleXAxis"
				text:		qsTr("Rescale x-axis")
				checked:	true
			}
		}

		CheckBox
		{
			label:		qsTr("PET-PEESE")
			name:		"plotsPooledEstimatesPetPeese"
			visible:	analysisType === "RoBMA"
		}
	}

	Group
	{
		title:		" " // Add a line to align with the first column
		columns:	1

		RadioButtonGroup
		{
			name:		"plotsPooledEstimatesType"
			title:		qsTr("Type")
			columns:	2

			RadioButton
			{
				value:		"averaged"
				label:		qsTr("Model averaged")
				checked:	true
			}

			RadioButton
			{
				value:		"conditional"
				label:		qsTr("Conditional")
			}

		}

		CheckBox
		{
			label:		qsTr("Prior distribution")
			name:		"plotsPooledEstimatesPriorDistribution"
			checked:	true
		}
	}

	Divider { }

	Group
	{
		title:	qsTr("Individual models")
		columns: 1

		CheckBox
		{
			label:	qsTr("Effect")
			name:	"plotsIndividualModelsEffect"
		}

		CheckBox
		{
			label:	qsTr("Heterogeneity")
			name:	"plotsIndividualModelsHeterogeneity"
		}
	}

	Group
	{
		title:		" "
		columns:	2

		RadioButtonGroup
		{
			name:				"plotsIndividualModelsType"
			title:				qsTr("Type")
			Layout.columnSpan:	2
			columns:			2

			RadioButton
			{
				value:		"averaged"
				label:		qsTr("Model averaged")
			}

			RadioButton
			{
				value:		"conditional"
				label:		qsTr("Conditional")
				checked:	true
			}
		}

		RadioButtonGroup
		{
			name: 		"plotsIndividualModelsOrder"
			title:		qsTr("Order")

			RadioButton
			{
				name: 	"increasing"
				label: 	qsTr("Ascending")
			}

			RadioButton
			{
				name: 	"decreasing"
				label: 	qsTr("Descending")
			}
		}

		RadioButtonGroup
		{
			name: 		"plotsIndividualModelsOrderBy"
			title:		qsTr("Order by")

			RadioButton
			{
				name:		"modelNumber"
				label:		qsTr("Model number")
				checked:	true
			}

			RadioButton
			{
				name:		"estimate"
				label:		qsTr("Estimate")
			}

			RadioButton
			{
				name:		"bayesFactor"
				label:		qsTr("Bayes factor")
			}

			RadioButton
			{
				name:		"posteriorProbability"
				label:		qsTr("Posterior prob.")
			}
		}

		Group
		{
			title:				qsTr("Show")
			Layout.columnSpan:	2

			CheckBox
			{
				label:		qsTr("Bayesian updating")
				name:		"plotsIndividualModelsShowBayesianUpdating"
				checked:	true
			}

			CheckBox
			{
				label:		qsTr("Posterior estimates")
				name:		"plotsIndividualModelsShowPosteriorEstimates"
				checked:	false
			}
		}
	}
}
