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
	title: 		qsTr("Prior and Posterior Plots")
	property string analysisType: "RoBMA"
	// RoBMA: Robust Bayesian Meta-Analsis
	// BiBMA: Binomial Bayesian Meta-Analysis
	// NoBMA: Normal Bayesian Meta-Analysis

	Group
	{

		CheckBox
		{
			label:	qsTr("Effect size")
			name:	"priorAndPosteriorPlotEffectSize"
		}

		CheckBox
		{
			label:	qsTr("Heterogeneity")
			name:	"priorAndPosteriorPlotHeterogeneity"
		}

		CheckBox
		{
			label:	qsTr("Moderation")
			name:	"priorAndPosteriorPlotModeration"
		}

		CheckBox
		{
			label:		qsTr("Weight function")
			enabled:	publicationBiasAdjustment.value != "none" && publicationBiasAdjustment.value != "PP"
			name:		"priorAndPosteriorPlotWeightFunction"
			visible:	analysisType === "RoBMA"

			CheckBox
			{
				name:		"priorAndPosteriorPlotWeightFunctionRescaleXAxis"
				text:		qsTr("Rescale x-axis")
				checked:	true
			}
		}

		CheckBox
		{
			enabled:	publicationBiasAdjustment.value != "none" && publicationBiasAdjustment.value != "original"
			label:		qsTr("PET-PEESE")
			name:		"priorAndPosteriorPlotPetPeese"
			visible:	analysisType === "RoBMA"
		}
	}

	Group
	{
		columns:	1

		RadioButtonGroup
		{
			name:		"priorAndPosteriorPlotType"
			title:		qsTr("Type")
			columns:	1

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
			label:		qsTr("Include prior distribution")
			name:		"priorAndPosteriorPlotIncludePriorDistribution"
			checked:	true
		}
	}
}
