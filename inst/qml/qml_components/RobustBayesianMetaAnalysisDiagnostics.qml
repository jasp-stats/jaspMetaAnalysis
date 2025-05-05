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

	title: qsTr("MCMC Diagnostics")

	CheckBox
	{
		Layout.columnSpan: 2
		label:		qsTr("Overview table")
		name:		"mcmcDiagnosticsOverviewTable"
	}

	Group
	{
		title:			qsTr("Plot")
		CheckBox
		{
			label:		qsTr("Effect size")
			name:		"mcmcDiagnosticsPlotEffectSize"
		}

		CheckBox
		{
			label:		qsTr("Heterogeneity")
			name:		"mcmcDiagnosticsPlotHeterogeneity"
		}

		CheckBox
		{
			label:		qsTr("Moderation")
			name:		"mcmcDiagnosticsPlotModeration"
			enabled:	predictors.count > 0
		}

		CheckBox
		{
			label:		qsTr("Weights")
			name:		"mcmcDiagnosticsPlotWeights"
			visible:	analysisType === "RoBMA"
			enabled:	publicationBiasAdjustment.value != "none" && publicationBiasAdjustment.value != "PP"
		}

		CheckBox
		{
			label:		qsTr("PET")
			name:		"mcmcDiagnosticsPlotPet"
			visible:	analysisType === "RoBMA"
			enabled:	publicationBiasAdjustment.value != "none" && publicationBiasAdjustment.value != "original"
		}

		CheckBox
		{
			label:		qsTr("PEESE")
			name:		"mcmcDiagnosticsPlotPeese"
			visible:	analysisType === "RoBMA"
			enabled:	publicationBiasAdjustment.value != "none" && publicationBiasAdjustment.value != "original"
		}
	}

	Group
	{
		Group
		{
			title:			qsTr("Type")
			CheckBox
			{
				label:		qsTr("Trace")
				name:		"mcmcDiagnosticsPlotTypeTrace"
			}

			CheckBox
			{
				label:		qsTr("Autocorrelation")
				name:		"mcmcDiagnosticsPlotTypeAutocorrelation"
			}

			CheckBox
			{
				label:		qsTr("Posterior samples density")
				name:		"mcmcDiagnosticsPlotTypePosteriorSamplesDensity"
			}
		}
	}

}
