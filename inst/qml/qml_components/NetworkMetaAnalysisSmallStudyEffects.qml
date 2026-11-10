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

Section
{
	title:		qsTr("Small-Study Effects")
	columns:	2
	enabled:	networkModel.value === "standard"
	info: qsTr("Options for assessing small-study effects (e.g., publication bias) in the network. Available for the standard model.")

	CheckBox
	{
		name:		"funnelPlot"
		id:			funnelPlot
		text:		qsTr("Comparison-adjusted funnel plot")
		checked:	false
		info: qsTr("Include a comparison-adjusted funnel plot. The treatment effects of the individual comparisons are centered at the pooled direct estimate of the comparison and plotted against their standard errors; asymmetry around zero suggests small-study effects.")

		CheckBox
		{
			name:		"funnelPlotPseudoConfidenceIntervals"
			text:		qsTr("Pseudo confidence intervals")
			checked:	true
			info: qsTr("Include the 90%, 95%, and 99% pseudo confidence regions around zero in the funnel plot (shaded as in the Funnel Plot analysis).")
		}
	}

	Group
	{
		title:		qsTr("Asymmetry Tests")
		info: qsTr("Tests of funnel plot asymmetry applied to the comparison-adjusted effects (the study effects minus the pooled direct effect of their comparison). The tests require at least 10 comparisons (study-level treatment effects).")

		CheckBox
		{
			name:		"funnelPlotEggerTest"
			id:			funnelPlotEggerTest
			text:		qsTr("Egger")
			checked:	false
			info: qsTr("Include the regression test of funnel plot asymmetry (Egger).")
		}

		CheckBox
		{
			name:		"funnelPlotBeggTest"
			id:			funnelPlotBeggTest
			text:		qsTr("Begg and Mazumdar")
			checked:	false
			info: qsTr("Include the rank correlation test of funnel plot asymmetry (Begg and Mazumdar).")
		}

		CheckBox
		{
			name:		"funnelPlotThompsonSharpTest"
			id:			funnelPlotThompsonSharpTest
			text:		qsTr("Thompson and Sharp")
			checked:	false
			info: qsTr("Include the regression test of funnel plot asymmetry (Thompson and Sharp).")
		}
	}

	Group
	{
		// the funnel plot and the asymmetry tests use the same order of the treatments
		id:			treatmentOrderGroup
		title:		qsTr("Treatment order")
		enabled:	funnelPlot.checked || funnelPlotEggerTest.checked || funnelPlotBeggTest.checked || funnelPlotThompsonSharpTest.checked
		info: qsTr("Order of the treatments used by the comparison-adjusted funnel plot and the asymmetry tests: each comparison is oriented by the treatment that comes first. Available when the funnel plot or an asymmetry test is included.")

		RadioButtonGroup
		{
			name:		"funnelPlotTreatmentOrder"
			id:			funnelPlotTreatmentOrder
			info: qsTr("Order of the treatments determining the direction of each comparison in the funnel plot and the asymmetry tests.")

			RadioButton
			{
				value:		"referenceFirst"
				label:		qsTr("Reference treatment first")
				checked:	true
				info: qsTr("The reference treatment comes first, followed by the other treatments in the network order.")
			}

			RadioButton
			{
				value:		"custom"
				label:		qsTr("Custom")
				info: qsTr("Specify the order of the treatments by moving them to 'Treatment Order' (shown below the options of the section).")
			}
		}
	}

	VariablesForm
	{
		// the custom order of the treatments for the funnel plot and the asymmetry tests (the treatments are those of the reference treatment list)
		Layout.columnSpan:	2
		visible:			treatmentOrderGroup.enabled && funnelPlotTreatmentOrder.value === "custom"
		preferredHeight:	jaspTheme.smallDefaultVariablesFormHeight

		AvailableVariablesList
		{
			name:	"funnelPlotTreatments"
			title:	qsTr("Treatments")
			source:	[{ name: "referenceTreatment" }]
		}

		AssignedVariablesList
		{
			name:	"funnelPlotTreatmentOrderCustom"
			title:	qsTr("Treatment Order")
			info:	qsTr("Order of the treatments for the funnel plot and the asymmetry tests: each comparison is oriented by the treatment listed first (e.g., from the oldest to the newest treatment, so that all comparisons are 'older versus newer'). Treatments that are not listed follow in the order of the network.")
		}
	}
}
