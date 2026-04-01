// Copyright (C) 2013-2026 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

Form
{
	// -----------------------------------------------------------------------------
	// 1. Planning
	// -----------------------------------------------------------------------------
	Section
	{
		title: qsTr("1. Planning")
		columns: 1

		Group
		{
			title: qsTr("Quality Constraints")

			DoubleField {
				name: "aqlplan"
				label: qsTr("Acceptable Quality Level (AQL)")
				defaultValue: 0.05
				min: 0
				max: 1
				inclusive: JASP.MaxOnly
				decimals: 6
			}

			DoubleField {
				name: "rqlplan"
				label: qsTr("Rejectable Quality Level (RQL)")
				defaultValue: 0.15
				min: 0
				max: 1
				inclusive: JASP.MaxOnly
				decimals: 6
			}
		}

		RadioButtonGroup
		{
			id: priorPlanGroup
			title: qsTr("Prior Distribution (Beta)")
			name: "priorplan"

			RadioButton {
				value: "impartial"
				label: qsTr("Impartial (Around the RQL)")
				checked: true

				CheckBox {
					name: "impartialCustomModeplan"
					label: qsTr("Custom prior mode")
					checked: false

					DoubleField {
						name: "impartialModeplan"
						label: qsTr("Mode")
						defaultValue: 0.05
						min: 0
						max: 1
						inclusive: JASP.None
						decimals: 6
					}
				}
			}

			RadioButton {
				value: "uniform"
				label: qsTr("Uniform")
			}

			RadioButton
			{
				value: "custom"
				label: qsTr("Custom")

				DoubleField {
					name: "alphaplan"
					label: qsTr("\u03B1")
					defaultValue: 1
					min: 0
					max: 10000
				}

				DoubleField {
					name: "betaplan"
					label: qsTr("\u03B2")
					defaultValue: 1
					min: 0
					max: 10000
				}

			}
			RadioButton {
				value: "impartial_three"
				label: qsTr("Impartial (Three-region)")
			}
		}

		Group
		{
			title: qsTr("Impartial (Three-region) Options")
			enabled: priorPlanGroup.value === "impartial_three"

			DropDown
			{
				name: "impartialThreeConstraintplan"
				label: qsTr("Shape constraint")
				indexDefaultValue: 0

				values: [
					{ label: qsTr("Avoid U-shape (\u03B1 \u2265 1 or \u03B2 \u2265 1)"), value: "no_U" },
					{ label: qsTr("Both \u03B1 \u2265 1 and \u03B2 \u2265 1"),         value: "both_ge_1" },
					{ label: qsTr("None (allow U-shape)"),                             value: "none" }
				]
			}
		}

		Group
		{
			IntegerField {
				name: "max_nplan"
				label: qsTr("(Max) Sample size (n)")
				defaultValue: 40
				min: 1
				max: 1000
			}

			DoubleField {
				name: "min_bfplan"
				label: qsTr("Bayes factor (min)")
				defaultValue: 30
				min: 1/10000
				max: 10000
			}
		}

		Group
		{
			title: qsTr("Output Options")

			CheckBox { name: "showPlansplan"; label: qsTr("Plans table") }
			CheckBox { name: "priorPlotplan"; label: qsTr("Prior plot") }
			CheckBox { name: "showThreeBFplan"; label: qsTr("Three-hypothesis BF table") }
		}
	}

	// -----------------------------------------------------------------------------
	// 2. Inference
	// -----------------------------------------------------------------------------
	Section
	{
		title: qsTr("2. Inference")
		columns: 1
		id: infer

		Group
		{
			CheckBox
			{
				id: inferPosterior
				name: "inferPosteriorinfer"
				label: qsTr("Use data to create posterior distribution")
			}

			Group
			{
				enabled: inferPosterior.checked

				IntegerField {
					id: data_n
					name: "data_ninfer"
					label: qsTr("Sample size (n)")
					defaultValue: 30
					min: 1
					max: 10000
				}

				IntegerField {
					name: "data_dinfer"
					label: qsTr("Observed number of defects (d)")
					defaultValue: 0
					min: 0
					max: parseInt(data_n.value)
				}

				IntegerField {
					id: lot_size
					name: "lot_sizeinfer"
					label: qsTr("Lot size (N)")
					defaultValue: 100
					min: parseInt(data_n.value)
					max: 100000
				}
			}

			Group
			{
				title: qsTr("Output Options")
				enabled: inferPosterior.checked

				CheckBox { name: "showInferenceTableinfer"; label: qsTr("Inference decision table") }
				CheckBox { name: "posteriorPlotinfer"; label: qsTr("Posterior plot") }
				CheckBox { name: "ppdPlotinfer"; label: qsTr("Posterior predictive distribution plot") }
			}
		}
	}
}
