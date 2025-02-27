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
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program. If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick
import QtQuick.Layouts
import JASP.Controls
import "./common" as Common
import JASP

Form
{
	columns: 1
	VariablesForm
	{
		AvailableVariablesList	{ name: "allVariablesList"; id: allVariablesList }
		AssignedVariablesList	{ name: "variables"; title: qsTr("Measurement"); id: variables; singleVariable: true; allowedColumns: ["scale"] }
	}

	Group
	{
		enabled: variables.count != 1
		Text { text: qsTr("Specify sample statistics directly (used if dataset is not available)") }
		CheckBox { name: "sampleStats"; id: sampleStats; checked: (variables.count != 1); enabled: false; visible: false }
		IntegerField { name: "sampleSize"; label: qsTr("Sample size (n)"); defaultValue: 24; min: 1 }
		DoubleField { name: "sampleMean"; label: qsTr("Sample mean"); defaultValue: 1.5; decimals: 6 }
		DoubleField { name: "sampleSD"; label: qsTr("Sample standard deviation"); defaultValue: 1; min: 0; inclusive: JASP.None; decimals: 6 }
	}

	// Todo: Label for k is a hacky solution to align it with the sample values. Adjust the width in code.
	DoubleField { name: "kValue"; label: qsTr("k                        "); defaultValue: 1.309; min: 0; negativeValues: false; inclusive: JASP.None; decimals: 6 }
	
	Group
	{
		title: qsTr("Specification limits")
		columns: 2
		CheckBox { name: "lsl"; label: qsTr("Lower Specification Limit (LSL)"); id: lsl; checked: false }
		DoubleField{ name: "lower_spec"; label: ""; id: lower_spec; defaultValue: 0; enabled: lsl.checked; negativeValues: true; max: upper_spec.value; inclusive: JASP.MaxOnly; decimals: 6 }
		CheckBox { name: "usl"; label: qsTr("Upper Specification Limit (USL)"); id: usl; checked: false }
		DoubleField { name: "upper_spec"; label: ""; id: upper_spec; defaultValue: 1; enabled: usl.checked; negativeValues: true; min: lower_spec.value; inclusive: JASP.MinOnly; decimals: 6 }
	}

	Group
	{
		columns: 2
		CheckBox { name: "sd"; label: qsTr("Standard deviation (historical)"); id: sd; checked: false }
		DoubleField { name: "stdev"; label: ""; enabled: sd.checked; defaultValue: 1; min: 0; negativeValues: false; inclusive: JASP.None; decimals: 6 }
	}

	Group
	{
		title: qsTr("Quality constraints")
		enabled: lsl.checked && usl.checked && sd.checked
		columns: 2
		Text { text: qsTr("Acceptable Quality Level (AQL)") }
		DoubleField{ name: "aql"; label: ""; negativeValues: false; defaultValue: 0.05; min: 0; max: 1; inclusive: JASP.None; decimals: 6 }
		Text { text: qsTr("Rejectable Quality Level (RQL / LTPD)") }
		DoubleField { name: "rql"; label: ""; negativeValues: false; defaultValue: 0.15; min: 0; max: 1; inclusive: JASP.None; decimals: 6 }
	}
}
