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
	Group
	{
		IntegerField { name: "lotSize"; label: qsTr("Lot size (N)"); defaultValue: 1000; min: 1}
		IntegerField { name: "sampleSize"; label: qsTr("Sample size (n)"); defaultValue: 24; min: 1 }
		DoubleField { name: "kValue"; label: qsTr("k"); defaultValue: 1.309; min: 0; negativeValues: false; inclusive: JASP.None; decimals: 6 }
		CheckBox { name: "sd"; label: qsTr("Standard deviation (historical) known"); id: sd; checked: true }
	}

	Group
	{
		CheckBox { name: "assessPlan"; label: qsTr("Assess variable plan"); id: assessVariable }
		Common.RiskPoints
		{
			enabled: assessVariable.checked
		}
	}

	Common.ProbDefect {}
	Common.OutputOptions {}
}
