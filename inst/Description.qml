import QtQuick
import JASP.Module

Description
{
	name		: "jaspAcceptanceSampling"
	title		: qsTr("Acceptance Sampling")
	description	: qsTr("Sampling for acceptance")
	icon		: "acceptance_sampling.svg"
	version			: "0.95.0"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "jasp-stats.org"
	license		: "GPL (>= 2)"

	GroupTitle
	{
		title:	qsTr("Attribute Sampling")
		icon:	"attribute.svg"
	}

	Analysis
	{
		title:	qsTr("Create Attribute Plan")
		qml:	"CreateAttributePlan.qml"
		func:	"CreateAttributePlan"
		requiresData: false
	}

	Analysis
	{
		title:	qsTr("Analyze Attribute Plan")
		qml:	"AnalyzeAttributePlan.qml"
		func:	"AnalyzeAttributePlan"
		requiresData: false
	}

	Separator {}

	GroupTitle
	{
		title:	qsTr("Variable Sampling")
		icon:	"variable.svg"
	}

	Analysis
	{
		title:	qsTr("Create Variable Plan")
		qml:	"CreateVariablePlan.qml"
		func:	"CreateVariablePlan"
		requiresData: false
	}

	Analysis
	{
		title:	qsTr("Analyze Variable Plan")
		qml:	"AnalyzeVariablePlan.qml"
		func:	"AnalyzeVariablePlan"
		requiresData: false
	}

	Analysis
	{
		menu:	qsTr("Accept/Reject Lots")
		title:	qsTr("Accept/Reject Lots (k-method)")
		qml:	"DecideVariableLots.qml"
		func:	"DecideVariableLots"
		requiresData: false
	}
}
