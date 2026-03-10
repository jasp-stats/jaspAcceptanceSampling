import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

Form
{
	columns: 1

	VariablesForm
	{
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "ssm_count"; title: qsTr("Defect Count"); allowedColumns: ["scale"]; singleVariable: true }
		AssignedVariablesList { name: "ssm_sampleSize"; title: qsTr("Sample Size (tested items)"); allowedColumns: ["scale"]; singleVariable: true }
		AssignedVariablesList { name: "ssm_total"; title: qsTr("Lot Size (total items)"); allowedColumns: ["scale"]; singleVariable: true }
		AssignedVariablesList { name: "ssm_time"; title: qsTr("Time"); allowedColumns: ["scale", "ordinal"]; singleVariable: true }
		AssignedVariablesList { name: "ssm_predictor"; title: qsTr("Predictor"); allowedColumns: ["scale"]; singleVariable: true; visible: scaleModel.currentValue === "WithPredictor" }
	}

	DropDown
	{
		id: scaleModel
		name: "ssm_scaleModel"
		label: qsTr("Model Type")
		indexDefaultValue: 0
		values: [
			{ label: qsTr("Without predictor"), value: "WithoutPredictor" },
			{ label: qsTr("With predictor"), value: "WithPredictor" }
		]
	}

	Section
	{
		title: qsTr("Settings")
		columns: 1

		Group
		{
			title: qsTr("Acceptance Limits")
			DoubleField { id: lowerLimit; name: "ssm_controlLimitsLower"; label: qsTr("Lower limit (AQL-like)"); defaultValue: 0.05; min: 0; max: upperLimit.value }
			DoubleField { id: upperLimit; name: "ssm_controlLimitsUpper"; label: qsTr("Upper limit (RQL-like)"); defaultValue: 0.10; min: lowerLimit.value; max: 1 }
		}

		Group
		{
			title: qsTr("Sudden Jump Tail Correction")
			CheckBox { id: postHocCorrection; name: "ssm_postHocCorrection"; label: qsTr("Enable sudden jump tail correction") }
			DoubleField { name: "ssm_postHocCorrectionWeight"; label: qsTr("Correction weight (%)"); defaultValue: 5; min: 0; max: 100; enabled: postHocCorrection.checked }
		}

		Group
		{
			title: qsTr("Decision Rule")
			DoubleField { name: "ssm_decisionAcceptThreshold"; label: qsTr("Accept if P(defects > RQL) <"); defaultValue: 0.10; min: 0; max: 1 }
			DoubleField { name: "ssm_decisionRejectThreshold"; label: qsTr("Reject if not accepted and P(defects < AQL) <"); defaultValue: 0.05; min: 0; max: 1 }
		}
	}

	Section
	{
		title: qsTr("Plots")
		columns: 1

		CheckBox { name: "ssm_statePlot"; label: qsTr("State time series (defect rate)"); checked: true }
		CheckBox { name: "ssm_predictionPlot"; label: qsTr("Posterior predictive for current lot"); checked: true }
		CheckBox { name: "ssm_posteriorDistPlot"; label: qsTr("Posterior for latest defect rate") }
		CheckBox { name: "ssm_plotBeta"; label: qsTr("Posterior for predictor coefficient beta"); visible: scaleModel.currentValue === "WithPredictor" }
	}

	Section
	{
		title: qsTr("Model")
		columns: 1

		Group
		{
			title: qsTr("Prior initial defect rate")
			DoubleField { name: "ssm_priorTheta1Shape1"; label: qsTr("\u03B8\u2081 alpha"); defaultValue: 1; min: 0.01 }
			DoubleField { name: "ssm_priorTheta1Shape2"; label: qsTr("\u03B8\u2081 beta"); defaultValue: 1; min: 0.01 }
		}

		Group
		{
			title: qsTr("State noise")
			DoubleField { name: "ssm_priorSigmaSD"; label: qsTr("Half-normal SD"); defaultValue: 1; min: 0.01 }
		}

		Group
		{
			title: qsTr("Predictor coefficient")
			visible: scaleModel.currentValue === "WithPredictor"
			DoubleField { name: "ssm_priorBetaScale"; label: qsTr("Cauchy scale"); defaultValue: 0.707; min: 0.01 }
		}
	}

	Section
	{
		title: qsTr("MCMC Diagnostics")
		columns: 1

		CheckBox { id: showMcmcSummary; name: "ssm_showMcmcSummary"; label: qsTr("Overview table") }

		Group
		{
			title: qsTr("Select parameters for summary table")
			enabled: showMcmcSummary.checked
			CheckBox { name: "ssm_showTheta"; label: qsTr("All theta[t]") }
			CheckBox { name: "ssm_showSigma"; label: qsTr("Sigma (state noise)") }
			CheckBox { name: "ssm_showYpred"; label: qsTr("y_pred (prediction)") }
			CheckBox { name: "ssm_showBeta"; label: qsTr("Beta (predictor coefficient)"); visible: scaleModel.currentValue === "WithPredictor" }
		}
	}

	Section
	{
		title: qsTr("Advanced")
		columns: 1

		Group
		{
			title: qsTr("MCMC")
			IntegerField { name: "ssm_advancedMcmcBurnin"; label: qsTr("Burnin"); defaultValue: 2000; min: 100 }
			IntegerField { name: "ssm_advancedMcmcSamples"; label: qsTr("Samples"); defaultValue: 5000; min: 100 }
			IntegerField { name: "ssm_advancedMcmcChains"; label: qsTr("Chains"); defaultValue: 3; min: 1 }
			IntegerField { name: "ssm_advancedMcmcThin"; label: qsTr("Thin"); defaultValue: 1; min: 1 }
			IntegerField { name: "ssm_advancedMcmcSeed"; label: qsTr("Seed"); defaultValue: 1; min: 1 }
			DoubleField { name: "ssm_advancedMcmcAdaptDelta"; label: qsTr("Adapt Delta"); defaultValue: 0.95; min: 0.5; max: 0.999 }
			IntegerField { name: "ssm_advancedMcmcMaxTreeDepth"; label: qsTr("Max tree depth"); defaultValue: 10; min: 5; max: 20 }
		}
	}
}
