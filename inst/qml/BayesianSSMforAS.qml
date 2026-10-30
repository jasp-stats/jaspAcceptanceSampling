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
		AssignedVariablesList { name: "ssm_time"; title: qsTr("Lot Number"); allowedColumns: ["scale", "ordinal"]; singleVariable: true }
		AssignedVariablesList { id: predictor; name: "ssm_predictor"; title: qsTr("Covariates (Optional)"); allowedColumns: ["scale"] }
	}

	Section
	{
		title: qsTr("Settings")
		columns: 1

		Group
		{
			title: qsTr("Acceptance Limits")
			DoubleField { id: lowerLimit; name: "ssm_controlLimitsLower"; label: qsTr("Lower limit (AQL)"); defaultValue: 0.05; min: 0; max: upperLimit.value }
			DoubleField { id: upperLimit; name: "ssm_controlLimitsUpper"; label: qsTr("Upper limit (RQL)"); defaultValue: 0.10; min: lowerLimit.value; max: 1 }
		}

		Group
		{
			title: qsTr("Decision Rule")
			DoubleField
			{
				id: acceptThreshold
				name: "ssm_decisionAcceptThreshold"
				label: qsTr("Accept if P(above RQL) <")
				info: qsTr("Accept when the posterior predictive probability that the lot's defect proportion exceeds RQL is below this acceptance threshold.")
				defaultValue: 0.10; min: 0; max: 1
			}
			DoubleField
			{
				name: "ssm_decisionRejectThreshold"
				label: qsTr("Reject if P(above RQL) > %1 and P(below AQL) <").arg(acceptThreshold.value)
				info: qsTr("Reject when the posterior predictive probability that the lot's defect proportion exceeds RQL is greater than the acceptance threshold, and the probability that it is below AQL is below this rejection threshold. Equality to the acceptance threshold gives Continue.")
				defaultValue: 0.05; min: 0; max: 1
			}
			Label
			{
				text: qsTr("Continue sampling if neither rule is satisfied.")
			}
		}
	}

	Section
	{
		title: qsTr("Plots")
		columns: 1

		CheckBox { name: "ssm_statePlot"; label: qsTr("State time series (defect rate)"); checked: true }
		CheckBox { name: "ssm_predictionPlot"; label: qsTr("Posterior predictive for current lot"); checked: true }
		CheckBox { name: "ssm_posteriorDistPlot"; label: qsTr("Posterior for latest defect rate") }
		CheckBox { name: "ssm_plotBeta"; label: qsTr("Posteriors for predictor coefficients"); visible: predictor.count > 0 }
	}

	Section
	{
		title: qsTr("Model")
		columns: 1

		Group
		{
			title: qsTr("Initial baseline probability")
			info: qsTr("Beta prior for the first lot's defect probability without predictors, or its baseline probability when all covariates are at their means.")
			DoubleField { name: "ssm_priorTheta1Shape1"; label: qsTr("\u03B8\u2081 alpha"); defaultValue: 1; min: 0.01 }
			DoubleField { name: "ssm_priorTheta1Shape2"; label: qsTr("\u03B8\u2081 beta"); defaultValue: 1; min: 0.01 }
		}

		Group
		{
			title: qsTr("Baseline innovation scale")
			DoubleField
			{
				name: "ssm_priorSigmaSD"
				label: qsTr("Half-normal prior scale")
				info: qsTr("Sets the underlying normal standard deviation of the half-normal prior. The baseline innovation scale itself is estimated from the data. The mixture's broad-component probability is fixed at 0.05 and its additional spread at 1.5 on the logit scale.")
				defaultValue: 1; min: 0.01
			}
		}

		Group
		{
			title: qsTr("Predictor coefficients")
			DoubleField
			{
				name: "ssm_priorBetaScale"
				label: qsTr("Cauchy scale")
				info: qsTr("Common scale of the independent zero-centered Cauchy priors for the predictor coefficients. You can set this before assigning covariates. Each coefficient is the change in log odds per one standard deviation increase in that covariate, holding the other covariates and baseline fixed.")
				defaultValue: 0.707; min: 0.01
			}
		}
	}

	Section
	{
		title: qsTr("MCMC Diagnostics")
		columns: 1

		CheckBox
		{
			name: "ssm_printDiagnostics"
			label: qsTr("Print Diagnostics")
			checked: false
			info: qsTr("Show the full computational diagnostics. Warnings about sampling problems or unavailable checks are always shown.")
		}

		CheckBox { id: showMcmcSummary; name: "ssm_showMcmcSummary"; label: qsTr("Overview table") }

		Group
		{
			title: qsTr("Select parameters for summary table")
			enabled: showMcmcSummary.checked
			CheckBox { name: "ssm_showTheta"; label: qsTr("All theta[t]") }
			CheckBox { name: "ssm_showSigma"; label: qsTr("Sigma (state noise)") }
			CheckBox { name: "ssm_showYpred"; label: qsTr("y_pred (prediction)") }
			CheckBox { name: "ssm_showBeta"; label: qsTr("Beta (predictor coefficients)"); visible: predictor.count > 0 }
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
			DoubleField { name: "ssm_advancedMcmcAdaptDelta"; label: qsTr("Adapt Delta"); defaultValue: 0.97; min: 0.5; max: 0.999 }
			IntegerField { name: "ssm_advancedMcmcMaxTreeDepth"; label: qsTr("Max tree depth"); defaultValue: 15; min: 5; max: 20 }
		}
	}
}
