import QtQuick
import JASP.Module

Description
{
	title: qsTr("Syntax Test Module")
	description: qsTr("Minimal module fixture for jaspSyntax API tests.")
	preloadData: true
	hasWrappers: true

	Analysis
	{
		title: qsTr("Default Analysis")
		func: "DefaultAnalysis"
	}

	Analysis
	{
		title: qsTr("Minimal Analysis")
		func: "MinimalAnalysis"
		qml: "MinimalAnalysis.qml"
		preloadData: false
	}

	Analysis
	{
		title: qsTr("Variable Analysis")
		func: "VariableAnalysis"
		qml: "VariableAnalysis.qml"
		preloadData: true
	}
}
