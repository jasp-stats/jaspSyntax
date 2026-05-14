import QtQuick
import JASP.Module

Description
{
	title: qsTr("Descriptives")
	description: qsTr("Minimal fixture for saved Descriptives replay.")
	preloadData: true
	hasWrappers: true

	Analysis
	{
		title: qsTr("Descriptive Statistics")
		func: "Descriptives"
		qml: "Descriptives.qml"
		preloadData: true
	}
}
