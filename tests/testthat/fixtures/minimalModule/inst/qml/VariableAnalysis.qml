import QtQuick
import JASP
import JASP.Controls

Form
{
	VariablesForm
	{
		AvailableVariablesList
		{
			name: "allVariablesList"
		}

		AssignedVariablesList
		{
			name: "variables"
			title: qsTr("Variables")
		}
	}
}
