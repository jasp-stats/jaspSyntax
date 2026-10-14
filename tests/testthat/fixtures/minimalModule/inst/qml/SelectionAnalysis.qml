import QtQuick
import JASP
import JASP.Controls

// Multi-dataset aware form for the selection-surface tests (mds-plan C4): the form picks its
// own dataset through dataSetSelectionOption, so the bridge's raw-option translation (human
// surface string -> filter id, before anything binds) is exercised end to end.
Form
{
	VariablesForm
	{
		dataSetSelectionOption: "dataSetA"

		AvailableVariablesList
		{
			name: "allVariablesListA"
		}

		AssignedVariablesList
		{
			name: "dependentA"
			title: qsTr("Dependent")
			depends: "dataSetA"
		}
	}
}
