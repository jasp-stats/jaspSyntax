import QtQuick
import JASP
import JASP.Controls

// Multi-dataset aware form for the selection-surface tests: the form picks its
// own dataset through dataSetSelectionOption, so the bridge's raw-option translation (human
// surface string -> filter id, before anything binds) is exercised end to end.
Form
{
	VariablesForm
	{
		// moved dataSetSelectionOption from the form to the LISTS -
		// the list that declares it is the namespace host (fixture re-pinned at the
		// first libSyntaxInterface resync after that commit).
		AvailableVariablesList
		{
			name: "allVariablesListA"
			dataSetSelectionOption: "dataSetA"
		}

		AssignedVariablesList
		{
			name: "dependentA"
			title: qsTr("Dependent")
			depends: "dataSetA"
		}
	}
}
