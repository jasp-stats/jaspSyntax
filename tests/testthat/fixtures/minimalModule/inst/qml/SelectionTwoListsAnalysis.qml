import QtQuick
import JASP
import JASP.Controls

// mds-plan D8.4: TWO per-list dataset selections in ONE form - the self-contained twin of the
// jaspTestModule zoo form (testMultiDataSet.qml). Each VariablesForm carries its own selecting
// AvailableVariablesList (dataSetA / dataSetB, mds-plan C10/D2.2) with an assigned list that
// depends on it, so the bridge's raw-option translation must resolve BOTH selection options
// (the registry is per-list since D2.3) before anything binds, and each section then binds
// against its own dataset. Ids stay invisible: the syntax surface takes title strings only
// (mds-plan C4), the bound wire value is the filter id, and the emitted syntax renders the
// title surface (rsyntax's selectionSurfaceFromBoundValue).
Form
{
	VariablesForm
	{
		AvailableVariablesList
		{
			name: "allVariablesListA"
			dataSetSelectionOption: "dataSetA"
		}

		AssignedVariablesList
		{
			name: "dependentA"
			title: qsTr("Dependent A")
			depends: "dataSetA"
		}
	}

	VariablesForm
	{
		AvailableVariablesList
		{
			name: "allVariablesListB"
			dataSetSelectionOption: "dataSetB"
		}

		AssignedVariablesList
		{
			name: "dependentB"
			title: qsTr("Dependent B")
			depends: "dataSetB"
		}
	}
}
