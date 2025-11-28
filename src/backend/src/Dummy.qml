import QtQuick
import QtQuick.Layouts
import QtQuick.Controls as QtC
import JASP
import JASP.Controls
 
 // This file is here to trigger qml imports
 
RowLayout
{
	CheckBox { id: check}
	QtC.CheckBox { id: sss }
	Label
	{
		text: "Hallo" + (check.checked ? " clicked" : "")

		Component.onCompleted: console.log("HALLO")
	}
}
