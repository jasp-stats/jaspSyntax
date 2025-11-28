//
// Copyright (C) 2013-2025 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "syntaxbridge_interface.h"

#include <QtPlugin>
Q_IMPORT_PLUGIN(QMinimalIntegrationPlugin)

extern "C"
{

namespace backend
{
	void STDCALL cleanUp()
	{
		syntaxBridgeCleanup();
	}

	void STDCALL loadDataSet(const SyntaxBridgeDataSet& dataset, bool inMemory, bool threshold, bool orderLabelsByValue)
	{
		syntaxBridgeLoadDataSet(&dataset, inMemory, threshold, orderLabelsByValue);
	}

	const STDCALL char* loadQmlAndParseOptions(const char* moduleName, const char* analysisName, const char* qmlFile, const char* options, const char* version, bool preloadData)
	{
		return syntaxBridgeLoadQmlAndParseOptions(moduleName, analysisName, qmlFile, options, version, preloadData);
	}

	const STDCALL char* generateModuleWrappers(const char* modulePath, bool preloadData)
	{
		return syntaxBridgeGenerateModuleWrappers(modulePath, preloadData);
	}


	const STDCALL char* generateAnalysisWrapper(const char* modulePath, const char* qmlFileName, const char* analysisName, const char* title, bool preloadData)
	{
		return syntaxBridgeGenerateAnalysisWrapper(modulePath, qmlFileName, analysisName, title, preloadData);
	}
}

}




