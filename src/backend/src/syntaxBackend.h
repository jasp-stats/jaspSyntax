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

#ifdef _WIN32
#define EXPORT __declspec(dllexport)
#else
#define EXPORT
#endif

#include "syntaxbridge_interface.h"

extern "C" {

namespace backend
{
	SYNTAX_INTERFACE void			STDCALL cleanUp();
	SYNTAX_INTERFACE void			STDCALL loadDataSet(const SyntaxBridgeDataSet& dataset, bool inMemory, bool threshold, bool orderLabelsByValue);
	SYNTAX_INTERFACE const char*	STDCALL loadQmlAndParseOptions(const char* moduleName, const char* analysisName, const char* qmlFile, const char* options, const char* version, bool preloadData);
	SYNTAX_INTERFACE const char*	STDCALL generateModuleWrappers(const char* modulePath, bool preloadData);
	SYNTAX_INTERFACE const char*	STDCALL generateAnalysisWrapper(const char* modulePath, const char* qmlFileName, const char* analysisName, const char* title, bool preloadData);
}
}




