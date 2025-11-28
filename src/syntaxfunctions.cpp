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

#include <Rcpp.h>
using namespace Rcpp;

#include "dataframeimporter.h"
#include "backend/src/syntaxBackend.h"

static bool		global_param_dbInMemory				= false;
static bool		global_param_orderLabelsByValue		= true;
static int		global_param_threshold				= 10;

// [[Rcpp::export]]
void cleanUp()
{
	backend::cleanUp();
}

// [[Rcpp::export]]
bool setParameter(String name, SEXP value)
{
	std::string nameStr		= name.get_cstring();

	if (nameStr == "dbInMemory" && Rcpp::is<bool>(value))
	{
		global_param_dbInMemory = Rcpp::as<bool>(value);
		return true;
	}
	else if (nameStr == "threshold" && Rcpp::is<int>(value))
	{
		global_param_threshold = Rcpp::as<int>(value);
		return true;
	}
	else if (nameStr == "orderLabelsByValue" && Rcpp::is<bool>(value))
	{
		global_param_orderLabelsByValue = Rcpp::as<bool>(value);
		return true;
	}

	return false;
}

// [[Rcpp::export]]
void loadDataSet(Rcpp::List data)
{
	const SyntaxBridgeDataSet& dataset = DataFrameImporter::loadDataFrame(data);

	backend::loadDataSet(dataset, global_param_dbInMemory, global_param_threshold, global_param_orderLabelsByValue);
}


// [[Rcpp::export]]
String loadQmlAndParseOptions(String moduleName, String analysisName, String qmlFile, String options, String version, bool preloadData)
{
	std::string qmlFileStr		= qmlFile.get_cstring(),
				optionsStr		= options.get_cstring(),
				versionStr		= version.get_cstring(),
				analysisNameStr	= analysisName.get_cstring(),
				moduleNameStr	= moduleName.get_cstring();


	return backend::loadQmlAndParseOptions(moduleNameStr.c_str(), analysisNameStr.c_str(), qmlFileStr.c_str(), optionsStr.c_str(), versionStr.c_str(), preloadData);
}

// [[Rcpp::export]]
String generateModuleWrappers(String modulePath, bool preloadData)
{

	std::string modulePathStr = modulePath.get_cstring();

	return backend::generateModuleWrappers(modulePathStr.c_str(), preloadData);
}


// [[Rcpp::export]]
String generateAnalysisWrapper(String modulePath, String qmlFileName, String analysisName, String title, bool preloadData)
{
	std::string qmlFileNameStr	= qmlFileName.get_cstring(),
				modulePathStr	= modulePath.get_cstring(),
				analysisNameStr	= analysisName.get_cstring(),
				titleStr		= title.get_cstring();

	return backend::generateAnalysisWrapper(modulePathStr.c_str(), qmlFileNameStr.c_str(), analysisNameStr.c_str(), titleStr.c_str(), preloadData);
}

// [[Rcpp::export]]
Rcpp::List getVariableNames()
{
	return DataFrameImporter::getVariableNames();
}

// [[Rcpp::export]]
Rcpp::List getVariableValues(String variableName)
{
	return DataFrameImporter::getVariableValues(variableName);
}




