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
#include "syntaxbridge_interface.h"
#include "json/json.h"

static bool		global_param_dbInMemory				= false;
static bool		global_param_orderLabelsByValue		= true;
static int		global_param_threshold				= 10;

// [[Rcpp::export]]
void cleanUp()
{
	syntaxBridgeCleanup();
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

	syntaxBridgeLoadDataSet(&dataset, global_param_dbInMemory, global_param_threshold, global_param_orderLabelsByValue);
}


// [[Rcpp::export]]
String loadQmlAndParseOptions(String moduleName, String analysisName, String qmlFile, String options, String version, bool preloadData)
{
	std::string qmlFileStr		= qmlFile.get_cstring(),
				optionsStr		= options.get_cstring(),
				versionStr		= version.get_cstring(),
				analysisNameStr	= analysisName.get_cstring(),
				moduleNameStr	= moduleName.get_cstring();


	return syntaxBridgeLoadQmlAndParseOptions(moduleNameStr.c_str(), analysisNameStr.c_str(), qmlFileStr.c_str(), optionsStr.c_str(), versionStr.c_str(), preloadData);
}

// [[Rcpp::export]]
String generateModuleWrappers(String modulePath)
{
	std::string modulePathStr = modulePath.get_cstring();

	return syntaxBridgeGenerateModuleWrappers(modulePathStr.c_str());
}

// [[Rcpp::export]]
Rcpp::List parseDescription(String modulePath)
{
	std::string modulePathStr = modulePath.get_cstring();

	std::string rawDescription = syntaxBridgeParseDescription(modulePathStr.c_str());

	Json::Value parsedDescription;
	Json::Reader().parse(rawDescription, parsedDescription);

	Rcpp::List result;

	result["name"]				= parsedDescription["name"].asString();
	result["title"]				= parsedDescription["title"].asString();
	result["author"]			= parsedDescription["author"].asString();
	result["website"]			= parsedDescription["website"].asString();
	result["license"]			= parsedDescription["license"].asString();
	result["maintainer"]		= parsedDescription["maintainer"].asString();
	result["description"]		= parsedDescription["description"].asString();
	result["requiresData"]		= parsedDescription["requiresData"].asBool();
	result["hasWrappers"]		= parsedDescription["hasWrappers"].asBool();
	result["isCommon"]			= parsedDescription["isCommon"].asBool();
	result["version"]			= parsedDescription["version"].asString();

	Rcpp::List analyses;

	for (const Json::Value & jsonAnalysis : parsedDescription["analyses"])
	{
		Rcpp::List analysis;
		analysis["name"]		= jsonAnalysis["name"].asString();
		analysis["qml"]			= jsonAnalysis["qml"].asString();
		analysis["title"]		= jsonAnalysis["title"].asString();
		analysis["preloadData"] = jsonAnalysis["preloadData"].asBool();
		analysis["hasWrapper"]	= jsonAnalysis["hasWrapper"].asBool();

		analyses.push_back(analysis);
	}

	result["analyses"] = analyses;

	return result;
}


// [[Rcpp::export]]
String generateAnalysisWrapper(String modulePath, String analysisName)
{
	std::string modulePathStr	= modulePath.get_cstring(),
				analysisNameStr	= analysisName.get_cstring();

	return syntaxBridgeGenerateAnalysisWrapper(modulePathStr.c_str(), analysisNameStr.c_str());
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




