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

template <typename Func>
auto callBridgeOrStop(const char * functionName, Func func) -> decltype(func())
{
	try
	{
		return func();
	}
	catch (const std::exception & exception)
	{
		Rcpp::stop("%s failed: %s", functionName, exception.what());
	}
	catch (...)
	{
		Rcpp::stop("%s failed with an unknown exception.", functionName);
	}
}

Json::Value parseBridgeJsonOrStop(const char * rawJson, const char * functionName)
{
	if (rawJson == nullptr)
		Rcpp::stop("%s returned a null pointer.", functionName);

	Json::Value parsedJson;
	Json::Reader reader;
	if (!reader.parse(rawJson, parsedJson))
		Rcpp::stop("%s returned invalid JSON.", functionName);

	return parsedJson;
}

// [[Rcpp::export]]
void cleanUp()
{
	callBridgeOrStop("syntaxBridgeClearNativeState", []() {
		syntaxBridgeClearNativeState();
	});
	callBridgeOrStop("syntaxBridgeCleanup", []() {
		syntaxBridgeCleanup();
	});
}

// [[Rcpp::export]]
void shutdownNative()
{
	callBridgeOrStop("syntaxBridgeShutdown", []() {
		syntaxBridgeShutdown();
	});
}

// [[Rcpp::export]]
void clearQmlFormsNative()
{
	callBridgeOrStop("syntaxBridgeClearQmlState", []() {
		syntaxBridgeClearQmlState();
	});
}

// [[Rcpp::export]]
void clearDatasetStateNative()
{
	callBridgeOrStop("syntaxBridgeClearDataSetState", []() {
		syntaxBridgeClearDataSetState();
	});
}

// [[Rcpp::export]]
void clearNativeStateNative()
{
	callBridgeOrStop("syntaxBridgeClearNativeState", []() {
		syntaxBridgeClearNativeState();
	});
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

	callBridgeOrStop("syntaxBridgeLoadDataSet", [&]() {
		syntaxBridgeLoadDataSet(&dataset, global_param_dbInMemory, global_param_threshold, global_param_orderLabelsByValue);
	});
}


// [[Rcpp::export]]
String loadQmlAndParseOptions(String moduleName, String analysisName, String qmlFile, String options, String version, bool preloadData)
{
	std::string qmlFileStr		= qmlFile.get_cstring(),
				optionsStr		= options.get_cstring(),
				versionStr		= version.get_cstring(),
				analysisNameStr	= analysisName.get_cstring(),
				moduleNameStr	= moduleName.get_cstring();


	return callBridgeOrStop("syntaxBridgeLoadQmlAndParseOptions", [&]() {
		return syntaxBridgeLoadQmlAndParseOptions(moduleNameStr.c_str(), analysisNameStr.c_str(), qmlFileStr.c_str(), optionsStr.c_str(), versionStr.c_str(), preloadData);
	});
}

// [[Rcpp::export]]
String generateModuleWrappers(String modulePath)
{
	std::string modulePathStr = modulePath.get_cstring();

	return callBridgeOrStop("syntaxBridgeGenerateModuleWrappers", [&]() {
		return syntaxBridgeGenerateModuleWrappers(modulePathStr.c_str());
	});
}

// [[Rcpp::export]]
Rcpp::List parseDescription(String modulePath)
{
	std::string modulePathStr = modulePath.get_cstring();

	Json::Value parsedDescription = parseBridgeJsonOrStop(
		callBridgeOrStop("syntaxBridgeParseDescription", [&]() {
			return syntaxBridgeParseDescription(modulePathStr.c_str());
		}),
		"syntaxBridgeParseDescription"
	);

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

	const Json::Value & jsonAnalyses = parsedDescription["analyses"];
	Rcpp::List analyses(jsonAnalyses.size());

	for (Json::ArrayIndex i = 0; i < jsonAnalyses.size(); ++i)
	{
		const Json::Value & jsonAnalysis = jsonAnalyses[i];
		Rcpp::List analysis;
		analysis["name"]		= jsonAnalysis["name"].asString();
		analysis["qml"]			= jsonAnalysis["qml"].asString();
		analysis["title"]		= jsonAnalysis["title"].asString();
		analysis["preloadData"] = jsonAnalysis["preloadData"].asBool();
		analysis["hasWrapper"]	= jsonAnalysis["hasWrapper"].asBool();

		analyses[i] = analysis;
	}

	result["analyses"] = analyses;

	return result;
}

// [[Rcpp::export]]
void loadDataSetFromJaspFile(String jaspFilePath)
{
	std::string jaspFilePathStr = jaspFilePath.get_cstring();

	Json::Value status = parseBridgeJsonOrStop(
		callBridgeOrStop("syntaxBridgeLoadDataSetFromJaspFileStatus", [&]() {
			return syntaxBridgeLoadDataSetFromJaspFileStatus(jaspFilePathStr.c_str(), global_param_dbInMemory);
		}),
		"syntaxBridgeLoadDataSetFromJaspFileStatus"
	);

	if (!status["ok"].asBool())
	{
		std::string error = status.isMember("error") ? status["error"].asString() : "unknown error";
		Rcpp::stop("syntaxBridgeLoadDataSetFromJaspFile failed: %s", error);
	}
}

SEXP transformJsonValueToSEXP(const Json::Value & json);

Rcpp::List transformJsonArrayToRcppList(const Json::Value & json)
{
	Rcpp::List result(json.size());
	for (Json::ArrayIndex i = 0; i < json.size(); ++i)
		result[i] = transformJsonValueToSEXP(json[i]);

	return result;
}

Rcpp::List transformJsonObjectToRcppList(const Json::Value & json)
{
	std::vector<std::string> memberNames = json.getMemberNames();
	Rcpp::List result(memberNames.size());
	Rcpp::CharacterVector resultNames(memberNames.size());

	for (size_t i = 0; i < memberNames.size(); ++i)
	{
		result[i] = transformJsonValueToSEXP(json[memberNames[i]]);
		resultNames[i] = memberNames[i];
	}

	result.attr("names") = resultNames;

	return result;
}

SEXP transformJsonValueToSEXP(const Json::Value & json)
{
	if (json.isNull())
		return R_NilValue;
	else if (json.isBool())
		return Rcpp::wrap(json.asBool());
	else if (json.isInt())
		return Rcpp::wrap(json.asInt());
	else if (json.isDouble()) // must be after isInt!
		return Rcpp::wrap(json.asDouble());
	else if (json.isString())
		return Rcpp::wrap(json.asString());
	else if (json.isArray())
		return Rcpp::wrap(transformJsonArrayToRcppList(json));
	else if (json.isObject())
		return Rcpp::wrap(transformJsonObjectToRcppList(json));

	return R_NilValue;
}

// [[Rcpp::export]]
Rcpp::List analysisOptionsFromJaspFile(String jaspFilePath, int analysisNr)
{
	std::string jaspFilePathStr = jaspFilePath.get_cstring();

	Json::Value status = parseBridgeJsonOrStop(
		callBridgeOrStop("syntaxBridgeAnalysisOptionsFromJaspFileStatus", [&]() {
			return syntaxBridgeAnalysisOptionsFromJaspFileStatus(jaspFilePathStr.c_str(), analysisNr);
		}),
		"syntaxBridgeAnalysisOptionsFromJaspFileStatus"
	);
	if (!status.isObject())
		Rcpp::stop("syntaxBridgeAnalysisOptionsFromJaspFileStatus returned a non-object status.");
	if (!status["ok"].asBool())
	{
		std::string error = status.isMember("error") ? status["error"].asString() : "unknown error";
		Rcpp::stop(
			"syntaxBridgeAnalysisOptionsFromJaspFile failed for analysis %d in file %s: %s",
			analysisNr,
			jaspFilePathStr.c_str(),
			error
		);
	}

	const Json::Value & parsedOptions = status["options"];
	if (!parsedOptions.isObject())
		Rcpp::stop(
			"syntaxBridgeAnalysisOptionsFromJaspFileStatus returned %s instead of a JSON object for analysis %d in file: %s",
			parsedOptions.isNull() ? "null" : "a non-object value",
			analysisNr,
			jaspFilePathStr.c_str()
		);

	return transformJsonObjectToRcppList(parsedOptions);
}

// [[Rcpp::export]]
String generateAnalysisWrapper(String modulePath, String analysisName)
{
	std::string modulePathStr	= modulePath.get_cstring(),
				analysisNameStr	= analysisName.get_cstring();

	return callBridgeOrStop("syntaxBridgeGenerateAnalysisWrapper", [&]() {
		return syntaxBridgeGenerateAnalysisWrapper(modulePathStr.c_str(), analysisNameStr.c_str());
	});
}

// [[Rcpp::export]]
Rcpp::List getVariableNames()
{
	Json::Value parsedNames = parseBridgeJsonOrStop(
		callBridgeOrStop("syntaxBridgeGetVariableNames", [&]() {
			return syntaxBridgeGetVariableNames();
		}),
		"syntaxBridgeGetVariableNames"
	);

	Rcpp::List result;
	for (const Json::Value & parsedName : parsedNames)
		result.push_back(parsedName.asCString());

	return result;
}



