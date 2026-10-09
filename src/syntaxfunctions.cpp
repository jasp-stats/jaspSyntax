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

#include <algorithm>
#include <cctype>

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

std::string normalizeParameterString(std::string value)
{
	auto isNotSpace = [](unsigned char c) { return !std::isspace(c); };
	value.erase(value.begin(), std::find_if(value.begin(), value.end(), isNotSpace));
	value.erase(std::find_if(value.rbegin(), value.rend(), isNotSpace).base(), value.end());
	std::transform(value.begin(), value.end(), value.begin(), [](unsigned char c) { return std::tolower(c); });
	return value;
}

bool verboseValueShowsNativeOutput(const std::string & value, bool & verbose)
{
	const std::string normalizedValue = normalizeParameterString(value);
	if (normalizedValue == "all" || normalizedValue == "jasp" || normalizedValue == "true" ||
		normalizedValue == "yes" || normalizedValue == "on" || normalizedValue == "1")
	{
		verbose = true;
		return true;
	}

	if (normalizedValue == "analysis" || normalizedValue == "none" || normalizedValue == "false" ||
		normalizedValue == "no" || normalizedValue == "off" || normalizedValue == "0")
	{
		verbose = false;
		return true;
	}

	return false;
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
bool setParameterNative(String name, SEXP value)
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
	else if (nameStr == "verbose")
	{
		if (Rcpp::is<bool>(value))
		{
			syntaxBridgeSetVerbose(Rcpp::as<bool>(value));
			return true;
		}

		if (TYPEOF(value) == STRSXP && Rf_length(value) > 0)
		{
			bool verbose = false;
			if (!verboseValueShowsNativeOutput(Rcpp::as<std::string>(value), verbose))
				Rcpp::stop("Unsupported verbose value. Use 'all', 'analysis', 'jasp', 'none', TRUE, or FALSE.");

			syntaxBridgeSetVerbose(verbose);
			return true;
		}

		return false;
	}

	return false;
}

// [[Rcpp::export]]
int loadDataSet(Rcpp::List data)
{
	const SyntaxBridgeDataSet& dataset = DataFrameImporter::loadDataFrame(data);

	//Returns the REAL database id the bridge assigned: the invariant handle to use inside
	//scripts and wrappers; the title stays a display affordance and a convenience input.
	return callBridgeOrStop("syntaxBridgeLoadDataSet", [&]() {
		return syntaxBridgeLoadDataSet(&dataset, global_param_dbInMemory, global_param_threshold, global_param_orderLabelsByValue);
	});
}

//' Load a named list of datasets for a multi-dataset aware analysis (syntax mode).
//' Each entry becomes its own DataSet in the bridge workspace (real id + column encoder),
//' titled with the list name; the first entry is the primary. State is cleared first, so
//' repeated calls rebuild cleanly.
//' @return named integer vector with the REAL database id of every loaded dataset
//'   (c(Name = id, ...)): the invariant handle for dataset selection options and logging,
//'   instead of referring to datasets by title.
// [[Rcpp::export]]
Rcpp::IntegerVector loadDataSets(Rcpp::List datasets)
{
	SEXP namesR = datasets.names();

	if (namesR == R_NilValue)
		Rcpp::stop("`datasets` must be a named list of data.frames");

	Rcpp::CharacterVector names(namesR);

	if (names.size() != datasets.size())
		Rcpp::stop("`datasets` must have exactly one name per dataset");

	for (int i = 0; i < datasets.size(); i++)
	{
		const std::string name = Rcpp::as<std::string>(names[i]);

		if (name.empty())
			Rcpp::stop("`datasets` entry %d has no name", i + 1);

		for (int j = 0; j < i; j++)
			if (Rcpp::as<std::string>(names[j]) == name)
				Rcpp::stop("`datasets` has the name '%s' more than once; dataset titles must be unique", name.c_str());

		if (!Rf_inherits(Rcpp::RObject(datasets[i]), "data.frame"))
			Rcpp::stop("`datasets` entry '%s' is not a data.frame", name.c_str());
	}

	//Fresh start, then accumulate: the named loads keep every dataset in the workspace
	//(see syntaxBridgeLoadDataSet), the titles are how the QML dataset selections refer to them.
	clearNativeStateNative();

	Rcpp::IntegerVector dataSetIds(datasets.size());

	for (int i = 0; i < datasets.size(); i++)
	{
		const std::string name = Rcpp::as<std::string>(names[i]);
		const SyntaxBridgeDataSet& dataset = DataFrameImporter::loadDataFrame(
				Rcpp::as<Rcpp::List>(datasets[i]), name);

		const int dataSetId = callBridgeOrStop("syntaxBridgeLoadDataSet", [&]() {
			return syntaxBridgeLoadDataSet(&dataset, global_param_dbInMemory, global_param_threshold, global_param_orderLabelsByValue);
		});

		if (dataSetId < 0)
			Rcpp::stop("the bridge did not report a dataset id for '%s'", name.c_str());

		dataSetIds[i] = dataSetId;
	}

	dataSetIds.names() = names;
	return dataSetIds;
}


// [[Rcpp::export]]
String loadQmlAndParseOptions(String moduleName, String analysisName, String qmlFile, String options, String version, bool preloadData)
{
	std::string qmlFileStr		= qmlFile.get_cstring(),
				optionsStr		= options.get_cstring(),
				versionStr		= version.get_cstring(),
				analysisNameStr	= analysisName.get_cstring(),
				moduleNameStr	= moduleName.get_cstring();

	Json::Value status = parseBridgeJsonOrStop(
		callBridgeOrStop("syntaxBridgeLoadQmlAndParseOptionsStatus", [&]() {
			return syntaxBridgeLoadQmlAndParseOptionsStatus(moduleNameStr.c_str(), analysisNameStr.c_str(), qmlFileStr.c_str(), optionsStr.c_str(), versionStr.c_str(), preloadData);
		}),
		"syntaxBridgeLoadQmlAndParseOptionsStatus"
	);

	if (!status["ok"].asBool())
	{
		std::string error = status.isMember("error") ? status["error"].asString() : "unknown error";
		Rcpp::stop("Error when parsing options: %s", error.c_str());
	}

	static std::string result;
	result = status["options"].toStyledString();
	return result.c_str();
}

//' Parse options through the QML form and return the FULL bridge status in pieces:
//' list(options = <json string>, multiDataSetJson = <json string or ""> ) -
//' multiDataSetJson ({ ids, names }) is present for multi-dataset aware analyses and pairs
//' with the per-dataset slices queued in the bridge (jaspBase::runJaspResults reads them via
//' .readDataSetRequestedNative). Strings all the way through: no R-side json re-serialization.
// [[Rcpp::export]]
List loadQmlAndParseOptionsStatus(String moduleName, String analysisName, String qmlFile, String options, String version, bool preloadData)
{
	std::string qmlFileStr		= qmlFile.get_cstring(),
				optionsStr		= options.get_cstring(),
				versionStr		= version.get_cstring(),
				analysisNameStr	= analysisName.get_cstring(),
				moduleNameStr	= moduleName.get_cstring();

	Json::Value status = parseBridgeJsonOrStop(
		callBridgeOrStop("syntaxBridgeLoadQmlAndParseOptionsStatus", [&]() {
			return syntaxBridgeLoadQmlAndParseOptionsStatus(moduleNameStr.c_str(), analysisNameStr.c_str(), qmlFileStr.c_str(), optionsStr.c_str(), versionStr.c_str(), preloadData);
		}),
		"syntaxBridgeLoadQmlAndParseOptionsStatus"
	);

	if (!status["ok"].asBool())
	{
		std::string error = status.isMember("error") ? status["error"].asString() : "unknown error";
		Rcpp::stop("Error when parsing options: %s", error.c_str());
	}

	List result;
	result["options"]			= status["options"].toStyledString();
	result["multiDataSetJson"]	= status.isMember("multiDataSetJson") ? status["multiDataSetJson"].toStyledString() : std::string("");
	return result;
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

// [[Rcpp::export]]
String columnEncoderContextNative()
{
	return callBridgeOrStop("syntaxBridgeColumnEncoderContext", []() {
		return syntaxBridgeColumnEncoderContext();
	});
}

// [[Rcpp::export]]
Rcpp::CharacterVector decodeColumnTextNative(Rcpp::CharacterVector values, String encoderContextJson)
{
	Json::Value input(Json::arrayValue);
	for (R_xlen_t i = 0; i < values.size(); ++i)
	{
		Rcpp::String value = values[i];
		if (value == NA_STRING)
			input.append(Json::Value());
		else
			input.append(std::string(value.get_cstring()));
	}

	const std::string inputJson = input.toStyledString();
	const std::string contextJson = std::string(encoderContextJson.get_cstring());
	Json::Value decoded = parseBridgeJsonOrStop(
		callBridgeOrStop("syntaxBridgeDecodeColumnText", [&]() {
			return syntaxBridgeDecodeColumnText(inputJson.c_str(), contextJson.c_str());
		}),
		"syntaxBridgeDecodeColumnText"
	);

	if (decoded.isObject() && decoded.isMember("ok") && !decoded["ok"].asBool())
	{
		std::string error = decoded.isMember("error") ? decoded["error"].asString() : "unknown error";
		Rcpp::stop("syntaxBridgeDecodeColumnText failed: %s", error.c_str());
	}

	if (!decoded.isArray())
		Rcpp::stop("syntaxBridgeDecodeColumnText returned a non-array JSON value.");

	Rcpp::CharacterVector result(decoded.size());
	for (Json::ArrayIndex i = 0; i < decoded.size(); ++i)
	{
		if (decoded[i].isNull())
			result[i] = NA_STRING;
		else if (decoded[i].isString())
			result[i] = decoded[i].asCString();
		else
			Rcpp::stop("syntaxBridgeDecodeColumnText returned a non-string value.");
	}

	return result;
}




//' Decode a Whole JSON Payload Against Every Bridge Encoder
//'
//' Runs the engine's send-string equivalent over a JSON payload (an analysis' results
//' json, say): each DataSet's own column encoder in the bridge workspace, then the
//' extra-options encoder, then the process-global one. An encoder only replaces the exact
//' tokens it minted itself, so decoding with all of them is safe for multi-dataset runs
//' regardless of which dataset was shown last. Text that is not valid JSON is returned
//' unchanged; empty input is returned as-is.
// [[Rcpp::export]]
String decodeJsonText(String json)
{
	if (json == NA_STRING)
		return json;

	const std::string jsonStr = json.get_cstring();
	if (jsonStr.empty())
		return json;

	Json::Value decoded = parseBridgeJsonOrStop(
		callBridgeOrStop("syntaxBridgeDecodeJsonText", [&]() {
			return syntaxBridgeDecodeJsonText(jsonStr.c_str());
		}),
		"syntaxBridgeDecodeJsonText"
	);

	if (!decoded.isObject() || !decoded.isMember("ok") || !decoded["ok"].isBool())
		Rcpp::stop("syntaxBridgeDecodeJsonText returned an unexpected payload.");

	if (!decoded["ok"].asBool())
	{
		std::string error = decoded.isMember("error") ? decoded["error"].asString() : "unknown error";
		Rcpp::stop("syntaxBridgeDecodeJsonText failed: %s", error.c_str());
	}

	if (!decoded.isMember("text") || !decoded["text"].isString())
		Rcpp::stop("syntaxBridgeDecodeJsonText returned no decoded text.");

	return std::string(decoded["text"].asString());
}
