#!/bin/bash

set -euo pipefail

HEADER_PATH="${1:-}"
BINARY_PATH="${2:-}"
SOURCE_PATH="${3:-src/syntaxfunctions.cpp}"

HEADER_ORIGIN="${SYNTAXINTERFACE_HEADER_ORIGIN:-}"
BINARY_ORIGIN="${SYNTAXINTERFACE_BINARY_ORIGIN:-}"

function usage() {
	echo "Usage: $0 <syntaxbridge_interface.h> <SyntaxInterface binary> [syntaxfunctions.cpp]" >&2
}

function print_path_context() {
	echo "  Header path: ${HEADER_PATH}" >&2
	if [ -n "${HEADER_ORIGIN}" ] && [ "${HEADER_ORIGIN}" != "${HEADER_PATH}" ]; then
		echo "  Header source: ${HEADER_ORIGIN}" >&2
	fi
	echo "  Binary path: ${BINARY_PATH}" >&2
	if [ -n "${BINARY_ORIGIN}" ] && [ "${BINARY_ORIGIN}" != "${BINARY_PATH}" ]; then
		echo "  Binary source: ${BINARY_ORIGIN}" >&2
	fi
	echo "  Native source: ${SOURCE_PATH}" >&2
}

function print_missing_symbols() {
	local FILE_PATH="$1"
	local SYMBOL

	while IFS= read -r SYMBOL; do
		[ -n "${SYMBOL}" ] && echo "    ${SYMBOL}" >&2
	done < "${FILE_PATH}"
}

function fail_with_missing_symbols() {
	local TITLE="$1"
	local DETAILS="$2"
	local MISSING_FILE="$3"

	echo "" >&2
	echo "ERROR: ${TITLE}" >&2
	print_path_context
	echo "  Missing symbols:" >&2
	print_missing_symbols "${MISSING_FILE}"
	echo "" >&2
	echo "${DETAILS}" >&2
	exit 1
}

function find_export_tool() {
	local CANDIDATE

	for CANDIDATE in dumpbin llvm-objdump objdump x86_64-w64-mingw32-objdump nm x86_64-w64-mingw32-nm; do
		if command -v "${CANDIDATE}" >/dev/null 2>&1; then
			command -v "${CANDIDATE}"
			return 0
		fi
	done

	for CANDIDATE in \
		/c/rtools46/ucrt64/bin/objdump \
		/c/rtools45/ucrt64/bin/objdump \
		/c/rtools44/ucrt64/bin/objdump \
		/c/rtools43/ucrt64/bin/objdump \
		/c/rtools42/ucrt64/bin/objdump
	do
		if [ -x "${CANDIDATE}" ]; then
			echo "${CANDIDATE}"
			return 0
		fi
	done

	return 1
}

function write_exports() {
	local TOOL_PATH="$1"
	local DLL_PATH="$2"
	local OUTPUT_PATH="$3"
	local TOOL_NAME

	TOOL_NAME="$(basename "${TOOL_PATH}")"

	case "${TOOL_NAME}" in
		dumpbin*)
			"${TOOL_PATH}" /exports "${DLL_PATH}" > "${OUTPUT_PATH}" 2>&1
			;;
		*objdump*)
			case "${DLL_PATH}" in
				*.dll|*.DLL)
					"${TOOL_PATH}" -p "${DLL_PATH}" > "${OUTPUT_PATH}" 2>&1
					;;
				*.dylib)
					# macOS's objdump exits 0 for -T on Mach-O but emits only a
					# warning with no symbol data. Use nm -g instead.
					nm -g "${DLL_PATH}" > "${OUTPUT_PATH}" 2>&1
					;;
				*)
					"${TOOL_PATH}" -T "${DLL_PATH}" > "${OUTPUT_PATH}" 2>&1 ||
						"${TOOL_PATH}" -t "${DLL_PATH}" > "${OUTPUT_PATH}" 2>&1
					;;
			esac
			;;
		*nm*)
			"${TOOL_PATH}" -g "${DLL_PATH}" > "${OUTPUT_PATH}" 2>&1
			;;
		*)
			return 1
			;;
	esac
}

if [ -z "${HEADER_PATH}" ] || [ -z "${BINARY_PATH}" ]; then
	usage
	exit 2
fi

if [ ! -f "${SOURCE_PATH}" ]; then
	echo "ERROR: Cannot verify SyntaxInterface symbols because the native source file is missing." >&2
	print_path_context
	exit 1
fi

if [ ! -f "${HEADER_PATH}" ]; then
	echo "ERROR: Cannot verify SyntaxInterface symbols because the header is missing." >&2
	print_path_context
	exit 1
fi

if [ ! -f "${BINARY_PATH}" ]; then
	echo "ERROR: Cannot verify SyntaxInterface symbols because the binary is missing." >&2
	print_path_context
	exit 1
fi

TMP_DIR="${TMPDIR:-/tmp}"
TMP_BASE="${TMP_DIR}/jaspsyntax-symbol-check.$$"
SYMBOLS_FILE="${TMP_BASE}.symbols"
MISSING_HEADER_FILE="${TMP_BASE}.missing-header"
MISSING_EXPORTS_FILE="${TMP_BASE}.missing-exports"
EXPORTS_FILE="${TMP_BASE}.exports"
trap 'rm -f "${SYMBOLS_FILE}" "${MISSING_HEADER_FILE}" "${MISSING_EXPORTS_FILE}" "${EXPORTS_FILE}"' EXIT

: > "${MISSING_HEADER_FILE}"
: > "${MISSING_EXPORTS_FILE}"

{ grep -Eho 'syntaxBridge[A-Za-z0-9_]+[[:space:]]*\(' "${SOURCE_PATH}" || true; } \
	| sed -E 's/[[:space:]]*\($//' \
	| sort -u > "${SYMBOLS_FILE}"

if [ ! -s "${SYMBOLS_FILE}" ]; then
	echo "ERROR: No SyntaxInterface bridge symbols were found in ${SOURCE_PATH}." >&2
	print_path_context
	exit 1
fi

while IFS= read -r SYMBOL; do
	if ! grep -Eq "(^|[^[:alnum:]_])${SYMBOL}[[:space:]]*\\(" "${HEADER_PATH}"; then
		echo "${SYMBOL}" >> "${MISSING_HEADER_FILE}"
	fi
done < "${SYMBOLS_FILE}"

if [ -s "${MISSING_HEADER_FILE}" ]; then
	fail_with_missing_symbols \
		"SyntaxInterface header does not declare every native bridge symbol used by jaspSyntax." \
		"The header and src/syntaxfunctions.cpp are out of sync. Use a jasp-desktop checkout whose SyntaxInterface header matches this jaspSyntax source, or remove stale generated headers and reinstall." \
		"${MISSING_HEADER_FILE}"
fi

SYMBOL_COUNT="$(wc -l < "${SYMBOLS_FILE}" | tr -d '[:space:]')"
echo "Verified ${SYMBOL_COUNT} SyntaxInterface declarations in ${HEADER_PATH}"

case "${BINARY_PATH}" in
	*.dll|*.DLL|*.so|*.dylib) ;;
	*) exit 0 ;;
esac

case "${JASPSYNTAX_CHECK_EXPORTS:-auto}" in
	0|false|FALSE|no|NO|never|NEVER)
		echo "Skipping SyntaxInterface DLL export check because JASPSYNTAX_CHECK_EXPORTS=${JASPSYNTAX_CHECK_EXPORTS}."
		exit 0
		;;
esac

EXPORT_TOOL="$(find_export_tool || true)"
if [ -z "${EXPORT_TOOL}" ]; then
	echo "ERROR: Cannot verify SyntaxInterface binary exports because dumpbin, objdump, and nm were not found." >&2
	print_path_context
	echo "" >&2
	echo "Install an export inspection tool or explicitly set JASPSYNTAX_CHECK_EXPORTS=false to bypass this ABI check." >&2
	exit 1
fi

if ! write_exports "${EXPORT_TOOL}" "${BINARY_PATH}" "${EXPORTS_FILE}"; then
	echo "ERROR: Cannot verify SyntaxInterface binary exports with ${EXPORT_TOOL}." >&2
	print_path_context
	echo "" >&2
	echo "Use a readable SyntaxInterface binary or explicitly set JASPSYNTAX_CHECK_EXPORTS=false to bypass this ABI check." >&2
	exit 1
fi

while IFS= read -r SYMBOL; do
	if ! grep -Eq "(^|[^[:alnum:]_])_?${SYMBOL}(@[0-9]+)?([^[:alnum:]_]|$)" "${EXPORTS_FILE}"; then
		echo "${SYMBOL}" >> "${MISSING_EXPORTS_FILE}"
	fi
done < "${SYMBOLS_FILE}"

if [ -s "${MISSING_EXPORTS_FILE}" ]; then
	fail_with_missing_symbols \
		"SyntaxInterface binary does not export every native bridge symbol used by jaspSyntax." \
		"The header and binary likely come from different jasp-desktop checkouts or branches. Rebuild or copy a matching SyntaxInterface binary, or point JASP_BUILD_DIR/JASPSYNTAX_LIB_DIR/JASPSYNTAX_LIB_PATH at the matching artifact." \
		"${MISSING_EXPORTS_FILE}"
fi

echo "Verified ${SYMBOL_COUNT} SyntaxInterface exports in ${BINARY_PATH} using ${EXPORT_TOOL}"
