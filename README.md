jaspSyntax exposes the native JASP SyntaxInterface bridge to R.

It is the lower-level runtime layer used by JASP tooling to:

- parse module `Description.qml` metadata,
- resolve analysis QML files,
- replay QML option binding through the native Desktop option pipeline,
- load R data frames or saved `.jasp` datasets into native state,
- read saved `.jasp` analysis options as saved/QML-bound records or as
  backend/runtime options.

Saved `.jasp` options are read from the archive and then replayed through QML
when runtime options are requested. They are not a replacement for Desktop's
full archive/module upgrade workflow for old files.

Prefer the high-level helpers such as `readModuleDescription()`,
`readAnalysisOptionsFromQml()`, `readDefaultAnalysisOptions()`,
`readAnalysisOptionsFromJaspFile()`, and `readDatasetFromJaspFile()` over the
raw native bridge calls.

The lower-level helpers (`parseQmlOptions()`, lifecycle controls, dataset-state
readers, column mapping helpers, and `nativeBridgeProvenance()`) are exported
for bridge integration and diagnostics. Treat them as experimental/native-facing
APIs; ordinary callers should stay on the high-level readers above. The raw
native bridge calls follow the SyntaxInterface ABI rather than a stable R API.
