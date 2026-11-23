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
`readAnalysisOptionsFromJaspFile()`, `readAnalysisResultsFromJaspFile()`, and
`readDatasetFromJaspFile()` over the
raw native bridge calls.

The lower-level helpers (`parseQmlOptions()`, lifecycle controls, dataset-state
readers, column mapping helpers, and `nativeBridgeProvenance()`) are exported
for bridge integration and diagnostics. Treat them as experimental/native-facing
APIs; ordinary callers should stay on the high-level readers above. The raw
native bridge calls follow the SyntaxInterface ABI rather than a stable R API.

## JASP versions

The native library comes from a GitHub release of this repository, built by the
`Build SyntaxInterface Libraries` workflow from a jasp-desktop ref. `configure`
downloads the release named in its `GITHUB_RELEASE_TAG` line; set the
`JASPSYNTAX_RELEASE_TAG` environment variable to install with another one.

For each JASP version that module unit tests should run against (e.g. `v0.99.1`):

1. run the workflow with `jasp_desktop_ref` set to the jasp-desktop tag of that
   version, `release_tag` set to the same name, and `update_configure` on. Run it
   from `master`, or from a `release/<version>` branch when the R code of master
   no longer fits that version.
2. The workflow then commits the tag into `configure`, moves the git tag to that
   commit, so `jasp-stats/jaspSyntax@<tag>` gives R code and libraries that belong
   together, and attaches `jasp-version.json` to the release: the jaspSyntax ref,
   the jasp-desktop commit and the jaspBase commit of that JASP version.

`jaspTools::setupJaspTools(jaspVersion = "<tag>")` reads that file to install
those packages.
