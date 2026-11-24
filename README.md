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

Every full build points `configure` at the new release on the git tag of that
release, so `jasp-stats/jaspSyntax@<tag>` gives R code and libraries that belong
together. A build of jasp-desktop `development` also makes it the default of
`configure` on the branch the workflow runs on (`update_configure: auto`; `yes`
or `no` override that). The release gets a `jasp-version.json`: the jaspSyntax
ref, the jasp-desktop commit and the jaspBase commit of that JASP version.

For a JASP version that module unit tests should run against (e.g. `v0.99.1`),
run the workflow with `jasp_desktop_ref` set to the jasp-desktop tag of that
version and `release_tag` set to the same name. Run it from `master`, or from a
`release/<version>` branch when the R code of master no longer fits that
version. `master` keeps downloading the development libraries.

`jaspTools::setupJaspTools(jaspVersion = "<tag>")` reads that file to install
those packages.
