context("dataset selection syntax surface")

# The contract the bridge's raw-option translation implements: selection options carry human
# strings only - the bare dataset TITLE for a dataset's default filter, "title - filterName"
# for a saved named filter (freshly loaded datasets have only the default, so in full R such a
# reference must fail loudly). Exact lookup, never parsing, never ids: a number is only ever a
# title that happens to read as one (M4). loadDataSet()/loadDataSets() return their ids as
# INFORMATIONAL values (docstrings say so), invisibly since s2.

localSelectionBridge <- function() {
  tryCatch(
    {
      jaspSyntax::clearNativeState()
      jaspSyntax::setParameter("verbose", "none")
      invisible(TRUE)
    },
    error = function(e) {
      testthat::skip(paste0("Native SyntaxInterface bridge unavailable: ", conditionMessage(e)))
    }
  )
}

selectionQml <- function() testthat::test_path("fixtures", "minimalModule", "inst", "qml", "SelectionAnalysis.qml")

parseSelectionJson <- function(optionsJson) {
  jaspSyntax::loadQmlAndParseOptionsStatus("minimalModule", "selectionFunc", selectionQml(),
                                           optionsJson, "1", TRUE)
}

parseSelection <- function(sel) {
  parseSelectionJson(as.character(jsonlite::toJSON(list(dataSetA = sel), auto_unbox = TRUE)))
}

test_that("selections resolve by title, and title alone", {
  localSelectionBridge()
  on.exit(jaspSyntax::clearNativeState(), add = TRUE)

  ids <- jaspSyntax::loadDataSets(list(Alpha = data.frame(x = 1:3), B = data.frame(y = 1:4)))
  expect_true(all(ids > 0L))
  expect_identical(names(ids), c("Alpha", "B"))

  st <- parseSelection("Alpha")
  expect_true(nzchar(st$multiDataSetJson))

  # the docstring example is exactly this: selection == the list name, not the returned id
  parsed <- jsonlite::fromJSON(st$options, simplifyVector = FALSE)
  keyA   <- as.character(utils::tail(unlist(parsed$dataSetA), 1L))
  msj    <- jsonlite::fromJSON(st$multiDataSetJson)
  expect_true(keyA %in% as.character(msj$ids))
  expect_equal(as.character(msj$dataSetIds[[keyA]]), as.character(ids[["Alpha"]]))
})

test_that("a numeric dataset title is a title, never an id (no id magic)", {
  localSelectionBridge()
  on.exit(jaspSyntax::clearNativeState(), add = TRUE)

  ids <- jaspSyntax::loadDataSets(list("42" = data.frame(x = 1:3), B = data.frame(y = 1:4)))
  st  <- parseSelection("42")
  expect_true(nzchar(st$multiDataSetJson))

  parsed  <- jsonlite::fromJSON(st$options, simplifyVector = FALSE)
  key42   <- as.character(utils::tail(unlist(parsed$dataSetA), 1L))
  msj     <- jsonlite::fromJSON(st$multiDataSetJson)
  expect_equal(as.character(msj$dataSetIds[[key42]]), as.character(ids[["42"]]),
               info = "\"42\" must select the dataset TITLED 42")

  # and the OTHER dataset's real id is rejected - the id never shadows, and is never used:
  expect_error(parseSelection(as.character(ids[["B"]])), "matches no dataset selection string")
})

test_that("unknown, named-filter and empty selections fail loudly with actionable messages", {
  localSelectionBridge()
  on.exit(jaspSyntax::clearNativeState(), add = TRUE)

  invisible(jaspSyntax::loadDataSets(list(Alpha = data.frame(x = 1:3), B = data.frame(y = 1:4))))

  # fresh loads have no named filters: 'title - name' cannot resolve and says so, listing options
  expect_error(parseSelection("Alpha - myfilter"), "matches no dataset selection string")
  expect_error(parseSelection("Alpha:myfilter"), "matches no dataset selection string")

  err <- tryCatch({ parseSelection("NoSuchDataSet"); "" }, error = conditionMessage)
  expect_match(err, "available:")
  expect_match(err, "Alpha")          # the listing names what IS loaded

  expect_error(parseSelection(""), "must name one of the loaded datasets")

  # old wrapper default shape {types,value} with an empty inside: same loud empty error
  shapedEmpty <- as.character(jsonlite::toJSON(
    list(dataSetA = list(types = "unknown", value = "")), auto_unbox = TRUE))
  expect_error(parseSelectionJson(shapedEmpty), "must name one of the loaded datasets")

  # ... and with a plain title inside it unwraps (checked in wrappers keep parsing)
  shaped <- as.character(jsonlite::toJSON(
    list(dataSetA = list(types = "unknown", value = "Alpha")), auto_unbox = TRUE))
  expect_true(nzchar(parseSelectionJson(shaped)$multiDataSetJson))

  # ... with a NON-plain inside it is rejected with the direct-title hint (M9)
  badShape <- as.character(jsonlite::toJSON(
    list(dataSetA = list(types = "unknown", value = list("Alpha", "B"))), auto_unbox = TRUE))
  expect_error(parseSelectionJson(badShape), "pass the dataset title directly")
})

test_that("loadDataSet returns its id invisibly (s2) and positively", {
  localSelectionBridge()
  on.exit(jaspSyntax::clearNativeState(), add = TRUE)

  seen <- withVisible(jaspSyntax::loadDataSet(data.frame(x = 1:3)))
  expect_false(seen$visible) # no "[1] <id>" noise
  expect_type(seen$value, "integer")
  expect_true(seen$value > 0L)
})

# ---- TWO per-list selection options in one form ------------------------------
# Selection lives on the LISTS; the host registry (D2.3) is what the bridge
# reads, so every selection option of the form - not just a form-level one - must translate
# before anything binds. Fixture: SelectionTwoListsAnalysis.qml, the self-contained twin of the
# jaspTestModule zoo form (testMultiDataSet.qml, gated test below).

twoListsQml <- function() testthat::test_path("fixtures", "minimalModule", "inst", "qml", "SelectionTwoListsAnalysis.qml")

parseTwoListsJson <- function(optionsJson) {
  jaspSyntax::loadQmlAndParseOptionsStatus("minimalModule", "selectionTwoListsFunc", twoListsQml(),
                                           optionsJson, "1", TRUE)
}

metaProvenanceKeys <- function(parsed) {
  meta <- parsed[[".meta"]]
  metaText <- jsonlite::toJSON(if (is.null(meta)) list() else meta, auto_unbox = TRUE)
  sum(lengths(regmatches(metaText, gregexpr("dataSetId|filterId", metaText))))
}

test_that("two per-list selection options in one form resolve independently (D8.4)", {
  localSelectionBridge()
  on.exit(jaspSyntax::clearNativeState(), add = TRUE)

  ids <- jaspSyntax::loadDataSets(list(Alpha = data.frame(x = 1:3, y = 4:6),
                                       B     = data.frame(x = 10:13, y = 20:23)))
  expect_identical(names(ids), c("Alpha", "B"))

  st <- parseTwoListsJson(as.character(jsonlite::toJSON(list(
    dataSetA   = "Alpha",
    dataSetB   = "B",
    dependentA = "x",
    dependentB = "x"      # same column NAME in both datasets: routing must still differ
  ), auto_unbox = TRUE)))
  expect_true(is.character(st$options) && nzchar(st$options))

  parsed <- jsonlite::fromJSON(st$options, simplifyVector = FALSE)
  msj    <- jsonlite::fromJSON(st$multiDataSetJson)

  keyA <- as.character(utils::tail(unlist(parsed$dataSetA), 1L))
  keyB <- as.character(utils::tail(unlist(parsed$dataSetB), 1L))
  expect_true(keyA %in% as.character(msj$ids))
  expect_true(keyB %in% as.character(msj$ids))
  expect_false(keyA == keyB, "the two selections are two distinct slices")

  expect_equal(as.character(msj$dataSetIds[[keyA]]), as.character(ids[["Alpha"]]))
  expect_equal(as.character(msj$dataSetIds[[keyB]]), as.character(ids[["B"]]))

  # the same-name column encodes per dataset (the D6.3 distinct-encodings contract, bridge-side)
  encA <- as.character(utils::tail(unlist(parsed$dependentA), 1L))
  encB <- as.character(utils::tail(unlist(parsed$dependentB), 1L))
  expect_match(encA, "^JASPColumn_D[0-9]+_F[0-9]+_[0-9]+_Encoded$")
  expect_match(encB, "^JASPColumn_D[0-9]+_F[0-9]+_[0-9]+_Encoded$")
  expect_false(encA == encB)

  # C8/C11: the parse result R receives carries no provenance - no sidecar, no provenance keys
  expect_null(parsed[[".filterIds"]])
  expect_equal(metaProvenanceKeys(parsed), 0L)
})

test_that("each selection option is validated on its own; the error names the option (D8.4)", {
  localSelectionBridge()
  on.exit(jaspSyntax::clearNativeState(), add = TRUE)

  invisible(jaspSyntax::loadDataSets(list(Alpha = data.frame(x = 1:3), B = data.frame(y = 1:4))))

  err <- tryCatch({
    parseTwoListsJson(as.character(jsonlite::toJSON(list(
      dataSetA = "Alpha", dataSetB = "NoSuchDataSet"), auto_unbox = TRUE)))
    ""
  }, error = conditionMessage)
  expect_match(err, "dataSetB")
  expect_match(err, "available:")
  expect_match(err, "Alpha")

  # an empty SECOND selection is just as loud as an empty first one
  err <- tryCatch({
    parseTwoListsJson(as.character(jsonlite::toJSON(list(
      dataSetA = "Alpha"), auto_unbox = TRUE)))
    ""
  }, error = conditionMessage)
  expect_match(err, "dataSetB")
})

test_that("the zoo form (two per-list selections, the D8.4 fixture) parses through the bridge", {
  localSelectionBridge()
  on.exit(jaspSyntax::clearNativeState(), add = TRUE)

  zooQml <- testthat::test_path("..", "..", "..", "jaspTestModule", "inst", "qml", "testMultiDataSet.qml")
  skip_if(!file.exists(zooQml), "jaspTestModule checkout not available next to jaspSyntax")

  ids <- jaspSyntax::loadDataSets(list(Alpha = iris, Beta = iris))
  st <- jaspSyntax::loadQmlAndParseOptionsStatus("jaspTestModule", "multiDataSetFunc", zooQml,
    as.character(jsonlite::toJSON(list(
      dataSetA = "Alpha", dataSetB = "Beta",
      dependentA = "Sepal.Length", dependentB = "Sepal.Length"), auto_unbox = TRUE)),
    "1", TRUE)
  expect_true(is.character(st$options) && nzchar(st$options))

  parsed <- jsonlite::fromJSON(st$options, simplifyVector = FALSE)
  msj    <- jsonlite::fromJSON(st$multiDataSetJson)
  expect_equal(length(msj$ids), 2L)
  expect_equal(sort(as.character(unlist(msj$dataSetIds))),
               sort(as.character(c(ids[["Alpha"]], ids[["Beta"]]))))
  expect_null(parsed[[".filterIds"]])
  expect_equal(metaProvenanceKeys(parsed), 0L)
})
