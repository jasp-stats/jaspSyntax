context("no R-side token grammar")

# The one global decode table in the native ColumnEncoder is strict: it
# resolves exactly the tokens the bridge minted and returns everything else
# raw (C4/C16). That makes any R-side pattern-matching of token spellings both
# redundant and a source of false positives - jasp-desktop MDS-PLAN C14
# deleted it ("deletion, not reformatting"). These tests pin that the grammar
# stays out of jaspSyntax.
#
# Scope note: options.R still parses Description.qml / option JSON with
# regexec/regmatches. That is QML/JSON text parsing, not column-token
# decoding, and is deliberately NOT covered by this ban - the scan targets the
# token grammar itself, not regex machinery in general.

grammarLiterals <- c(
  "JaspColumn", "JASPColumn", "jaspColumn",
  "JaspExtraOptions", "JASPExtraOptions", "jaspExtraOptions",
  "_Encoded"
)

test_that("package R sources contain no encoded-name grammar at all", {
  rDir <- test_path("..", "..", "R")
  testthat::skip_if_not(dir.exists(rDir), "package R/ sources are only present in a checkout")

  files <- list.files(rDir, pattern = "[.][Rr]$", full.names = TRUE)
  expect_gte(length(files), 5L)

  for (file in files) {
    lines <- readLines(file, warn = FALSE)
    for (token in grammarLiterals) {
      hits <- grep(token, lines, fixed = TRUE)
      expect_equal(
        length(hits), 0L,
        info = paste0(
          basename(file), " mentions token grammar `", token,
          "` at line(s): ", paste(hits, collapse = ", ")
        )
      )
    }
    # Belt and braces: no regex call may even mention "encoded" - token
    # decisions belong to the native table, never to an R pattern.
    regexCalls <- grep("(grepl|regexpr|gregexpr|regexec|regmatches|gsub|sub)\\s*\\(", lines)
    encodedMentions <- grep("(?i)encoded", lines, perl = TRUE)
    expect_equal(
      length(intersect(regexCalls, encodedMentions)), 0L,
      info = paste0(
        basename(file), " has regex call(s) at line(s) ",
        paste(intersect(regexCalls, encodedMentions), collapse = ", "),
        " mentioning an encoded-token pattern"
      )
    )
  }
})

test_that("the installed namespace exposes no regex token detectors", {
  ns <- asNamespace("jaspSyntax")

  # The two grammar helpers deleted in D1.8 (and their D1.2/D1.3 bridge-tweak
  # alternation) must not come back, reachable or not.
  expect_false(exists(".containsEncodedBridgeColumnTokens", envir = ns, inherits = FALSE))
  expect_false(exists(".isEncodedBridgeColumnName", envir = ns, inherits = FALSE))

  # Deparse everything reachable in the namespace: the grammar strings above
  # must not survive inside any function body or constant either (this is the
  # scan that also works when tests run against an installed package).
  objects <- mget(ls(ns, all.names = TRUE), envir = ns)
  texts <- vapply(objects, function(object) paste(deparse(object), collapse = "\n"), character(1L))
  hits <- names(texts)[vapply(
    grammarLiterals,
    function(token) any(grepl(token, texts, fixed = TRUE)),
    logical(1L)
  )]
  expect_equal(length(hits), 0L, info = paste(hits, collapse = ", "))
})

localNativeColumnBridge <- function() {
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

test_that("non-token text survives the native decode sweep unchanged", {
  localNativeColumnBridge()
  on.exit(jaspSyntax::clearNativeState(), add = TRUE)

  jaspSyntax::loadDataSet(data.frame(
    score = c(1.5, 2.5),
    group = factor(c("a", "b")),
    check.names = FALSE
  ))

  # The table sweep is a strict lookup: legacy spellings, D-shaped tokens that
  # were never minted, and ordinary text all pass through byte-identical
  # (this replaces every decision the deleted regex detectors used to make).
  plain <- c(
    "hello world",
    "The following variables are used: 'x', 'y'.",
    "score.scale",
    "group.nominal",
    "JaspColumn_1_Encoded",
    "jaspColumn2",
    "JASPColumn_D99_F99_9_Encoded",
    "JaspExtraOptions_0_Encoded",
    "",
    NA_character_
  )
  expect_equal(jaspSyntax::decodeColumnText(plain), plain)
  expect_equal(jaspSyntax::decodeColumnNames(plain), plain)
  expect_equal(jaspSyntax::decodeColumnNames(plain, strict = TRUE), plain)

  # Real tokens minted by the live bridge still round-trip through the very
  # same call - the sweep decodes what it knows and nothing else. (Token
  # source: the raw loaded-dataset names, cf. test-native-column-context.R.)
  dataset <- data.frame(score = c(1.5, 2.5), group = factor(c("a", "b")), check.names = FALSE)
  rawNames <- names(jaspSyntax::readLoadedDataset(decode = FALSE))
  expect_true(all(grepl("^JASPColumn_D[0-9]+_F[0-9]+_[0-9]+_Encoded$", rawNames)))
  expect_setequal(jaspSyntax::decodeColumnNames(rawNames), names(dataset))

  token <- rawNames[[1L]]
  decodedToken <- jaspSyntax::decodeColumnNames(token)
  expect_true(decodedToken %in% names(dataset))
  expect_equal(
    jaspSyntax::decodeColumnText(c("plain text", token, "more plain text")),
    c("plain text", decodedToken, "more plain text")
  )
})
