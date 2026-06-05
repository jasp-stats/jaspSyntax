context("native column encoder context")

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

contextColumnNames <- function(context) {
  vapply(context$columns, function(column) column$name, character(1L))
}

contextDecodeExpectation <- function(context) {
  columnNames <- contextColumnNames(context)
  c(
    columnNames[[1L]],
    columnNames[[2L]],
    paste0("model uses ", columnNames[[1L]], " and ", columnNames[[2L]])
  )
}

loadContextDataset <- function(dataset) {
  jaspSyntax::clearNativeState()
  jaspSyntax::loadDataSet(dataset)
  jaspSyntax::columnEncoderContext()
}

test_that("native decoder contexts can be interleaved without mutating live state", {
  localNativeColumnBridge()
  on.exit(jaspSyntax::clearNativeState(), add = TRUE)

  datasets <- list(
    alpha = data.frame(
      alpha_group = c("a", "b", "a"),
      alpha_score = c(1.1, 2.2, 3.3),
      check.names = FALSE
    ),
    beta = data.frame(
      beta_group = c("x", "y", "x"),
      beta_score = c(10.5, 11.5, 12.5),
      check.names = FALSE
    ),
    gamma = data.frame(
      gamma_group = c("left", "right", "left"),
      gamma_score = c(-1.25, 0.5, 1.75),
      check.names = FALSE
    )
  )

  contexts <- lapply(datasets, loadContextDataset)
  for (datasetName in names(datasets)) {
    expect_equal(sort(contextColumnNames(contexts[[datasetName]])), names(datasets[[datasetName]]))
  }

  jaspSyntax::clearNativeState()
  jaspSyntax::loadDataSet(datasets$alpha)
  liveContext <- jaspSyntax::columnEncoderContext()

  tokens <- c(
    "JaspColumn_0_Encoded",
    "JaspColumn_3_Encoded",
    "model uses JaspColumn_0_Encoded and JaspColumn_3_Encoded"
  )
  liveExpected <- contextDecodeExpectation(liveContext)
  expect_equal(jaspSyntax::decodeColumnText(tokens), liveExpected)

  interleavedContexts <- contexts[c("beta", "gamma", "alpha", "beta", "alpha", "gamma")]
  for (context in interleavedContexts) {
    expect_equal(
      jaspSyntax::decodeColumnText(tokens, context),
      contextDecodeExpectation(context)
    )
    expect_equal(jaspSyntax::decodeColumnText(tokens), liveExpected)
  }

  restoredContext <- jaspSyntax::columnEncoderContext()
  expect_equal(restoredContext$columns, liveContext$columns)
  expect_equal(restoredContext$extra, liveContext$extra)
  expect_equal(jaspSyntax::decodeColumnText(tokens), liveExpected)
})

test_that("native decoder C API reports malformed contexts without corrupting live state", {
  localNativeColumnBridge()
  on.exit(jaspSyntax::clearNativeState(), add = TRUE)

  dataset <- data.frame(
    api_group = c("control", "treatment"),
    api_score = c(1.25, 2.5),
    check.names = FALSE
  )
  jaspSyntax::loadDataSet(dataset)

  rawContext <- jaspSyntax:::columnEncoderContextNative()
  parsedContext <- jsonlite::fromJSON(rawContext, simplifyVector = FALSE)
  expect_equal(parsedContext$version, 1L)
  expect_equal(sort(vapply(parsedContext$columns, `[[`, character(1L), "name")), names(dataset))
  expect_equal(parsedContext$extra, list())

  tokens <- c("JaspColumn_0_Encoded", "JaspColumn_3_Encoded")
  liveBefore <- jaspSyntax::decodeColumnText(tokens)

  expect_error(
    jaspSyntax:::decodeColumnTextNative(tokens, "{not valid context json"),
    "Could not parse column encoder context JSON",
    fixed = TRUE
  )

  expect_equal(jaspSyntax::decodeColumnText(tokens), liveBefore)
})
