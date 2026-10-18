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
  # mds-plan D8.4: the wrapper is the host-filter-prefix-only surface {version, prefix, native};
  # the captured columns live in the opaque native payload.
  vapply(jsonlite::fromJSON(context$native, simplifyVector = FALSE)$columns,
         function(column) column$name, character(1L))
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

  # Real bridge state per dataset: the context AND the encoded tokens the dataset's columns
  # actually mint (the shown filter's view namespaces them as JASPColumn_D<dsId>_F<filterId>_<n>
  # _Encoded, mds-plan Phase 1). The ids restart after every clear, so the token strings collide
  # across these three datasets - replaying a context must resolve them in the namespace it was
  # captured in, whichever dataset is live.
  state <- lapply(datasets, function(dataset) {
    jaspSyntax::clearNativeState()
    jaspSyntax::loadDataSet(dataset)
    context <- jaspSyntax::columnEncoderContext()
    list(
      context = context,
      tokens = names(jaspSyntax::readLoadedDataset(decode = FALSE)),
      columnNames = names(dataset)
    )
  })
  for (datasetName in names(datasets)) {
    expect_equal(sort(contextColumnNames(state[[datasetName]]$context)), names(datasets[[datasetName]]))
    expect_match(state[[datasetName]]$tokens[[1L]], "^JASPColumn_D[0-9]+_F[0-9]+_[0-9]+_Encoded$")
  }

  jaspSyntax::clearNativeState()
  jaspSyntax::loadDataSet(datasets$alpha)
  liveContext <- jaspSyntax::columnEncoderContext()
  liveTokens <- state$alpha$tokens
  liveExpected <- names(datasets$alpha)

  expect_equal(jaspSyntax::decodeColumnText(liveTokens), liveExpected)

  interleavedNames <- c("beta", "gamma", "alpha", "beta", "alpha", "gamma")
  for (datasetName in interleavedNames) {
    entry <- state[[datasetName]]
    expect_equal(
      jaspSyntax::decodeColumnText(entry$tokens, entry$context),
      entry$columnNames
    )
    expect_equal(jaspSyntax::decodeColumnText(liveTokens), liveExpected)
  }

  restoredContext <- jaspSyntax::columnEncoderContext()
  # mds-plan D8.4: the wrapper carries {version, prefix, native} only - the capture payload is
  # compared, not duplicated per-column fields.
  expect_equal(restoredContext$native, liveContext$native)
  expect_equal(restoredContext$prefix, liveContext$prefix)
  expect_equal(jaspSyntax::decodeColumnText(liveTokens), liveExpected)
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
  expect_equal(parsedContext$version, 2L)
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
