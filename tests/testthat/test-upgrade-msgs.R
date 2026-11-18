context("options of an older version")

# Options written for an older version of a module are upgraded by its Upgrades.qml before the
# form parses them (jasp-desktop#6414). What that upgrade has to say, and the options that the
# form does not know, must reach R: as warnings, and in the status of loadQmlAndParseOptionsStatus.

upgradeQml <- function() testthat::test_path("fixtures", "upgradeModule", "inst", "qml", "UpgradeAnalysis.qml")

parseUpgrade <- function(options, version, parser = jaspSyntax::loadQmlAndParseOptionsStatus) {
  warnings <- character()
  result   <- withCallingHandlers(
    parser("upgradeModule", "UpgradeAnalysis", upgradeQml(),
           as.character(jsonlite::toJSON(options, auto_unbox = TRUE)), version, TRUE),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(result = result, warnings = warnings)
}

localUpgradeBridge <- function() {
  jaspSyntax:::cleanUp()
  jaspSyntax::setParameter("verbose", "none")
  jaspSyntax::loadDataSet(data.frame(x = c(1.1, 2.2, 3.3), y = c(3.2, 2.1, 1.3)))

  # A SyntaxInterface from before jasp-desktop#6414 knows nothing about versions
  probe <- parseUpgrade(list(number = 5), "0.1")
  if (is.null(probe$result$upgradeMsgs) || !nzchar(probe$result$upgradeMsgs))
    testthat::skip("This SyntaxInterface does not upgrade options")
}

test_that("the messages of an upgrade are warnings, and part of the status", {
  localUpgradeBridge()
  on.exit(jaspSyntax:::cleanUp(), add = TRUE)

  parsed  <- parseUpgrade(list(number = 5, vars = "x"), "0.1")
  options <- jsonlite::fromJSON(parsed$result$options, simplifyVector = FALSE)

  expect_equal(options$count, 5)
  expect_equal(options$extra, 3)
  expect_length(options$variables, 1L) # the single string of the renamed list option became a list of one
  expect_null(options$number)

  expect_setequal(parsed$warnings, c("The options were upgraded", "extra: extra is new in 0.2"))

  msgs <- jsonlite::fromJSON(parsed$result$upgradeMsgs, simplifyVector = FALSE)
  expect_setequal(names(msgs), c("", "extra"))
  expect_equal(msgs[[which(names(msgs) == "")]], list("The options were upgraded")) # [[""]] never matches a name
  expect_equal(msgs[["extra"]],                  list("extra is new in 0.2"))
})

test_that("loadQmlAndParseOptions gives the same warnings", {
  localUpgradeBridge()
  on.exit(jaspSyntax:::cleanUp(), add = TRUE)

  parsed  <- parseUpgrade(list(number = 5), "0.1", parser = jaspSyntax::loadQmlAndParseOptions)
  options <- jsonlite::fromJSON(as.character(parsed$result), simplifyVector = FALSE)

  expect_equal(options$count, 5)
  expect_setequal(parsed$warnings, c("The options were upgraded", "extra: extra is new in 0.2"))
})

test_that("an option that the form does not know is a warning", {
  localUpgradeBridge()
  on.exit(jaspSyntax:::cleanUp(), add = TRUE)

  parsed <- parseUpgrade(list(count = 2, conut = 5), "0.2")

  expect_length(parsed$warnings, 1L)
  expect_match(parsed$warnings, "conut", fixed = TRUE)
  expect_equal(jsonlite::fromJSON(parsed$result$options)$count, 2)
})

test_that("options of the current version give no warning", {
  localUpgradeBridge()
  on.exit(jaspSyntax:::cleanUp(), add = TRUE)

  parsed <- parseUpgrade(list(count = 2, variables = "y"), "0.2")

  expect_length(parsed$warnings, 0L)
  expect_identical(parsed$result$upgradeMsgs, "")
})
