test_that("to_lcov works with coverage objects", {
  tmp <- tempfile()
  cov <- package_coverage(test_path("TestSummary"))

  to_lcov(cov, filename = tmp)

  lines <- readLines(tmp)
  
  # Check if file starts with SF: and contains DA: lines
  expect_match(lines, "^SF:", all = FALSE)
  expect_match(lines, "^DA:", all = FALSE)
  expect_match(lines, "^end_of_record$", all = FALSE)
})

test_that("to_lcov outputs correct format and content", {
  tmp <- withr::local_tempfile()
  
  src <- withr::local_tempfile(fileext = ".R")
  writeLines(
    c(
      "f <- function(x) {",
      "  if (x > 0) {",
      "    return(x)",
      "  }",
      "  return(0)",
      "}"
    ),
    src
  )
  
  test <- withr::local_tempfile(fileext = ".R")
  writeLines(c("source(src_file)", "f(1)"), test)
  
  env <- new.env()
  env$src_file <- src
  
  writeLines("f(1)", test)
  
  cov <- file_coverage(src, test)
  
  to_lcov(cov, filename = tmp)
  
  lines <- readLines(tmp)
  
  # Expect SF: followed by the path of src
  # covr usually normalizes paths
  normalized_src <- normalizePath(src, mustWork = FALSE)
  expect_match(lines, paste0("SF:", normalized_src), all = FALSE)
  
  # Expect DA lines for lines in the file
  # The exact lines depend on how R parses and covr instruments
  # But we expect at least some DA lines
  expect_match(lines, "^DA:", all = FALSE)
  
  # Verify end of record
  expect_true(any(lines == "end_of_record"))
})
