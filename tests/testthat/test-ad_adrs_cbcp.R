# ad_adrs.R and ad_adrs_basic.R templates ----
run_adrs_template <- function(template_file) {
  template_path <- system.file("templates", template_file, package = "admiralonco")
  lines <- readLines(template_path)
  save_idx <- grep("^# Save output", lines)
  if (length(save_idx) > 0L) {
    lines <- lines[seq_len(save_idx[1L] - 1L)]
  }

  had_worst_resp <- exists("worst_resp", envir = globalenv(), inherits = FALSE)
  old_worst_resp <- if (had_worst_resp) {
    get("worst_resp", envir = globalenv(), inherits = FALSE)
  }
  on.exit(
    if (had_worst_resp) {
      assign("worst_resp", old_worst_resp, envir = globalenv())
    } else if (exists("worst_resp", envir = globalenv(), inherits = FALSE)) {
      rm("worst_resp", envir = globalenv())
    },
    add = TRUE
  )

  run_env <- new.env(parent = globalenv())
  exprs_parsed <- parse(text = lines)
  for (i in seq_along(exprs_parsed)) {
    suppressMessages(eval(exprs_parsed[[i]], envir = run_env))
    if (exists("worst_resp", envir = run_env, inherits = FALSE)) {
      assign(
        "worst_resp",
        get("worst_resp", envir = run_env, inherits = FALSE),
        envir = globalenv()
      )
    }
  }
  run_env$adrs
}

## Test 1: CBCP matches CBOR CR/PR in ad_adrs.R template ----
test_that("ad_adrs.R template Test 1: CBCP is Y for subjects with CBOR of CR or PR", {
  adrs <- run_adrs_template("ad_adrs.R")

  cbor_resp <- adrs %>%
    filter(PARAMCD == "CBOR" & AVALC %in% c("CR", "PR")) %>%
    pull(USUBJID)
  cbcp_resp <- adrs %>%
    filter(PARAMCD == "CBCP" & AVALC == "Y") %>%
    pull(USUBJID)

  expect_true(length(cbor_resp) > 0L)
  expect_setequal(cbcp_resp, cbor_resp)
})

## Test 2: CBCP matches CBOR CR/PR in ad_adrs_basic.R template ----
test_that("ad_adrs_basic.R template Test 2: CBCP is Y for subjects with CBOR of CR or PR", {
  adrs <- run_adrs_template("ad_adrs_basic.R")

  cbor_resp <- adrs %>%
    filter(PARAMCD == "CBOR" & AVALC %in% c("CR", "PR")) %>%
    pull(USUBJID)
  cbcp_resp <- adrs %>%
    filter(PARAMCD == "CBCP" & AVALC == "Y") %>%
    pull(USUBJID)

  expect_true(length(cbor_resp) > 0L)
  expect_setequal(cbcp_resp, cbor_resp)
})
