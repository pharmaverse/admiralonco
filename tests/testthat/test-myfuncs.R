
# myfirstfunc ----
test_that("default works", {
  expect_dfs_equal(
    base = expected,
    comp = myfirstfunc(test_data)
  )
})

test_that("new_var works", {
  expect_dfs_equal(
    base = expected,
    comp = myfirstfunc(
      test_data,
      new_var = MYVAR)
  )
})

# mysecondfunc ----
test_that("default works", {
  expect_dfs_equal(
    base = expected,
    comp = mysecondfunc(test_data)
  )
})

test_that("new_var works", {
  expect_dfs_equal(
    base = expected,
    comp = mysecondfunc(
      test_data,
      new_var = MYVAR)
  )
})
