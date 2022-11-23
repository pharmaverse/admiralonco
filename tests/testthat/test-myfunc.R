

test_that("default works", {
  expect_dfs_equal(
    base = expected,
    comp = myfunc(test_data)
  )
})

test_that("new_var works", {
  expect_dfs_equal(
    base = expected,
    comp = myfunc(
      test_data,
      new_var = MYVAR)
  )
})
