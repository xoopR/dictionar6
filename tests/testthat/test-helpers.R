test_that("assert_named_list", {
    expect_error(assert_named_list(1), "must be a list")
    expect_error(assert_named_list(list(1)), "'x' must have unique")
    expect_error(assert_named_list(list(a = 1, a = 2)), "'x' must have unique")
})
