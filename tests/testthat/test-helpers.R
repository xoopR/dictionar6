test_that("assert_named_list", {
    expect_error(assert_named_list(1), "must be a list")
    expect_error(assert_named_list(list(1)), "must be named")
})

test_that("as.character.R6", {
    expect_equal(as.character(R6Class("test")$new()), "test")
})
