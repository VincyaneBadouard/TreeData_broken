test_that("CommentByRow", {

  # Import data
  library(data.table)

  # Create test data
  dt <- data.table(A = c(2), B = c(6))

  dt <- CommentByRow(dt, comment = "A = 2") # 1st comment

  Rslt <- CommentByRow(dt, comment = "B = 6") # 2nd comment

  expect_true("Comment" %in% names(Rslt))
  expect_true(Rslt$Comment %in% c("A = 2/B = 6"))

})
