test_that("correlation", {
  cormat(., 0, 0.5,
         ., ., .,
         ., ., .)

  x <- 0.3
  cormat(., ., x,
         ., ., .,
         ., ., .)

  cormat(., ., 1/9, .,
         ., ., ., .,
         ., ., ., .,
         ., ., ., .)
})
