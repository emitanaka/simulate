test_that("correlation", {
  expect_true({
    correlation_matrix(., runif(1, -1, 1), runif(1, -1, 1),
                       ., ., runif(1, -1, 1),
                       ., ., .) %>%
      isSymmetric()
  })

  expect_true({
    correlation_matrix(., ., .,
                       runif(1, -1, 1), ., .,
                       runif(1, -1, 1), runif(1, -1, 1), .) %>%
      isSymmetric()
  })

  expect_equal({
    correlation_matrix(., 0, 0.5,
                       ., ., .,
                       ., ., .)
  }, matrix(c(1, 0, 0.5, 0, 1, 0, 0.5, 0, 1), nrow = 3))

  expect_error({
    correlation_matrix(., 0, -2,
                       ., ., .,
                       ., ., .)
  })

  expect_error({
    correlation_matrix(., 0, 0.8,
                       ., ., .,
                       -0.8, ., .)
  })

  expect_error({
    correlation_matrix(., 0, 0.8,
                       ., ., .,
                       -0.8, ., ., .)
  })

  expect_equal({
    correlation_matrix(., 0, 0.5,
                       ., ., .,
                       ., ., .)
  }, matrix(c(1, 0, 0.5, 0, 1, 0, 0.5, 0, 1), nrow = 3))

  expect_equal({
    correlation_matrix(., 0, -1,
                       ., ., .,
                       ., ., .)
  }, matrix(c(1, 0, -1, 0, 1, 0, -1, 0, 1), nrow = 3))

  expect_equal({
    x <- 0.3
    correlation_matrix(., 0, x,
                       ., ., .,
                       ., ., .)
  }, matrix(c(1, 0, 0.3, 0, 1, 0, 0.3, 0, 1), nrow = 3))

  expect_equal({
    correlation_matrix(., 0, 1/9,
                       ., ., .,
                       ., ., .)
  }, matrix(c(1, 0, 1/9, 0, 1, 0, 1/9, 0, 1), nrow = 3))



  expect_equal({
    correlation_matrix(.upper = c(0.4, 0.5, 0.8, 0.1, 0.2, 0.3))
  }, matrix(c(1, 0.4, 0.5, 0.1, 0.4, 1, 0.8, 0.2, 0.5, 0.8, 1, 0.3, 0.1, 0.2, 0.3, 1), nrow = 4))

  expect_equal({
    # FIXME
    correlation_matrix(.lower = c(0.4, 0.5, 0.8, 0.1, 0.2, 0.3))
  }, matrix(c(1, 0.4, 0.5, 0.8, 0.4, 1, 0.1, 0.2, 0.5, 0.1, 1, 0.3, 0.8, 0.2, 0.3, 1), nrow = 4, byrow = TRUE))

})
