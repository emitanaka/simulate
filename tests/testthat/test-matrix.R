test_that("matrix operators", {

  expect_equal(matrix_id(3), diag(3))
  expect_equal(matrix_id(3, vars = 1:3), diag(1:3))
  expect_equal(matrix_id(3, vars = 1:3, inverse_return = TRUE),
               diag(1/c(1:3)))
  expect_equal(matrix_id(3, names = LETTERS[1:3]),
               matrix(as.vector(diag(3)), ncol = 3,
                      dimnames = list(LETTERS[1:3], LETTERS[1:3])))

  expect_equal(matrix_cs(3, rho = 0.5),
               matrix(c(1, 0.5, 0.5,
                        0.5, 1, 0.5,
                        0.5, 0.5, 1),
                      byrow = TRUE, nrow = 3))
  expect_equal(matrix_cs(3, rho = 0.5, vars = 3),
               matrix(c(3, 0.5, 0.5,
                        0.5, 3, 0.5,
                        0.5, 0.5, 3),
                      byrow = TRUE, nrow = 3))
  expect_equal(matrix_cs(3, rho = 0.5, vars = c(1:3)),
               matrix(c(1, 0.5, 0.5,
                        0.5, 2, 0.5,
                        0.5, 0.5, 3),
                      byrow = TRUE, nrow = 3))

  expect_equal(matrix_ar1(4, rho = 0.5),
               matrix(c(1, 0.5, 0.5^2, 0.5^3,
                        0.5, 1, 0.5, 0.5^2,
                        0.5^2, 0.5, 1, 0.5,
                        0.5^3, 0.5^2, 0.5, 1),
                      byrow = TRUE, nrow = 4))
  expect_equal(matrix_ar1(4, rho = 0.5, vars = 3),
               3* matrix(c(1, 0.5, 0.5^2, 0.5^3,
                        0.5, 1, 0.5, 0.5^2,
                        0.5^2, 0.5, 1, 0.5,
                        0.5^3, 0.5^2, 0.5, 1),
                      byrow = TRUE, nrow = 4))
  expect_equal(matrix_ar1(4, rho = 0.5, vars = c(1:4)),
               diag(sqrt(1:4)) %*% matrix(c(1, 0.5, 0.5^2, 0.5^3,
                           0.5, 1, 0.5, 0.5^2,
                           0.5^2, 0.5, 1, 0.5,
                           0.5^3, 0.5^2, 0.5, 1),
                         byrow = TRUE, nrow = 4)  %*% diag(sqrt(1:4)))


  # TODO: test for ante-dependence is needed

  M <- matrix_id(3)
  M1 <- M2 <- M3 <- M
  nms <- c("A", "B", "C")
  dimnames(M1) <- list(nms, nms)
  colnames(M2) <- nms
  rownames(M3) <- nms
  M4A <- matrix(3, nrow = 3, ncol = 4)
  M4B <- matrix(3, nrow = 3, ncol = 4, dimnames = list(nms, LETTERS[1:4]))

  expect_equal(mset_names(M, nms), M1)
  expect_equal(mset_colnames(M, nms), M2)
  expect_equal(mset_rownames(M, nms), M3)
  expect_equal(mset_names(M4A, list(nms, LETTERS[1:4])), M4B)


  # TODO: other matrix operators



})
