
#' @export
matrix_identity <- function(n, .names = NULL) {
  out <- setup_matrix(n, .names)
  out
}

#' @export
matrix_compound_symmetric <- function(n, rho = 0, .names = NULL) {
  out <- setup_matrix(n, .names)
  out <- out * (1 - rho) + matrix(rho, n, n)
  out
}

#' @export
matrix_AR1 <- function(n, rho = 0, .names = NULL) {
  out <- setup_matrix(n, .names, diag = FALSE)
  exponent <- abs(matrix(1:n - 1, nrow = n, ncol = n, byrow = TRUE) -
                    (1:n - 1))
  out * rho^exponent
}

setup_matrix <- function(n, .names = NULL, diag = TRUE) {
  stopifnot(n %% 1 == 0)
  if(diag) {
    out <- diag(n)
  } else {
    out <- matrix(1, n, n)
  }
  dimnames(out) <- list(.names, .names)
  out
}
