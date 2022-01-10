
#' Constructors for structured correlation and covariance matrix
#'
#' Correlation and covariance matrices are assumed to be symmetrics, semi-positive
#' definite. In the case of correlation matrices, the diagonal elements should be 1
#' and the off-diagonal elements between -1 and 1 inclusive.
#'
#' @param n The dimension of the matrix.
#' @param rho The
#' @param vars A vector of diagonal variances or a single variance.
#' @param names Optional character vector of the same size as `n`.
#' @param inverse_return Whether to return the inverse or not.
#' @name matrix-constructors
NULL

#' @describeIn matrix-constructors Identity matrix.
#' @export
matrix_id <- function(n, vars = 1, names = NULL, inverse_return = FALSE) {
  out <- setup_matrix(n, names)
  if(length(vars)==1L) {
    scale <- ifelse(inverse_return, 1/vars, vars)
    out <- scale * out
  } else {
    scale <- diag(vars)
    if(inverse_return) scale <- diag(1/vars)
    out <- scale * out
  }
  out
}

#' @describeIn matrix-constructors A synonym for `matrix_id`.
#' @export
matrix_diag <- matrix_id

#' @describeIn matrix-constructors Compound symmetric matrix.
#' @export
matrix_cs <- function(n, rho = 0, vars = 1, names = NULL, inverse_return = FALSE) {
  out <- setup_matrix(n, names)
  out <- out * (1 - rho) + matrix(rho, n, n)
  diag(out) <- vars
  if(inverse_return) return(solve(out))
  out
}

#' @describeIn matrix-constructors First order autoregressive order matrix.
#' @export
matrix_ar1 <- function(n, rho = 0, vars = 1, names = NULL, inverse_return = FALSE) {
  out <- setup_matrix(n, names, diag = FALSE)
  exponent <- abs(matrix(1:n - 1, nrow = n, ncol = n, byrow = TRUE) -
                    (1:n - 1))
  if(length(vars)==1L) {
    out <- vars * out * rho^exponent
  } else {
    out <- diag(sqrt(vars)) %*% (out * rho^exponent) %*% diag(sqrt(vars))
  }
  if(inverse_return) return(solve(out))
  out
}

#' @describeIn matrix-constructors First order ante-dependence order matrix.
#' @export
matrix_ad1 <- function(n, u, vars = 1, names = NULL, inverse_return = FALSE) {
  U <- diag(n)
  U[matrix(c(1:(n - 1), 1:(n - 1) + 1L), ncol = 2)] <- u
  if(length(vars) == 1L) {
    out <- 1/vars * U %*% t(U)
  } else {
    out <- U %*% diag(1/vars) %*% t(U)
  }
  if(inverse_return) return(out)
  solve(out)
}

# TODO:
# fa1 - first order factor analytic
# tp - toeplitz

setup_matrix <- function(n, names = NULL, diag = TRUE) {
  stopifnot(n %% 1 == 0)
  if(diag) {
    out <- diag(n)
  } else {
    out <- matrix(1, n, n)
  }
  if(!is.null(names)) dimnames(out) <- list(names, names)
  out
}


#' Set the name of the matrix.
#'
#' @param X A matrix.
#' @param names A character vector corresponding to the variable names. For
#'   `mset_names`, it can also be a list of two character vectors with the
#'   first and second elements corresponding to row and column names
#'   respectively.
#' @family Matrix manipulators
#' @export
mset_names <- function(X, names = NULL) {
  if(is.character(names)) {
    dimnames(X) <- list(names, names)
  }
  if(is.list(names)) {
    dimnames(X) <- names
  }
  X
}

#' @describeIn mset_names Set column names only.
#' @export
mset_colnames <- function(X, names = NULL) {
  colnames(X) <- names
  X
}

#' @describeIn mset_names Set row names only.
#' @export
mset_rownames <- function(X, names = NULL) {
  rownames(X) <- names
  X
}

#' Set the name of the matrix.
#'
#' @param X A matrix.
#' @family Matrix manipulators
#' @export
mset_values <- function(X, i, j, value) {
  X[matrix(c(i, j), ncol = 2)] <- value
  X
}

mset_matrix <- function(X, rows, cols, ..., .matrix = NULL) {
  dots <- enquos(...)
  x <- which(!map_lgl(dots, is_a_dot))
  vals <- map_dbl(dots[x], eval_tidy)
  S <- matrix(vals, length(rows), length(cols), byrow = TRUE)
  X[rows, cols] <- S
  X
}

mcheck_positive_definite <- function(X) {
  eS <- eigen(X)
  if(any(eS$values < 0)) abort("The matrix is not positive definite")
  X
}
