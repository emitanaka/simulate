


#' Construct correlation matrix
#'
#' Given a set of values, convert it to a correlation matrix.
#'
#' @param ... A series of values or dots for the correlation matrix. Only the
#'   value in the upper triangle or lower triangle (not both) should be used.
#'   Use `.` for value of 1 in the diagonal entries and value of 0 in off-diagonal
#'   entries. For a n x n correlation matrix, there should be n x n values.
#' @param .names The name of the variables.
#' @param .upper A vector of values for the upper triangle values
#'   filled by row (or column if `.byrow = FALSE`. If `.upper` is defined then `...`
#'   is ignored.
#' @param .byrow A logical value indicating whether the matrix is filled by
#'   row or column. The default is `TRUE`.
#' @param .check A logical value to indicate if the correlation matrix is valid
#'   or not. This essentially equates to check that the matrix is semi-positive definite.
#' @examples
#' correlation_matrix(., 0.7, .,   .,
#'                    .,   ., ., 0.3,
#'                    .,   ., .,   .,
#'                    .,   ., .,   .)
#'
#' correlation_matrix(.upper = c(0.7, 0.1, 0.3, 0.2, -0.1, -0.3))
#'
#' correlation_matrix(.upper = c(0.7, 0.1, 0.3, 0.2, -0.1, -0.3),
#'                    .byrow = FALSE)
#'
#' correlation_matrix(.upper = rep(0.2, 3), .names = c("A", "B", "C"))
#'
#' @seealso cor_matrix_str
#' @family correlation matrix
#' @export
correlation_matrix <- function(..., .names = NULL, .upper = NULL, .byrow = TRUE,
                               .check = TRUE) {

  if(!is.null(.upper)) {
    nvec <- length(.upper)
    nvar <- which(triangular(nvec) == nvec) + 1
    if(.byrow) {
      x <- which(as.vector(t(upper.tri(diag(nvar), diag = FALSE))))
    } else {
      x <- which(as.vector(upper.tri(diag(nvar), diag = FALSE)))
    }
    vals <- .upper

  }  else {
    dots <- enquos(...)
    n <- length(dots)
    nvar <- sqrt(n)
    x <- which(!map_lgl(dots, is_a_dot))
    vals <- map_dbl(dots[x], eval_tidy)
    ilower <- which(as.vector(lower.tri(diag(nvar), diag = FALSE)))
    iupper <- which(as.vector(upper.tri(diag(nvar), diag = FALSE)))
    stopifnot(all(x %in% ilower) | all(x %in% iupper))
  }

  if(anyDuplicated(.names)) abort("There are some duplicate names.")
  if(!is.null(.names)) stopifnot(nvar == length(.names))
  stopifnot(nvar %% 1 == 0)
  stopifnot(all(vals <= 1) & all(vals >= -1))

  out <- 0.5 * diag(nvar)
  out[x] <- vals
  out <- out + t(out)
  if(.check && any(eigen(out)$values < 0)) {
    abort("The correlation matrix is invalid.")
  }

  colnames(out) <- rownames(out) <- .names
  out
}

triangular <- function(n) sapply(1:n, function(x) sum(1:x))

is_a_dot <- function(x) is_symbol(get_expr(x), name = ".")



matrix_str <- function(f_string, n) {
  nspaces <- nchar(f_string)
  dots <- rep(c(paste0(rep(" ", nspaces + 1), collapse = ""), rep("., ", n), "\n"), n)
  dots <- dots[-length(dots)]
  dots[length(dots)] <- "."
  dots <- dots[-1]
  res <- paste0(f_string, "(", paste0(dots, collapse = ""), ")")
  cat(res)
}

#' Input structure for correlation matrix
#'
#' This function is designed for interactive use to quickly generate the
#' input structure needed for `correlation_matrix` function.
#'
#' @param n The dimension of the correlation matrix.
#' @family correlation matrix
#' @export
cor_matrix_str <- function(n) {
  matrix_str("correlation_matrix", n)
}

#' #' Generate correlated random variables
#' #'
#' #' @export
#' correlate <- function(.data, vars = NULL, mean = NULL,
#'                       cor = NULL, cov = NULL, dist = "normal") {
#'   comat <- cor %||% cov
#'   p <- nrow(comat)
#'   n <- nrow(.data)
#'   mean <- mean %||% rep(0, p)
#'   if(all(dist=="normal")) {
#'     eS <- eigen(comat, symmetric = TRUE)
#'     ev <- eS$values
#'     out <- mean + eS$vectors %*% diag(sqrt(ev)) %*% t(matrix(rnorm(p * n), n))
#'     out <- t(out)
#'     colnames(out) <- vars
#'   }
#'   else {
#'     abort("Not implemented")
#'   }
#'   cbind(.data, out)
#' }
