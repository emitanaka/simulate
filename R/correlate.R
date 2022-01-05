
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

#' Convert values to correlation matrix
#'
#' @param ... A series of values or dots for the correlation matrix.
#' @param .names The name of the variables.
#' @param .upper,.lower A vector of values for the upper (lower) triangle values
#'   filled by column (row). If the `.upper` or `.lower` is defined then `...`
#'   must not be defined.
#'
#' @export
correlation_matrix <- function(..., .names = NULL, .upper = NULL, .lower = NULL) {

  if(!is.null(.upper)) {
    nvec <- length(.upper)
    nvar <- which(triangular(nvec) == nvec) + 1
    x <- which(as.vector(upper.tri(diag(nvar), diag = FALSE)))
    vals <- .upper

  } else if(!is.null(.lower)) {
    nvec <- length(.lower)
    nvar <- which(triangular(nvec) == nvec) + 1
    x <- which(as.vector(t(lower.tri(diag(nvar), diag = FALSE))))
    vals <- .lower

  } else {
    dots <- enquos(...)
    n <- length(dots)
    nvar <- sqrt(n)
    x <- which(!map_lgl(dots, is_a_dot))
    vals <- map_dbl(dots[x], eval_tidy)
    ilower <- which(as.vector(lower.tri(diag(nvar), diag = FALSE)))
    iupper <- which(as.vector(upper.tri(diag(nvar), diag = FALSE)))
    stopifnot(all(x %in% ilower) | all(x %in% iupper))
  }

  stopifnot(nvar %% 1 == 0)
  stopifnot(all(vals <= 1) & all(vals >= -1))

  out <- 0.5 * diag(nvar)

  out[x] <- vals
  out <- out + t(out)
  colnames(out) <- rownames(out) <- .names
  out
}

triangular <- function(n) sapply(1:n, function(x) sum(1:x))

is_a_dot <- function(x) is_symbol(get_expr(x), name = ".")

mset_names <- function(X, .vars = NULL) {
  dimnames(X) <- list(.vars, .vars)
  X
}

mset_values <- function(X, i, j, value) {
  X[i, j] <- value
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


matrix_str <- function(f_string, n) {
  nspaces <- nchar(f_string)
  dots <- rep(c(paste0(rep(" ", nspaces + 1), collapse = ""), rep("., ", n), "\n"), n)
  dots <- dots[-length(dots)]
  dots[length(dots)] <- "."
  dots <- dots[-1]
  res <- paste0(f_string, "(", paste0(dots, collapse = ""), ")")
  cat(res)
}

#' @export
cor_matrix_str <- function(n) {
  matrix_str("correlation_matrix", n)
}
