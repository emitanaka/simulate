

#' @export
simulate.data.frame <- function(.data, nsim = 1, seed = NULL, ..., .cor = NULL) {
  dots <- enquos(..., .named = TRUE)
  dots_nms <- names(dots)
  out <- .data
  if(is.null(.cor)) {
    if(inherits(.data, "grouped_df")) {
      rows <- attr(.data, "groups")$.rows
    } else {
      rows <- list(1:nrow(.data))
    }
    for(i in seq_along(dots)) {
      effects <- vector(mode = "list", length = length(rows))
      arcrd <- dots_nms[i]
      out[[arcrd]] <- vector(mode = "numeric", length(nrow(out)))
      for(j in seq_along(rows)) {
        res <- eval_tidy(dots[[i]])
        #browser()
        out[[arcrd]][rows[[j]]] <- simulate(res, data = out[rows[[j]], , drop = FALSE])
        effects[[j]] <- attr(res, "effects")
      }
      attr(out, "effects") <- c(attr(out, "effects"), effects)
    }
  } else {
    correlated_vars <- dimnames(.cor)
    p <- dim(.cor)
    if(is.null(correlated_vars)) {
      if(!all(p == length(dots_nms))) {
        abort("The dimension of the correlation matrix doesn't match the input variables.")
      }
      dimnames(.cor) <- list(dots_nms, dots_nms)
    }
    correlated_vars <- dimnames(.cor)
    if(!correlated_vars[[1]] %in% dots_nms | !correlated_vars[[2]] %in% dots_nms ) {
      abort("Some correlated variables do not exist.")
    }
    if(!all(sort(correlated_vars[[1]])==sort(correlated_vars[[2]]))) {
      abort("The correlation matrix is not defined correctly.")
    }
    Cmat <- .cor[correlated_vars[[1]], correlated_vars[[1]]]
    # if diagonal matrix then just the same as .cor = NULL
    if(all(Cmat==diag(nrow(Cmat)))) {
      return(simulate(.data, ...))
    }

    browser()

  }

  out
}

#' @export
simulate.sim_bernoulli <- function(x, nsim = 1, seed = NULL, data = NULL) {
  bdist <- function(n, prob) stats::rbinom(n, 1, prob)
  simulate_shell_distribution(x, nsim, seed, bdist, data)
}

#' @export
simulate.sim_beta <- function(x, nsim = 1, seed = NULL, data = NULL) {
  simulate_shell_distribution(x, nsim, seed, stats::rbeta, data)
}

#' @export
simulate.sim_binomial <- function(x, nsim = 1, seed = NULL, data = NULL) {
  simulate_shell_distribution(x, nsim, seed, stats::rbinom, data)
}

#' @export
simulate.sim_cauchy <- function(x, nsim = 1, seed = NULL, data = NULL) {
  simulate_shell_distribution(x, nsim, seed, stats::rcauchy, data)
}

#' @export
simulate.sim_chisq <- function(x, nsim = 1, seed = NULL, data = NULL) {
  simulate_shell_distribution(x, nsim, seed, stats::rchisq, data)
}

#' @export
simulate.sim_exponential <- function(x, nsim = 1, seed = NULL, data = NULL) {
  simulate_shell_distribution(x, nsim, seed, stats::rexp, data)
}

#' @export
simulate.sim_f <- function(x, nsim = 1, seed = NULL, data = NULL) {
  fdist <- function(n, df1, df2, ncp) {
    if(is.null(ncp)) {
      values <- stats::rf(n, df1, df2)
    } else {
      values <- double(n)
      if(length(ncp) > 1) {
        w <- is.na(ncp)
        values[w] <- stats::rf(sum(w), df1[w], df2[w], ncp[w])
        values[!w] <- stats::rf(sum(!w), df1[!w], df2[!w], ncp[!w])
      } else {
        if(is.na(ncp)) {
          values <- stats::rf(n, df1, df2)
        } else {
          values <- stats::rf(n, df1, df2, ncp)
        }
      }
    }
    values
  }
  simulate_shell_distribution(x, nsim, seed, fdist, data)
}

#' @export
simulate.sim_form <- function(x, nsim = 1, seed = NULL, data = NULL) {
  fdist <- function(n, form) {
    form
  }
  simulate_shell_distribution(x, nsim, seed, fdist, data, multiply_by_nsim = FALSE)
}


#' @export
simulate.sim_gamma <- function(x, nsim = 1, seed = NULL, data = NULL) {
  simulate_shell_distribution(x, nsim, seed, stats::rgamma, data)
}

#' @export
simulate.sim_geometric <- function(x, nsim = 1, seed = NULL, data = NULL) {
  simulate_shell_distribution(x, nsim, seed, stats::rgeom, data)
}

#' @export
simulate.sim_hypergeometric <- function(x, nsim = 1, seed = NULL, data = NULL) {
  simulate_shell_distribution(x, nsim, seed, stats::rhyper, data)
}

#' @export
simulate.sim_multinominal <- function(x, nsim = 1, seed = NULL, data = NULL) {
  rmulti <- function(n, size, prob) {
    l <- max(c(length(size), length(prob)))
    nclasses <- lengths(prob)
    # append the prob with 0 if less than max number of classes
    prob <- lapply(prob, function(x) c(x, rep(0, max(nclasses) - length(x))))
    prob <- rep(prob, length.out = l)
    size <- rep(size, length.out = l)
    out <- lapply(rep(1:l, nsim), function(i) stats::rmultinom(1, size[[i]], prob[[i]]))
    out <- t(Reduce(cbind, out))
    structure(lapply(1:ncol(out), function(i) out[,i]),
              class = c("sim_draw_multinominal", "list"),
              nclass = rep(nclasses, nsim))
  }

  args <- attr(x, "args")
  if(!is.list(args$input$prob)) {
    args$input$prob <- list(args$input$prob)
  }
  attr(x, "args") <- args
  simulate_shell_distribution(x, nsim, seed, rmulti, data)
}

#' @export
simulate.sim_negative_binomial <- function(x, nsim = 1, seed = NULL, data = NULL) {
  simulate_shell_distribution(x, nsim, seed, stats::rnbinom, data)
}

#' @export
simulate.sim_normal <- function(x, nsim = 1, seed = NULL, data = NULL) {
  simulate_shell_distribution(x, nsim, seed, stats::rnorm, data)
}

#' @export
simulate.sim_poisson <- function(x, nsim = 1, seed = NULL, data = NULL) {
  simulate_shell_distribution(x, nsim, seed, stats::rpois, data)
}

#' @export
simulate.sim_t <- function(x, nsim = 1, seed = NULL, data = NULL) {
  tdist <- function(n, df, mean = 0, sd = 1, ncp = NULL, params = NULL) {
    if(is.null(ncp)) {
      values <- stats::rt(n, df) * sd + mean
    } else {
      values <- stats::rt(n, df, ncp) * sd + mean
    }
    values
  }
  simulate_shell_distribution(x, nsim, seed, tdist, data)
}

#' @export
simulate.sim_uniform <- function(x, nsim = 1, seed = NULL, data = NULL) {
  simulate_shell_distribution(x, nsim, seed, stats::runif, data)
}

#' @export
simulate.sim_weibull <- function(x, nsim = 1, seed = NULL, data = NULL) {
  simulate_shell_distribution(x, nsim, seed, stats::rweibull, data)
}

simulate_shell_distribution <- function(x, nsim, seed, fdist, data, multiply_by_nsim = TRUE) {
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    stats::runif(1)
  if (is.null(seed))
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }

  args <- attr(x, "args")
  args$data <- data %||% args$data
  paramsv <- names(args$input)
  #browser()
  prms <- sapply(paramsv, function(aparam) {
    eval_effects(args$data, args$params[[aparam]], args$input[[aparam]])
  }, USE.NAMES = TRUE, simplify = FALSE)
  input <- sapply(paramsv, function(aparam) prms[[aparam]]$input,
                  USE.NAMES = TRUE, simplify = FALSE)
  # check parameters

  lapply(args$validator, function(x) eval_tidy(x, data = input))
  # continue
  l <- lengths(input)
  if(!all(l %in% c(max(l), 1L, 0L))) {
    abort("The parameters should be of length 1 or of the same lengths.")
  }
  n <- nrow(args$data) %||% max(l) %||% 1L
  out <- if(multiply_by_nsim) { do.call(fdist, c(list(n * nsim), input)) } else {
    Reduce("c", lapply(1:nsim, function(i) do.call(fdist, c(list(n), input))))
  }
  structure(out,
            params = input,
            class = unique(c("sim_draw", class(out))),
            seed = RNGstate)
}

#' @export
print.sim_draw <- function(x, ...) {
  attr(x, "seed") <- NULL
  attr(x, "params") <- NULL
  if(inherits(x, c("numeric", "integer"))) attr(x, "class") <- NULL
  NextMethod()
}

#' @export
print.sim_draw_multinominal <- function(x, ..., max = NULL) {
 # browser()
  max <- max %||% getOption("max.print", 99999L)
  M <- matrix(as.character(do.call("c", x)), ncol = length(x))
  Mdot <- matrix(".", ncol = ncol(M), nrow = nrow(M))
  nclass <- attr(x, "nclass")
  for(i in 1:nrow(M)) {
    Mdot[i, 1:nclass[i]] <- M[i, 1:nclass[i]]
  }
  rownames(Mdot) <- rep.int("", nrow(Mdot))
  colnames(Mdot) <- paste0("[", 1:ncol(Mdot), "]")
  print(Mdot, quote = FALSE, right = TRUE, max = max)
}

`[.sim_draw_multinominal` <- `[[`
