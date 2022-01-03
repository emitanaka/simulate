eval_effects <- function(data, params, input) {
  n <- nrow(data) %||% 1L
  res <- data.frame(row.names = seq(n))
  prms <- unique(c(names(params), names(data)))
  #browser()
  res <- sapply(prms, function(avar) true_effects(data[[avar]], params[[avar]], n),
                USE.NAMES = TRUE, simplify = FALSE)
  if(is_formula(input)) {
    vars <- all.vars(input)
    if(!all(ind <- vars %in% names(res))) {
      abort(sprintf("The variables, %s, are not defined in the data.", paste0(vars[!ind], collapse = ", ")))
    }
    input <- eval(get_expr(do.call("substitute", list(input, res))))
  }
  list(input = input,
       effects = res)
}

prm_to_lower_level <- function(params, params_essential) {
  # if only one param then it  not have to be nested
  if(length(params_essential)==1L) {
    out <- list()
    current <- params[[params_essential]]
    if(is.null(current)) {
      out[[params_essential]] <- params
    } else {
      out[[params_essential]] <- c(params[[params_essential]], params[setdiff(names(params), params_essential)])
    }
  } else {
    out <- params
  }
  out
}

context_args <- function(input, params, data, validator = NULL) {
  lapply(validator, function(x) eval_tidy(x, data = input))
  e <- new.env()
  e$data <- data
  e$input <- input
  e$params <- prm_to_lower_level(params, names(input))
  e$validator <- validator
  return(e)
}

deparse_to_char <- function(x) {
  if(is_quosure(x)) {
    x <- get_expr(x)
  }
  if(is_formula(x) | is_call(x) | is_symbol(x)) {
    x <- paste(deparse(x), collapse = "")
  }
  x
}

is_sim_distribution <- function(x) {
  inherits(x, "sim_distribution")
}

true_effects <- function(f, effects, n = 1L) {
  vlev <- levels(f) %||% sort(unique(f))
  res <- setNames(rep(0, length(vlev)), vlev)
  #browser()
  if(is_sim_distribution(effects)) {
    simulate(effects, nsim = n)
  } else if(is.null(effects)) {
    f
  } else if(is.list(effects)) {
    res[1:length(effects$values)] <- effects$values
    true_effects(f, res)
  } else if(is_named(effects)) {
    veff <- names(effects)
    vmiss <- setdiff(vlev, veff)
    res[veff] <- effects
    unname(res[f])
  } else if(is_vector(effects)) {
    res[1:length(effects)] <- effects
    true_effects(f, res)
  } else if(is_formula(effects)) {
    out <- eval_tidy(get_expr(effects), list(data = data.frame(eff = vlev)))
    res[1:length(res)] <- out$values
    true_effects(f, res)
  }
}

