eval_effects <- function(data, params, input, n) {
  res <- data.frame(row.names = seq(n))
  prms <- names(params)
  for(avar in prms) {
    res[[avar]] <- true_effects(data[[avar]], params[[avar]])
  }
  if(is_formula(input)) {
    vars <- all.vars(input)
    # check_var_exists(data %@% "design", label = unique(c(vars, prms)))
    input <- eval(get_expr(do.call("substitute", list(input, res))))
  }
  list(input = input,
       effects = res)
}


get_topenv_info <- function() {
  e <- caller_env(n = 2)$.top_env
  data <- e$data
  n <- nlevels(data[[e$var]])
  list(data = data,
       n = ifelse(n==0, nrow(data), n))
}



true_effects <- function(f, effects) {
  vlev <- levels(f)
  res <- setNames(rep(0, length(vlev)), vlev)
  if(is.list(effects)) {
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
  }
}

