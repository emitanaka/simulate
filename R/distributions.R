#' Simulation framework for a fixed mean structure
#'
#' @param form A formula structure of the effects.
#' @param params The list of parameters.
#' @param data An optional data frame.
#'
#' @examples
#' # simulate
#' sim_fixed(mean = 3) %>%
#'   simulate(10)
#'
#' sim_fixed(mean = ~grp)
#'
#' @family simulation frameworks
#' @export
sim_form <- function(form = 0, params = NULL, data = NULL) {
  structure(deparse_to_char(form),
            class = c("sim_form", "sim_distribution"),
            args = context_args(input = list(form = form),
                                params = params, data = data))
}


#' Simulation framework for Bernoulli random variable
#'
#' @inheritParams sim_form
#' @param prob The probability of success
#' @family simulation frameworks
#' @export
sim_bernoulli <- function(prob, params = NULL, data = NULL) {
  validator <- list(quote(valid_prop(prob)))
  structure(paste0("~Bernoulli(", deparse_to_char(prob), ")"),
            class = c("sim_bernoulli", "sim_distribution"),
            args = context_args(input = list(prob = prob),
                                params = params, data = data,
                                validator = validator))
}



#' Simulation framework for Beta random variable
#'
#' @inheritParams sim_form
#' @param shape1,shape2 Parameters of the Beta distribution.
#' @family simulation frameworks
#' @export
sim_beta <- function(shape1, shape2, params = NULL) {
  validator <- list(quote(valid_positive(shape1)),
                    quote(valid_positive(shape2)))
  structure(paste0("~Beta(", deparse_to_char(shape1), ", ", deparse_to_char(shape2), ")"),
            class = c("sim_beta", "sim_distribution"),
            args = context_args(input = list(shape1 = shape1, shape2 = shape2),
                                params = params, data = data,
                                validator = validator))
}

#' Simulation framework for Binomial random variable
#'
#' @inheritParams sim_form
#' @inheritParams sim_bernoulli
#' @param size The number of trials.
#' @family simulation frameworks
#' @export
sim_binomial <- function(size, prob, params = NULL, data = NULL) {
  validator <- list(quote(valid_integer(size)),
                    quote(valid_positive(size)),
                    quote(valid_prop(prob)))
  structure(paste0("~Binomial(", deparse_to_char(size), ", ", deparse_to_char(prob), ")"),
            class = c("sim_binomial", "sim_distribution"),
            args = context_args(input = list(size = size, prob = prob),
                                params = params, data = data,
                                validator = validator))
}

#' Simulation framework for Cauchy random variable
#'
#' @inheritParams sim_form
#' @param location The location parameter.
#' @param scale The scale parameter.
#' @family simulation frameworks
#' @export
sim_cauchy <- function(location, scale, params = NULL, data = NULL) {
  validator <- list(quote(valid_positive(scale)))
  structure(paste0("~Cauchy(", deparse_to_char(location), ", ", deparse_to_char(scale), ")"),
            class = c("sim_cauchy", "sim_distribution"),
            args = context_args(input = list(location = location, scale = scale),
                                params = params, data = data,
                                validator = validator))
}

#' Simulation framework for Chi-square random variable
#'
#' @inheritParams sim_form
#' @param df The degrees of freedom.
#' @family simulation frameworks
#' @export
sim_chisq <- function(df, ncp = 0, params = NULL, data = NULL) {
  validator <- list(quote(valid_positive(df)),
                    quote(valid_positive(ncp)))
  structure(paste0("~Chisq(", deparse_to_char(df), ")"),
            class = c("sim_chisq", "sim_distribution"),
            args = context_args(input = list(df = df, ncp = ncp),
                                params = params, data = data,
                                validator = validator))
}

#' Simulation framework for exponential random variable
#'
#' @inheritParams sim_form
#' @param rate The rate.
#' @family simulation frameworks
#' @export
sim_exponential <- function(rate = 1, params = NULL, data = NULL) {
  validator <- list(quote(valid_positive(rate)))
  structure(paste0("~Exponential(", deparse_to_char(rate), ")"),
            class = c("sim_exponential", "sim_distribution"),
            args = context_args(input = list(rate = rate),
                                params = params, data = data,
                                validator = validator))
}

#' Simulation framework for F random variable
#'
#' @inheritParams sim_form
#' @param df1,df2 The numerator and denominator degrees of freedom.
#' @family simulation frameworks
#' @export
sim_f <- function(df1, df2, ncp = NULL, params = NULL, data = NULL) {
  validator <- list(quote(valid_positive(df1)),
                    quote(valid_positive(df2)),
                    quote(valid_positive_or_null(ncp)))
  structure(paste0("~F(", deparse_to_char(df1), ",", deparse_to_char(df2), ")"),
            class = c("sim_f", "sim_distribution"),
            args = context_args(input = list(df1 = df1, df2 = df2, ncp = ncp),
                                params = params, data = data,
                                validator = validator))
}

#' Simulation framework for Gamma random variable
#'
#' @inheritParams sim_form
#' @param shape The shape.
#' @param rate The rate.
#' @family simulation frameworks
#' @export
sim_gamma <- function(shape, rate, params = NULL, data = NULL) {
  validator <- list(quote(valid_positive(shape)),
                    quote(valid_positive(rate)))
  structure(paste0("~Gamma(", deparse_to_char(shape), ",", deparse_to_char(rate), ")"),
            class = c("sim_gamma", "sim_distribution"),
            args = context_args(input = list(shape = shape, rate = rate),
                                params = params, data = data,
                                validator = validator))
}

#' Simulation framework for Geometric random variable
#'
#' @inheritParams sim_form
#' @inheritParams sim_bernoulli
#' @family simulation frameworks
#' @export
sim_geometric <- function(prob, params = NULL, data = NULL) {
  validator <- list(quote(valid_prop(prob)))
  structure(paste0("~Geometric(", deparse_to_char(prob), ")"),
            class = c("sim_geometric", "sim_distribution"),
            args = context_args(input = list(prob = prob),
                                params = params, data = data,
                                validator = validator))
}

#' Simulation framework for Hyper-geometric random variable
#'
#' @inheritParams sim_form
#' @param m,n,k The parameters of hypergeometric distribution.
#' @family simulation frameworks
#' @export
sim_hypergeometric <- function(m, n, k, params = NULL, data = NULL) {
  validator <- list(quote(valid_positive(m)),
                    quote(valid_positive(n)),
                    quote(valid_positive(k)),
                    quote(valid_greater(m + n, k)))
  structure(paste0("~Hypergeometric(", deparse_to_char(m), ", ", deparse_to_char(n), ", ", deparse_to_char(k), ")"),
            class = c("sim_hypergeometric", "sim_distribution"),
            args = context_args(input = list(m = m, n = n, k = k),
                                params = params, data = data,
                                validator = validator))
}

#' Simulation framework for multinominal distribution
#'
#' @inheritParams sim_binomial
#' @family simulation frameworks
#' @export
sim_multinominal <- function(size, prob, params = NULL, data = NULL) {
  validator <- list(quote(valid_positive(size)),
                    quote(lapply(prob, valid_positive)))
  structure(paste0("~Multinominal(", deparse_to_char(size), ",",
                   deparse(prob), ")"),
            class = c("sim_multinominal", "sim_distribution"),
            args = context_args(input = list(size = size, prob = prob),
                                params = params, data = data,
                                validator = validator))
}


#' Simulation framework for negative Binomial random variable
#'
#' @inheritParams sim_binomial
#' @family simulation frameworks
#' @export
sim_negative_binomial <- function(size, prob, params = NULL, data = NULL) {
  validator <- list(quote(valid_positive(size)),
                    quote(valid_prop(prob)))
  structure(paste0("~NegativeBinomial(", deparse_to_char(size), ",", deparse_to_char(prob), ")"),
            class = c("sim_negative_binomial", "sim_distribution"),
            args = context_args(input = list(size = size, prob = prob),
                                params = params, data = data,
                                validator = validator))
}

#' Simulation framework for Normal random variable
#'
#' @inheritParams sim_form
#' @param mean The mean.
#' @param sd The standard deviation.
#' @family simulation frameworks
#' @export
sim_normal <- function(mean = 0, sd = 1, params = NULL, data = NULL) {
  validator <- list(quote(valid_positive(sd)))
  structure(paste0("~Normal(", deparse_to_char(mean), ", ", deparse_to_char(sd), ")"),
            class = c("sim_normal", "sim_distribution"),
            args = context_args(input = list(mean = mean, sd = sd),
                                params = params, data = data,
                                validator = validator))
}

#' Simulation framework for Poisson random variable
#'
#' @inheritParams sim_form
#' @param lambda The mean.
#' @family simulation frameworks
#' @export
sim_poisson <- function(lambda, params = NULL, data = NULL) {
  validator <- list(quote(valid_positive(lambda)))
  structure(paste0("~Poisson(", deparse_to_char(lambda), ")"),
            class = c("sim_poisson", "sim_distribution"),
            args = context_args(input = list(lambda = lambda),
                                params = params, data = data,
                                validator = validator))
}

#' Simulation framework for t random variable
#'
#' @inheritParams sim_normal
#' @inheritParams sim_chisq
#' @param ncp The non-centrality parameters.
#' @family simulation frameworks
#' @export
sim_t <- function(df, mean = 0, sd = 1, ncp = NULL, params = NULL, data = NULL) {
  validator <- list(quote(valid_positive(df)),
                    quote(valid_positive(sd)),
                    quote(valid_positive_or_null(ncp)))
  structure(paste0("~t(", deparse_to_char(df), ", ", deparse_to_char(mean), ", ", deparse_to_char(sd), ")"),
            class = c("sim_t", "sim_distribution"),
            args = context_args(input = list(df = df, mean = mean, sd = sd, ncp = ncp),
                                params = params, data = data,
                                validator = validator))
}

#' Simulation framework for uniform random variable
#'
#' @inheritParams sim_form
#' @param min,max The minimum and maximum value.
#' @family simulation frameworks
#' @export
sim_uniform <- function(min, max, params = NULL, data = NULL) {
  validator <- list(quote(valid_greater(max, min)))
  structure(paste0("~Uniform(", deparse_to_char(min), ", ", deparse_to_char(max), ")"),
            class = c("sim_uniform", "sim_distribution"),
            args = context_args(input = list(min = min, max = max),
                                params = params, data = data,
                                validator = validator))
}

#' Simulation framework for Weibull random variable
#'
#' @inheritParams sim_form
#' @param shape,scale The shape and scale.
#' @family simulation frameworks
#' @export
sim_weibull <- function(shape, scale = 1, params = NULL, data = NULL) {
  validator <- list(quote(valid_positive(scale)),
                    quote(valid_positive(shape)))
  structure(paste0("~Weibull(", deparse_to_char(shape), ", ", deparse_to_char(scale), ")"),
            class = c("sim_weibull", "sim_distribution"),
            args = context_args(input = list(shape = shape, scale = scale),
                                params = params, data = data,
                                validator = validator))
}



#' @export
print.sim_distribution <- function(x, ...) {
  cat(x, "\n")
}







