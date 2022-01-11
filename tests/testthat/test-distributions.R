test_that("distributions", {
  aseed <- runif(1)
  expect_equal({
    sim_bernoulli(0) %>%
      simulate()
  }, 0, ignore_attr = TRUE)

  expect_equal({
    sim_bernoulli(1) %>%
      simulate()
  }, 1, ignore_attr = TRUE)

  expect_equal({
    sim_bernoulli(c(0, 1)) %>%
      simulate()
  }, c(0, 1), ignore_attr = TRUE)

  expect_equal({
    sim_bernoulli(0) %>%
      simulate(nsim = 3)
  }, c(0, 0, 0), ignore_attr = TRUE)

  expect_equal({
    sim_bernoulli(c(0, 1)) %>%
      simulate(nsim = 3)
  }, c(0, 1, 0, 1, 0, 1), ignore_attr = TRUE)

  expect_error({
    sim_bernoulli(2)
  })

  expect_equal({
    sim_beta(1, 1) %>%
      simulate(nsim = 5000000, seed = aseed) %>%
      mean()
  }, 0.5, ignore_attr = TRUE, tolerance = 0.0001)

  y <- sim_beta(1, 1) %>%
    simulate(nsim = 1000000, seed = aseed)
  expect_equal({
    sim_beta(1, 1) %>%
      simulate(nsim = 1000000, seed = aseed)
  }, y, ignore_attr = TRUE)

  expect_equal({
    sim_binomial(3, 0.5) %>%
      simulate(nsim = 1000000, seed = aseed) %>%
      mean()
  }, 1.5, ignore_attr = TRUE, tolerance = 0.0001)

  expect_equal({
    sim_cauchy(-3, 1) %>%
      simulate(nsim = 1000000, seed = aseed) %>%
      median()
  }, -3, ignore_attr = TRUE, tolerance = 0.001)

  expect_equal({
    sim_chisq(4) %>%
      simulate(nsim = 1000000, seed = aseed) %>%
      mean()
  }, 4, ignore_attr = TRUE, tolerance = 0.001)

  expect_equal({
    sim_exponential(4) %>%
      simulate(nsim = 5000000, seed = aseed) %>%
      mean()
  }, 1/4, ignore_attr = TRUE, tolerance = 0.001)

  expect_equal({
    sim_f(4, 3) %>%
      simulate(nsim = 10000000, seed = aseed) %>%
      mean()
  }, 3 / (3 - 2), ignore_attr = TRUE, tolerance = 0.01)

  expect_equal({
    sim_gamma(4, 3) %>%
      simulate(nsim = 1000000, seed = aseed) %>%
      mean()
  }, 4 * 1/3, ignore_attr = TRUE, tolerance = 0.001)

  expect_equal({
    sim_geometric(0.8) %>%
      simulate(nsim = 10000000, seed = aseed) %>%
      mean()
  }, 0.2/0.8, ignore_attr = TRUE, tolerance = 0.001)

  expect_error(sim_hypergeometric(3, 4, 10))

  expect_equal({
    sim_hypergeometric(10, 4, 8) %>%
      simulate(nsim = 10000000, seed = aseed) %>%
      mean()
  }, 8 * 10/(10 + 4), ignore_attr = TRUE, tolerance = 0.001)


  expect_equal({
    sim_multinominal(1, c(0.4, 0.8, 0.3)) %>%
      simulate(nsim = 20) %>%
      Reduce(`+`, .)
  }, rep(1L, 20), ignore_attr = TRUE)

  expect_equal({
    sim_multinominal(c(1, 3), c(0.4, 0.8, 0.3)) %>%
      simulate(nsim = 20) %>%
      Reduce(`+`, .)
  }, rep(c(1L, 3L), 20), ignore_attr = TRUE)

  expect_equal({
    sim_multinominal(c(1, 3), list(c(0, 0, 1), c(1, 0))) %>%
      simulate(nsim = 20) %>%
      Reduce(`+`, .)
  }, rep(c(1L, 3L), 20), ignore_attr = TRUE)


  expect_equal({
    sim_negative_binomial(10, 0.4) %>%
      simulate(nsim = 10000000, seed = aseed) %>%
      mean()
  }, 10 * 0.6 / (1 - 0.6), ignore_attr = TRUE, tolerance = 0.001)

  expect_equal({
    sim_normal(-10, 1) %>%
      simulate(nsim = 10000000, seed = aseed) %>%
      mean()
  }, -10, ignore_attr = TRUE, tolerance = 0.001)

  expect_equal({
    sim_normal(-10, 3) %>%
      simulate(nsim = 10000000, seed = aseed) %>%
      sd()
  }, 3, ignore_attr = TRUE, tolerance = 0.001)

  expect_equal({
    sim_poisson(3) %>%
      simulate(nsim = 10000000, seed = aseed) %>%
      mean()
  }, 3, ignore_attr = TRUE, tolerance = 0.001)

  expect_equal({
    sim_t(3, -5, 2) %>%
      simulate(nsim = 10000000, seed = aseed) %>%
      mean()
  }, -5, ignore_attr = TRUE, tolerance = 0.001)

  expect_error(sim_uniform(3, -5))

  expect_equal({
    sim_uniform(-4, -2) %>%
      simulate(nsim = 10000000, seed = aseed) %>%
      mean()
  }, -3, ignore_attr = TRUE, tolerance = 0.001)

  expect_equal({
    sim_weibull(3, 1.5) %>%
      simulate(nsim = 10000000, seed = aseed) %>%
      mean()
  }, 1.5 * gamma(1 + 1/3), ignore_attr = TRUE, tolerance = 0.001)

  x <- rep(c(-3, 0, 10), each = 20)
  x1 <- rep(c(-3, 0, 10), each = 10)
  expect_equal({
    sim_normal(mean= ~x, sd = 0.01, data = data.frame(x = x)) %>%
      simulate()
  }, x, ignore_attr = TRUE, tolerance = 0.01)

  expect_equal({
    sim_normal(mean= ~x, sd = 0.01, data = data.frame(x = x)) %>%
      simulate(nsim = 2)
  }, rep(x, times = 2), ignore_attr = TRUE, tolerance = 0.01)

  expect_error({
    sim_normal(mean= ~x, sd = c(0.01, 0.02), data = data.frame(x = x)) %>%
      simulate()
  })

  frm <- sim_normal(mean= ~x, sd = 0.01, data = data.frame(x = x1))

  expect_equal({
    frm %>%
      simulate(nsim = 2, data = data.frame(x = x))
  }, rep(x, times = 2), ignore_attr = TRUE, tolerance = 0.01)

  expect_equal({
    frm %>%
      simulate(nsim = 3, data = data.frame(x = x1))
  }, rep(x1, times = 3), ignore_attr = TRUE, tolerance = 0.01)

  g <- sample(c("a", "b"), 10, replace = TRUE)
  frm2 <- sim_normal(mean= ~g, sd = 0.01, data = data.frame(g = g))
  expect_equal({
    frm2 %>%
      params("mean", g = c("a" = 3, "b" = 10)) %>%
      simulate()
  }, c(3, 10)[as.numeric(g=="b") + 1], ignore_attr = TRUE, tolerance = 0.01)

  expect_equal({
    frm2 %>%
      params("mean", g = c("a" = 3, "b" = 10)) %>%
      simulate()
  }, c(3, 10)[as.numeric(g=="b") + 1], ignore_attr = TRUE, tolerance = 0.01)

  expect_equal({
    sim_normal(mean= ~g, sd = 0.01,
               data = data.frame(g = g),
               params = list(mean = list(g = c("a" = -2, "b" = -1)))) %>%
      params("mean", g = c("a" = 3, "b" = 10)) %>%
      simulate()
  }, c(3, 10)[as.numeric(g=="b") + 1], ignore_attr = TRUE, tolerance = 0.01)

  expect_equal({
    sim_normal(mean= ~g, sd = 0.01,
               data = data.frame(g = g),
               params = list(mean = list(g = c("a" = -2, "b" = -1)))) %>%
      simulate()
  }, c(-2, -1)[as.numeric(g=="b") + 1], ignore_attr = TRUE, tolerance = 0.01)


  expect_equal({
    sim_form(~g,
               data = data.frame(g = g),
               params = list(form = list(g = c("a" = -2, "b" = -1)))) %>%
      simulate()
  }, c(-2, -1)[as.numeric(g=="b") + 1], ignore_attr = TRUE)

  expect_equal({
    sim_form(~g,
             data = data.frame(g = g),
             params = list(g = c("a" = -2, "b" = -1))) %>%
      simulate()
  }, c(-2, -1)[as.numeric(g=="b") + 1], ignore_attr = TRUE)

  expect_equal({
    sim_form(~g,
             data = data.frame(g = g)) %>%
      params(g = c("a" = -2, "b" = 3)) %>%
      simulate()
  }, c(-2, 3)[as.numeric(g=="b") + 1], ignore_attr = TRUE)

  g1 <- sample(c("a", "b"), 10, replace = TRUE)
  g2 <- sample(c("a", "b"), 10, replace = TRUE)

  expect_equal({
    sim_form(~g1 + g2,
             data = data.frame(g1 = g1, g2 = g2),
             params = list(g2 = c("a" = 10, "b" = 0))) %>%
      params(g1 = c("a" = -2, "b" = 3)) %>%
      simulate()
  }, c(-2, 3)[as.numeric(g1=="b") + 1] + c(10, 0)[as.numeric(g2=="b") + 1],
  ignore_attr = TRUE)

  expect_equal({
    sim_form(~g1 + g2,
             data = data.frame(g1 = g1, g2 = g2),
             params = list(g2 = c("a" = 10, "b" = 0))) %>%
      params("form", g1 = c("a" = -2, "b" = 3)) %>%
      simulate()
  }, c(-2, 3)[as.numeric(g1=="b") + 1] + c(10, 0)[as.numeric(g2=="b") + 1],
  ignore_attr = TRUE)



})
