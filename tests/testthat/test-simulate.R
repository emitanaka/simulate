test_that("simulate", {
  g1 <- sample(c("a", "b"), 20000, replace = TRUE)
  g2 <- sample(c("a", "b"), 20000, replace = TRUE)
  expect_equal({
    sim_form(~ p[1] * g1 + p[2] * g2 + p[3] * g3,
             data = data.frame(g1 = g1, g2 = g2, g3 = g2)) %>%
      params(g1 = c(1, 1),
             g2 = c(2, 2),
             g3 = c(3, 3),
             p = sim_multinominal(1, c(0.2, 0.4, 0.4))) %>%
      simulate() %>%
      table()/20000
  }, c(0.2, 0.4, 0.4), ignore_attr = TRUE, tolerance = 0.01)

  expect_equal({
    sim_form(~p[1] * x1 + p[2] * x2, data = data.frame(x1 = rep(0, 10000),
                                                       x2 = rep(10, 10000))) %>%
      params(p = sim_multinominal(1, c(0.1, 0.9))) %>%
      simulate() %>%
      table() / 10000
  }, c(0.1, 0.9), ignore_attr = TRUE, tolerance = 0.01)

  expect_equal({
    sim_form(~p * g1) %>%
      params(g1 = sim_normal(),
             p = sim_bernoulli(0)) %>%
      simulate()
  }, 0, ignore_attr = TRUE)


  cormat(., 0, 0.5,
         ., ., .,
         ., ., .)

  data.frame(id = seq(100000))  %>%
    simulate(y1 = sim_normal())


  d <- data.frame(id = seq(100000)) %>%
    simulate(y1 = sim_normal(0, 1),
             y2 = sim_normal(~0.8 * y1, 1),
             y3 = sim_normal(0, 1),
             y4 = sim_normal(~ 0.5 * y3, 1),
             .cor = correlation_matrix(., ., 0.5,
                                       ., ., -0.3,
                                       ., ., .,
                                       .names = c("y1", "y2", "y3")))

  data.frame(id = seq(100000)) %>%
    simulate(y1 = sim_normal(0, 1),
             y2 = sim_normal(~0.8 * y1, 1),
             y3 = sim_normal(0, 1),
             y4 = sim_normal(~ 0.5 * y3, 1),
             .cor = correlation_matrix(., ., 0.5, .,
                                       ., ., -0.3, .,
                                       ., ., ., .,
                                       ., ., ., .))

  cor(d[, c("y1", "y2", "y3", "y4")])
  c(mean(d$y1), mean(d$y2), mean(d$y3), mean(d$y4))
  c(sd(d$y1), sd(d$y2), sd(d$y3), sd(d$y4))

  library(ggplot2)
  ggplot(d, aes(y1, y2)) + geom_point()

  library(dplyr)
  df1 %>%
    group_by(grp2) %>%
    simulate(resp1 = sim_normal())

  simulate(df1, resp1 = sim_normal(mean=~ grp1 + grp2,
                                   sd = ~ grp2,
                                   list(mean = list(grp1 = c("C" = 10, "A" = -20, "B" = 3),
                                                      grp2 = c("a" = 10)),
                                          sd = list(grp2 = c("a" = 1)))))


  simulate(df1, resp1 = sim_normal(mean=~ grp1,
                                   sd = 1,
                                   params(mean = list(grp1 = c("A" = 10)))))

  df1 %>%
    simulate(y1 = sim_normal(mean = ~grp1, sd = ~grp2) %>%
                params("mean", grp1 = c("A" = 10, "B" = 3),
                             grp2 = c("a" = 3)) %>%
                params("sd", grp2 = c(1, 2)),
             y2 = sim_normal())

  df1 %>%
    simulate(y1 ~ normal(mean = ~grp1, sd = ~grp2) %>%
               params("mean", grp1 = c("A" = 10, "B" = 3),
                      grp2 = c("a" = 3)) %>%
               params("sd", grp2 = c(1, 2)),
             y2 = sim_normal())

  simulate(df1, resp1 = sim_normal(mean=~ grp1 + grp2,
                                   sd = 1,
                                   params(mean = list(grp1 =~ sim_normal(),
                                                      grp2 =~ sim_t(3)))))

  simulate(df1, resp1 = sim_fixed(~grp1 + grp2,
                                  params(grp1 = c("C" = 10, "A" = -20),
                                         grp2 =~ sim_normal(0, 1))))


})
