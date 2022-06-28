test_that("simulate", {
  aseed <- runif(1)
  g1 <- sample(c("a", "b"), 20000, replace = TRUE)
  g2 <- sample(c("a", "b"), 20000, replace = TRUE)
  expect_equal({
    sim_form(~ p[1] * g1 + p[2] * g2 + p[3] * g3,
             data = data.frame(g1 = g1, g2 = g2, g3 = g2)) %>%
      params(g1 = c(1, 10),
             g2 = c(2, 20),
             g3 = c(3, 30),
             p = sim_multinominal(1, c(0.2, 0.4, 0.4))) %>%
      simulate(seed = aseed) %>%
      table()/20000
  }, c(0.2, 0.4, 0.4), ignore_attr = TRUE, tolerance = 0.01)

  expect_equal({
    sim_form(~p[1] * x1 + p[2] * x2, data = data.frame(x1 = rep(0, 10000),
                                                       x2 = rep(10, 10000))) %>%
      params(p = sim_multinominal(1, c(0.1, 0.9))) %>%
      simulate(seed = aseed) %>%
      table() / 10000
  }, c(0.1, 0.9), ignore_attr = TRUE, tolerance = 0.01)

  expect_equal({
    sim_form(~p * g1) %>%
      params(g1 = sim_normal(),
             p = sim_bernoulli(0)) %>%
      simulate()
  }, 0, ignore_attr = TRUE)

  expect_equal({
    df <- data.frame(id = seq(100000))  %>%
      simulate(y1 = sim_normal(10, 0.5), .seed = aseed)
    mean(df$y1)
  }, 10, ignore_attr = TRUE, tolerance = 0.001)

  d <- data.frame(id = seq(500000)) %>%
    simulate(y1 = sim_normal(0, 1),
             y2 = sim_normal(10, 1),
             y3 = sim_normal(-10, 1),
             .cor = correlation_matrix(., ., 0.5,
                                       ., ., -0.3,
                                       ., ., .),
             .seed = aseed)
  expect_equal(mean(d$y1), 0, tolerance = 0.005)
  expect_equal(mean(d$y2), 10, tolerance = 0.005)
  expect_equal(mean(d$y3), -10, tolerance = 0.005)
  expect_equal(sd(d$y1), 1, tolerance = 0.005)
  expect_equal(sd(d$y2), 1, tolerance = 0.005)
  expect_equal(sd(d$y3), 1, tolerance = 0.005)
  expect_equal(cor(d$y1, d$y2), 0, tolerance = 0.05)
  expect_equal(cor(d$y1, d$y3), 0.5, tolerance = 0.05)
  expect_equal(cor(d$y2, d$y3), -0.3, tolerance = 0.05)

  n <- 10000
  df1 <- data.frame(grp1 = sample(LETTERS[1:3], n, replace = TRUE),
                    grp2 = sample(letters[1:2], n, replace = TRUE))
  sim1 <- df1 %>%
    simulate(y1 = sim_normal(mean=~ grp1 + grp2,
                             sd=~ grp2,
                             params = list(mean = list(grp1 = c("C" = 10, "A" = -20, "B" = 3),
                                                       grp2 = c("a" = 10)),
                                           sd = list(grp2 = c("a" = 1)))))
  sim2 <- df1 %>%
    simulate(y1 = sim_normal(mean=~ grp1 + grp2,
                             sd=~ grp2) %>%
               params("mean", grp1 = c("C" = 10, "A" = -20, "B" = 3),
                      grp2 = c("a" = 10)) %>%
               params("sd", grp2 = c("a" = 1)))

  expect_equal(tapply(sim1$y1, list(sim1$grp1, sim1$grp2), mean),
               matrix(c(-10, 13, 20, -20, 3, 10), ncol = 2), tolerance = 0.01,
               ignore_attr = TRUE)

  expect_equal(tapply(sim2$y1, list(sim2$grp1, sim2$grp2), mean),
               matrix(c(-10, 13, 20, -20, 3, 10), ncol = 2), tolerance = 0.01,
               ignore_attr = TRUE)

  sim3 <- df1 %>%
    simulate(y1 = sim_normal(mean = ~grp1, sd = ~grp2) %>%
               params("mean", grp1 = c("A" = 10, "B" = 3),
                              grp2 = c("a" = 3)) %>%
               params("sd", grp2 = c(1, 2)),
             y2 = sim_normal())

  expect_equal(tapply(sim3$y1, list(sim3$grp1, sim3$grp2), mean),
               matrix(c(10, 3, 0, 10, 3, 0), ncol = 2), tolerance = 0.01,
               ignore_attr = TRUE)
  expect_equal(tapply(sim3$y1, list(sim3$grp1, sim3$grp2), sd),
               matrix(c(1, 1, 1, 2, 2, 2), ncol = 2), tolerance = 0.01,
               ignore_attr = TRUE)

  # TODO: fix needed for below
  sim4 <- df1 %>%
    simulate(y = sim_form(~grp1 + 3 * grp2) %>%
               params(grp1 = sim_normal(0, 100),
                      grp2 = sim_bernoulli(0.5)))

  tapply(sim4$y, list(sim4$grp1, sim4$grp2), mean)

  sim5 <- df1 %>%
    simulate(y = sim_form(~grp1 + grp2) %>%
               params(grp1 = c("C" = 10, "A" = -20),
                      grp2 = sim_normal(0, 1)))


})
