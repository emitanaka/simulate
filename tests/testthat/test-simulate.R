test_that("simulate", {
  df1 <- data.frame(grp1 = c("C", "A", "A", "A", "C", "C", "B", "B", "B", "A", "B", "B",
                   "A", "C", "C", "C", "B", "C", "B", "B", "A", "C", "B", "A", "C",
                   "A", "A", "A", "C", "C", "C", "B", "C", "C", "B", "B", "B", "C",
                   "A", "C", "A", "A", "B", "C", "B", "A", "C", "B", "B", "A", "A",
                   "B", "B", "B", "B", "A", "C", "A", "C", "B", "C", "A", "B", "C",
                   "A", "B", "C", "A", "C", "C", "C", "B", "B", "B", "C", "B", "C",
                   "C", "B", "A", "C", "C", "A", "A", "B", "B", "C", "B", "B", "B",
                   "A", "C", "A", "A", "B", "C", "B", "C", "C", "C"),
             grp2 = c("e", "g", "c", "c", "c", "e", "a", "e", "a", "g", "a", "c",
                      "a", "g", "b", "f", "g", "b", "f", "g", "d", "a", "g", "e", "d",
                      "d", "d", "d", "b", "g", "a", "g", "e", "b", "e", "b", "g", "e",
                      "f", "e", "d", "a", "a", "g", "b", "g", "e", "c", "f", "d", "b",
                      "e", "c", "b", "b", "b", "g", "b", "a", "d", "g", "g", "f", "a",
                      "c", "c", "c", "a", "a", "c", "b", "e", "a", "e", "d", "d", "f",
                      "c", "g", "g", "c", "g", "g", "c", "e", "c", "b", "c", "d", "c",
                      "a", "b", "f", "f", "d", "f", "b", "e", "d", "d"))
  simulate(df1, resp1 = sim_normal(mean=~ grp1, sd = 1,
                                   params = params(mean = list(grp1 = c("C" = 10, "A" = -20, "B" = 3)))))


})
