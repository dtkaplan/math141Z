test_that("slice plot plays well with other layers", {
  P1 <-  slice_plot(100 * abs(sin(hp/40)) ~  hp,
                    domain(hp = c(0,200)))
  expect_true(inherits(P1, "ggplot"))
  Existing_plot <- gf_point(mpg ~  hp, data = mtcars)
  P1 <- Existing_plot %>% slice_plot(100 * abs(sin(hp/40)) ~  hp)
  expect_true(inherits(P1, "ggplot"))
})

test_that("Domain must be provided in some way", {
  P1 <-  slice_plot(100 * abs(sin(hp/40)) ~  hp)
  expect_error(P1)
  P1 <- slice_plot(100 * abs(sin(hp/40)) ~ hp,
                   domain(x = range(0,200)))
  expect_error(P1)
})
