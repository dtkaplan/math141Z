test_that("various argument configurations work", {
  P <- contour_plot(
    x *  y ~ x & y,
    domain(x=c(0,1), y=c(-1,1))
    )
  expect_true(inherits(P,  "gg"))
  P2 <- contour_plot(
    sin(fred*ginger) ~ fred + ginger,
    domain(fred=range(0,2), ginger = range(0, pi))
  )
  expect_true(P2$labels$x == "fred")
  expect_true(P2$labels$y == "ginger")
})

test_that("surface  plots get made", {
  P1 <- surface_plot(
    sin(fred*ginger) ~ fred + ginger,
    domain(fred=range(0,2), ginger = range(0, pi)))
  expect_true(inherits(P1, "plotly"))
})


test_that("contour plot plays well with other layers", {
  P1 <-  contour_plot(mpg * abs(sin(hp/40)) ~  hp + mpg,
                    domain(hp = c(0,200), mpg=c(0,40)))
  expect_true(inherits(P1, "ggplot"))
  Existing_plot <- gf_point(mpg ~  hp, data = mtcars)
  P1 <- Existing_plot %>%
    contour_plot(mpg * abs(sin(hp/40)) ~  hp + mpg,
                 domain(hp = c(0,200), mpg=c(0,40)))
  expect_true(inherits(P1, "ggplot"))
  P1 <-
    contour_plot(mpg * abs(sin(hp/40)) ~  hp + mpg,
                 domain(hp = c(0,200), mpg=c(0,40))) %>%
    gf_point(mpg ~ hp, data = mtcars, color="red")
  expect_true(inherits(P1, "ggplot"))
  P2 <- P1 %>% contour_plot(mpg + hp ~ hp+mpg, filled=FALSE, contour_color="red")
  expect_true(inherits(P2, "ggplot"))
  P2 <- P1 %>%
    contour_plot(mpg + hp ~ hp+mpg, filled=FALSE,
                 contours_at = 10,
                 label_color = "red", contour_color="red")
})

test_that("Domain must be provided in some way", {
  P1 <-  contour_plot(mpg * abs(sin(hp/40)) ~  hp + mpg,
                      domain(hp = c(0,200)))
  expect_error(P1)
  P1 <- contour_plot(mpg * abs(sin(hp/40)) ~  hp + mpg,
                     domain(hp = c(0,200), x = c(10,20)))
  expect_error(P1)
})
# Need to
# 1. confirm that contour_plot() works with previous and following plot objects.
# 2. write surface_plot() which will just use plot_ly(type="surface")
# 3. confirm you can overlay contours from different functions.
# 4. find a black-and-white color scheme?
