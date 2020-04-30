test_that("Domain is correctly validated with function", {
  f <- mosaic::makeFun(fred * ginger + gene ~ fred & ginger, gene=3)
  Dom1 <- domain(fred = c(0,1), ginger = c(-2, 2))
  expect_equal("",
               mosaicUSAFA:::validate_domain(Dom1,c("ginger", "fred")))
  expect_error(mosaicUSAFA:::validate_domain(Dom1,c("gene", "fred")))
  expect_error(mosaicUSAFA:::validate_domain(Dom1, "fred"))
})

test_that("Function is properly  evaluated on grid",  {
  grid <- mosaicUSAFA:::eval_on_domain(x * y ~ x & y,  domain(x=c(0,1), y = c(-1,1)),  n=5)
  expect_equal(25, nrow(grid))
  expect_equal(c(0,1), range(grid$x))
  expect_equivalent(c("x", "y",  ".output."), names(grid))
  expect_equal(c(-1,  1), range(grid$.output))
})

test_that("Functions of one variable work.", {
  grid <- mosaicUSAFA:::eval_on_domain(x^2 + 3 ~ x,  domain(x=c(-2,2)), n=5)
  expect_equivalent(c("x",  ".output."), names(grid))
})

test_that("eval_on_domain checks variable  names", {
  expect_error(mosaicUSAFA:::eval_on_domain(x ~ x,  domain(xx=c(0,1))))
})
