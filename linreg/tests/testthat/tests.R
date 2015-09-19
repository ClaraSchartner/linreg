#this test will fail because they are not absolutely identical.

data("faithful")
m<-lm(eruptions~waiting,faithful)
n<-linreg_function(eruptions~waiting,faithful)
test_that("incorrect",{
 expect_less_than(sum(as.numeric(unlist(m[[1]]))-as.numeric(unlist(n[[1]]))),0.001)
})


data("iris")
lm.iris<-lm(Sepal.Length~Sepal.Width+Petal.Length^2,iris)
lin.iris<-  linreg_function(Sepal.Length~Sepal.Width+Petal.Length^2,iris)
test_that("cubic is correct",{
expect_less_than(sum(as.numeric(unlist(lm.iris[[1]]))-as.numeric(unlist(lin.iris[[1]]))),0.001)
})

test_that("not correct class",{
  expect_identical(class(lin.iris),"linreg")
})

r<-plot(lin.iris)
test_that("not ggplot",{
  expect_identical(class(r),c("gg","ggplot"))
})