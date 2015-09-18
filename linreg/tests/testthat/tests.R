#this test will fail because they are not absolutely identical.

data("faithful")
m<-lm(eruptions~waiting,faithful)
n<- linreg(eruptions~waiting,faithful)
test_that("incorrect",{
 expect_less_than(sum(as.numeric(unlist(m[[1]]))-as.numeric(unlist(n[[1]]))),0.001)
})


data("iris")
lm.iris<-lm(Sepal.Length~Sepal.Width+Petal.Length^2,iris)
lin.iris<-  linreg(Sepal.Length~Sepal.Width+Petal.Length^2,iris)
test_that("cubic is correct",{
expect_less_than(sum(as.numeric(unlist(lm.iris[[1]]))-as.numeric(unlist(lin.iris[[1]]))),0.001)
})