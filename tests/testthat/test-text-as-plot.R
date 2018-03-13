context("text-as-plot")
example1 <- c("Hello", "a nice day to", "you, Stranger!")
example2 <- capture.output(stem(mtcars$hp))

test_that("multiplication works", {
  text_to_df(example1)
})
