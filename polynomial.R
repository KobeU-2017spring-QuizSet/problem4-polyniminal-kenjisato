
# Solution ----------------------------------------------------------------


polynomial <- function(coeff) {
  # Parameter: coeff = c(a0, a1, ..., an)
  # Value: p(x) = a0 + a1 x + ... + an x^n
  p <- function(x) {
    power <- seq_along(coeff) - 1
    sum(coeff * (x ^ power))
  }
}

coeff <- c(1, 0, 1)
p <- polynomial(coeff)
print(vapply(1:10, p, numeric(1)))



# Small tests -------------------------------------------------------------

## 1 

coeff1 <- c(1, 2, 3, 4)
p1 <- polynomial(coeff1)

## 2

coeff2 <- c(0, 0, 0, -3, 0)
p2 <- polynomial(coeff2)

## Run test

e <- try(library(testthat), silent = TRUE)
if (class(e) == "try-error") {
  warning("Skip tests...")
} else {
  context("1")
  expect_equal(p1(0), coeff1[1])
  expect_equal(p2(0), coeff2[1])
  expect_equal(p1(3), 
               coeff1[1] + coeff1[2] * 3 + coeff1[3] * 3 ^ 2 + coeff1[4] * 3 ^ 3)
}

  
