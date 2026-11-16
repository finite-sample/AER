## tobit() must use the 'data' of the caller, also when it is called inside a
## function and the formula was created elsewhere (e.g., in the global
## environment) where an object of the same name exists.
library("AER")
data("Affairs", package = "AER")
d <- Affairs
d$w <- rep(c(0.5, 1, 2), length.out = nrow(d))
ft <- affairs ~ age + yearsmarried + religiousness + occupation + rating
fem <- subset(d, gender == "female")
ref <- tobit(ft, data = fem, weights = w)
fit_in_function <- function(d) tobit(ft, data = d, weights = w)
m <- fit_in_function(fem)
stopifnot(
  nobs(m) == nrow(fem),
  all.equal(coef(m), coef(ref)),
  all.equal(sandwich(m), sandwich(ref)),
  all.equal(sum(weights(m)), sum(fem$w))
)
## the same applies to fitting by group with lapply()
by_gender <- lapply(split(d, d$gender), function(d) coef(tobit(ft, data = d)))
stopifnot(
  all.equal(by_gender$female, coef(tobit(ft, data = fem))),
  all.equal(by_gender$male, coef(tobit(ft, data = subset(d, gender == "male"))))
)
