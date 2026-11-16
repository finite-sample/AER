## ivreg() with an offset: fitting y with offset o must be equivalent to
## fitting y - o without offset, apart from fitted values (which include o).
library("AER")
set.seed(1)
n <- 200
d <- data.frame(z1 = rnorm(n), z2 = rnorm(n), u = rnorm(n), o = runif(n, 0, 3))
d$x <- d$z1 + 0.5 * d$z2 + d$u + rnorm(n)
d$y <- 1 + 2 * d$x + d$o + d$u
m0 <- ivreg(I(y - o) ~ x | z1 + z2, data = d)
m1 <- ivreg(y ~ x | z1 + z2, data = d, offset = o)
m2 <- ivreg(y ~ x + offset(o) | z1 + z2, data = d)
for(m in list(m1, m2)) stopifnot(
  all.equal(coef(m), coef(m0)),
  all.equal(residuals(m), residuals(m0)),
  all.equal(fitted(m), fitted(m0) + d$o, check.attributes = FALSE),
  all.equal(vcov(m), vcov(m0)),
  all.equal(sandwich(m), sandwich(m0)),
  all.equal(predict(m, newdata = d), fitted(m), check.attributes = FALSE),
  all.equal(summary(m, diagnostics = TRUE)$diagnostics,
            summary(m0, diagnostics = TRUE)$diagnostics)
)
