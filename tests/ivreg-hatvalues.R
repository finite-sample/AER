## hatvalues() and hence vcovHC(type = "HC2"/"HC3") for weighted ivreg() fits.
## With the regressors as their own instruments, ivreg() is weighted least
## squares and must reproduce lm().
library("AER")
data("CigarettesSW", package = "AER")
cig <- transform(subset(CigarettesSW, year == "1995"),
  lpacks = log(packs), lrprice = log(price/cpi), lrincome = log(income/population/cpi),
  tdiff = (taxs - tax)/cpi, w = population/mean(population))
m_lm <- lm(lpacks ~ lrprice + lrincome, data = cig, weights = w)
m_iv <- ivreg(lpacks ~ lrprice + lrincome | lrprice + lrincome, data = cig, weights = w)
stopifnot(
  all.equal(hatvalues(m_iv), hatvalues(m_lm)),
  all.equal(vcovHC(m_iv, type = "HC2"), vcovHC(m_lm, type = "HC2")),
  all.equal(vcovHC(m_iv, type = "HC3"), vcovHC(m_lm, type = "HC3"))
)
## genuine IV: the weighted hat matrix W^(1/2) X (Xz' W Xz)^(-1) Xz' W^(1/2)
## is a projection, so its trace is the number of coefficients
m <- ivreg(lpacks ~ lrprice + lrincome | lrincome + tdiff, data = cig, weights = w)
X <- model.matrix(m, component = "regressors")
Xz <- model.matrix(m, component = "projected")
sw <- sqrt(cig$w)
H <- (sw * X) %*% solve(crossprod(sw * Xz)) %*% t(sw * Xz)
stopifnot(
  all.equal(unname(hatvalues(m)), unname(diag(H))),
  all.equal(sum(hatvalues(m)), 3)
)
## unit weights do not change anything
m1 <- ivreg(lpacks ~ lrprice + lrincome | lrincome + tdiff, data = transform(cig, one = 1), weights = one)
m0 <- ivreg(lpacks ~ lrprice + lrincome | lrincome + tdiff, data = cig)
stopifnot(all.equal(hatvalues(m1), hatvalues(m0)))
