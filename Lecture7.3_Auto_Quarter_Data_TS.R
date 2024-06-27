
library(haven) # import Stata data
library(stargazer)
library(lmtest)
library(sandwich)
library(stats)
library(nlme)

# load data and create time series
auto <- read_dta("auto_quarter_data.dta")
attach(auto)
incomets<-ts(income, start=c(1959, 1), end=c(1990, 4), freq=4)
pricets<-ts(price, start=c(1959, 1), end=c(1990, 4), freq=4)
gasts<-ts(gas, start=c(1959, 1), end=c(1990, 4), freq=4)

# run regressions
reg1<-lm(gasts~ incomets +pricets)

# detect auto correlation
plot(reg1$residuals)
plot(lag(reg1$residuals,1), reg1$residuals)
acf(residuals(reg1))

# formal tests for detecting correlation
dwtest(reg1)
bgtest(reg1)

# correct for standard errors
# Normal SE
coeftest(reg1)
# Newey West SE
coeftest(reg1, vcov = NeweyWest)


# use FGLS
mod.gls <- gls(gas~ income * price + I(income ^ 2) + I(price ^ 2),
               data=auto, correlation=corARMA(p=1), method="ML")
stargazer(mod.gls, type="text", digits=8)
