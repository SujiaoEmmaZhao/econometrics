
### Exploring panel data

library(foreign)
Panel <- read.dta("http://dss.princeton.edu/training/Panel101.dta")
# conditioning plots
coplot(y ~ year|country, type="l", data=Panel) # Lines
coplot(y ~ year|country, type="b", data=Panel) # Points and lines

# scatter plot
library(car)
scatterplot(y~year|country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=Panel)



### Fixed Effects Model
#Heterogeneity across countries and years
library(gplots)
# plotmeans draw a 95% confidence interval around the means
plotmeans(y ~ country, main="Heterogeineity across countries", data=Panel) 
plotmeans(y ~ year, main="Heterogeineity across years", data=Panel)





### OLS regression
library(stargazer)
ols <-lm(y ~ x1, data=Panel)
#summary(ols)
stargazer(ols,type = "text", digits=4)

plot(Panel$x1, Panel$y, pch=19, xlab="x1", ylab="y")
abline(lm(Panel$y~Panel$x1),lwd=3, col="red")




### Least squares dummy variable model
fixed.dum <-lm(y ~ x1 + factor(country) - 1, data=Panel)
#summary(fixed.dum)
stargazer(ols,fixed.dum, type = "text", digits=4)

yhat <- fixed.dum$fitted
library(car)
scatterplot(yhat~Panel$x1|Panel$country, boxplots=FALSE, xlab="x1", ylab="yhat",smooth=FALSE)
abline(lm(Panel$y~Panel$x1),lwd=3, col="red")

# Each component of the factor variable (country) is absorbing the effects particular to each country. Predictor x1 was not significant in the OLS model, once controlling for differences across countries, x1 became significant in the OLS_DUM (i.e. LSDV model).




### Fixed effects: n entity-specific intercepts (using plm)
library(plm)
fixed <- plm(y ~ x1, data=Panel, index=c("country", "year"), model="within")
summary(fixed)
stargazer(ols,fixed.dum,fixed, type = "text", digits=4)

fixef(fixed) # Display the fixed effects 

### Fixed Effects or OLS

pFtest(fixed, ols) # Testing for fixed effects, null: OLS better than fixed
# So the fixed effects model is a better 




### Random effects: Random Intercept, Partial Pooling Model

random <- plm(y ~ x1, data=Panel, index=c("country", "year"), model="random")
summary(random)
# Interpretation of the coefficients is tricky
# since they include both the within-entity and between-entity effects.
# In the case of TSCS data represents the average effect of X over Y 
# when X changes across time and between countries by one unit
stargazer(fixed,random, type = "text", digits=4)

# Or alternatively
# Setting as panel data (an alternative way to run the above model
Panel.set <- plm.data(Panel, index = c("country", "year"))
# Random effects using panel setting (same output as above)
random.set <- plm(y ~ x1, data = Panel.set, model="random")
summary(random.set)
stargazer(random.set,random, type = "text", digits=4)




### Fixed Effects or Random effects

# To decide between fixed or random effects you can run a Hausman test where the null hypothesis is that the preferred model is random effects vs. the alternative the fixed effects.
# It basically tests whether the unique errors (ui) are correlated with the regressors, the null hypothesis is they are not.


# Run a fixed effects model and save the estimates, then run a random model and save the estimates, then perform the test. 
# If the p-value is significant (for example <0.05) then use fixed effects, if not use random effects.

phtest(fixed, random)



### Other Tests/Diagnostics

## 1. Testing for time-fixed effects
fixed.time <- plm(y ~ x1 + factor(year), data=Panel, index=c("country","year"), model="within")
summary(fixed.time)

# Testing time-fixed effects-The null is that no time-fixed effects needed
pFtest(fixed.time, fixed)
# In this example, no need to use time-fixed effects.
plmtest(fixed, c("time"), type=("bp"))


## 2. Random effects or OLS 
# Breusch-Pagan Lagrange multiplier (LM)

# Regular OLS (pooling model) using plm
pool <- plm(y ~ x1, data=Panel, index=c("country", "year"), model="pooling")
stargazer(ols,pool, type = "text", digits=4)

# Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better).
plmtest(pool, type=c("bp"))

# Here we failed to reject the null. 
# There is no evidence of significant differences across countries, 
# Therefore you can run a simple OLS regression.



## 3. Testing for cross-sectional dependence/contemporaneous correlation
# Breusch-Pagan LM test of independence and Pasaran CD test

# According to Baltagi, cross-sectional dependence is a problem in macro panels with long time series.
# This is not much of a problem in micro panels (few years and large number of cases).
# The null hypothesis in the B-P/LM and Pasaran CD tests of independence is that residuals across
# entities are not correlated. B-P/LM and Pasaran CD (cross-sectional dependence) tests are used to test
# whether the residuals are correlated across entities*. Cross-sectional dependence can lead to bias in
# tests results (also called contemporaneous correlation). 

fixed <- plm(y ~ x1, data=Panel, index=c("country", "year"), model="within")
pcdtest(fixed, test = c("lm"))
# No cross-sectional dependence
pcdtest(fixed, test = c("cd"))
# No cross-sectional dependence


## 4. Testing for serial correlation
# Serial correlation tests apply to macro panels with long time series. Not a problem in micro
# panels (with very few years). The null is that there is not serial correlation.

pbgtest(fixed)
# No serial correlation


## 5. Testing for heteroskedasticity

# The null hypothesis for the Breusch-Pagan test is homoskedasticity
library(lmtest)
bptest(y ~ x1 + factor(country), data = Panel, studentize=F)
# Presence of heteroskedasticity 


## 6. Correct for heteroskedasticity

# The --vcovHC- function estimates three heteroskedasticity-consistent covariance estimators:
#   "white1" - for general heteroskedasticity but no serial correlation. Recommended for random effects.
#   "white2" - is "white1" restricted to a common variance within groups. Recommended for random effects.
#    "arellano" - both heteroskedasticity and serial correlation. Recommended for fixed effects.


# The following options apply*:
#    HC0 - heteroskedasticity consistent. The default.
#    HC1,HC2, HC3 - Recommended for small samples. HC3 gives less weight to influential observations.
#    HC4 - small samples with influential observations
#    HAC - heteroskedasticity and autocorrelation consistent (type ?vcovHAC for more details)

random <- plm(y ~ x1, data=Panel, index=c("country", "year"), model="random")
coeftest(random) # Original coefficients
coeftest(random, vcovHC) # Heteroskedasticity consistent coefficients
coeftest(random, vcovHC(random, type = "HC3")) # Heteroskedasticity consistent coefficients, type 3
# The following shows the HC standard errors of the coefficients
t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(random, type = x)))))


fixed <- plm(y ~ x1, data=Panel, index=c("country", "year"), model="within")
coeftest(fixed) # Original coefficients
coeftest(fixed, vcovHC) # Heteroskedasticity consistent coefficients
coeftest(fixed, vcovHC(fixed, method = "arellano")) # Heteroskedasticity consistent coefficients (Arellano)
coeftest(fixed, vcovHC(fixed, type = "HC3")) # Heteroskedasticity consistent coefficients, type 3
# The following shows the HC standard errors of the coefficients
t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(fixed, type = x)))))




