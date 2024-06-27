library(wooldridge)
library(tidyverse)
data(wage2)
View(wage2)

dat<-wage2 %>% select(wage, educ, IQ, exper, feduc) %>% drop_na()

library(stargazer)
stargazer( dat,  
           type = "text", 
           keep = c(1, 5),  
           omit.summary.stat = c("p25", "p75"), 
           covariate.labels = c("Wage", "Education"),
           digits = 4 )

full.model  <- lm(dat$wage ~ dat$educ + dat$IQ + dat$exper ) # Full Model if we were able to measure ability
naive.model <- lm(dat$wage ~ dat$educ + dat$exper )      # Naive Model where ability is excluded (omitted variable bias)

stargazer( full.model, naive.model, 
           type = "text", 
           dep.var.labels = ("Wage"),
           column.labels = c("Full Model", "Naive Model"),
           covariate.labels = c("Education", "Ability", "Experience"),
           omit.stat = "all", 
           digits = 2 )


par(mfrow=c(1,3) )
plot(dat$feduc, dat$educ, xlab = "Father's education", ylab = "Education" )   # Plot
abline(lm(dat$educ ~ dat$feduc ), col = "red")                              # regression line 
plot(dat$feduc, dat$wage, xlab = "Father's education", ylab = "Wage" )
abline(lm(dat$wage ~ dat$feduc ), col = "green") 
plot(dat$feduc, dat$IQ, xlab = "Father's education", ylab = "Ability")
abline(lm(dat$IQ ~ dat$feduc ), col = "blue")


first.stage  <- lm(dat$educ ~ dat$feduc + dat$expe)        #Run the first stage predict the education variable

stargazer( first.stage,
           type = "text", 
           dep.var.labels = ("Education"),
           column.labels = c("First stage"),
           omit.stat = c("adj.rsq", "ser"), 
           covariate.labels = c("Father's education", "Experience"),
           digits = 2 )


dat$x1_hat <- fitted(first.stage)   
#Saved the predicted values of education from the first stage

second.stage <- lm(dat$wage ~ dat$x1_hat + dat$exper )   
#Run the 2nd stage using the predicted values of education to look at the effect on income.


stargazer( full.model, naive.model, second.stage,
           type = "text", 
           dep.var.labels = ("Wage"),
           column.labels = c("Full Model", "Naive Model", "IV model"),
           omit.stat = c("adj.rsq", "ser"), 
           covariate.labels = c("Education", "Ability", "Predicted Education", "Experience"),
           digits = 2 )
