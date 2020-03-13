#ST 413 HW 6 Scratch

#1.1 (using output)
#a
# e^-.116 = .89
#It is estimated that being male has a 11% decrease in the rate of doctor visit

#b
#since there is a lot of deviance, but low p-values for every 
# explanatory var, there is evidence to suggest extra poisson variation
#disp param = resid dev / n-p = 23527 / 4400 = 5.35
#coefficient after quasi poisson = the same thing = -.116

#c
#hosp = 0.165757 --> e^.165 = 1.18
# how does it differ between genders
#female: .166 --> e^... = 1.18
#male: 0.165757 - 0.136451 = .029 -> e^.. = 1.029


#for each unit increase in hospital visits, females are estimated to have an 18% increase
# in physicians office visits, while males are estimated to have a 2.9% increase.


#d
#e^.0387 = 1.039 => 3.9% increase
#interaction means what now?
#hosp + interaction x gender
# = 0.165757 + 0.038742 x -0.136451
# = .161 ? 
#For each unit increase in hospital visits, it is estimated that the difference
#between male and female rates of physician visits increase by 3.9%. 


#22.18
library(Sleuth3)
#birds data

birds<-ex1220
#a

#Log transform Area, Elev, and DistNear, AreaNear
birds$logArea <- log(birds$Area)
birds$logElev <- log(birds$Elev)
birds$logDistNear <- log(birds$DistNear)
birds$logANear <- log(birds$AreaNear)
birds$logDistSc <- log(birds$DistSc)
birds$invasive <- birds$Total - birds$Native
birds

m1 <- glm(formula = Native ~ logArea + logElev+ logDistNear + logANear, data = birds, family = "poisson")
summary(m1)

#check for extra poisson w quasi and look at dispersion param
m1_q <- glm(formula = Native ~ logArea + logDistNear + logANear + logElev, data = birds, family = "quasipoisson")
summary(m1_q)
#dispersion parameter large --> go with quasi


#b (dropterm to find the worst coef)
library(MASS)

dropterm(m1_q)

#dropping terms would result in a higher AIC, try dropping log elev

working_m <- glm(formula = Native ~ logArea + logDistNear + logANear, data = birds, family = "quasipoisson")
summary(working_m)

pchisq(working_m$deviance, df=working_m$df.residual, lower.tail=FALSE)

#c 
#describe effects of remaining coeffs
#logArea = .27 -> 1.31 --> 31% increase in rate of Native birds
#logDistNear = -.061 -> .94 --> 6% decrease in rate of natives
#logANear = -.05 -> .95 --> 5% decrease in rate of natives

#22.23
#shuttles data
shuttles <- ex2223
shuttles

m_shuttle <- glm(formula = Incidents ~ Temp, data = shuttles, family = "poisson")
summary(m_shuttle)
pchisq(m_shuttle$deviance, df=m_shuttle$df.residual, lower.tail=FALSE)

#p-val not bad --> could be extra poisson but there are no more vars

#temp = -.11 => e^.-11 = .896
#conf int = -.11 +- 1.96*.0394 --> -.11 +- .0772
# = (-.187 ,-.033)
#back transform: e^-.187, e^-.033
# = (.829, .968)

#In conclusion, it is estimated that for each 1 unit increase in temperature the 
# rate of a failure goes down by about 10.4%, with a 95% confidence interval of 
# 3.2% and 17.1%. This means temperature has a negative effect on failure.