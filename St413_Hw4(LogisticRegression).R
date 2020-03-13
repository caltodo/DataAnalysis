#St 413 HW 4

library(Sleuth3)
library(ggplot2)
#21.9.10

death <- case1902
death

death$logitprop = log((death$Death + .5) / ( death$NoDeath + .5))
death

death$Response <- cbind(death$Death, death$NoDeath)

#a
ggplot(death)+
  geom_point(aes(x = Aggravation, y = logitprop))

#b
death$logitprop = log((death$Death) / (death$NoDeath))
mod_1 <- glm(formula = Response ~ Aggravation + Victim, data = death, family = "binomial")
summary(mod_1)

#c

#Testing Indicator
#Reduced Model: 
mod_reduced <- glm(formula = Response ~ Aggravation, data = death, family ="binomial")
summary(mod_reduced)

1-pchisq(mod_1$deviance, mod_reduced$df.residual)
#P-val high => Full model is adequate

#d
#testing indicator with wald test
summary(mod_1)
#P-val < 2*10^-16 ==> Significant and should not be zero

#e
#95% CI = pt est +- (1.96) (SE)
# = 1.5397 +- (1.96 * .1867)
# = (1.17 , 1.91)
#log back: (3.23, 6.72)

#conclusion:
# It is estimated white victim related crimes increase the odds of death sentence by 
#a multiplicative change of between 3.23 and 6.72. You are between 3.23 and 6.72 times
# more likely to receive the death penalty for a white victim related crime.


#21.9.16


fish <- ex2116
fish

ggplot(fish)+
  geom_point(aes(x = Dose, y = Tumor))

fish$Response <- cbind(fish$Tumor, fish$Total - fish$Tumor)

mod_fish_1 <- glm(formula = Response ~ Dose, data = fish, family = "binomial")
summary(mod_fish)

#extra bin var

mod_fish_2 <- glm(formula = Response ~ Dose, data = fish, family = "quasibinomial")
summary(mod_fish)

anova(mod_fish_2, mod_fish_1, test = "F")
#The models are the same, use regular

#for every k-fold increase in beta ==> k^beta
#1.5^(14.33) ==> For every 50% Increase in Dose the odds of liver tumors for 
#trout increase by 333.72.

#log(1) = -.867 + 14.33x
#x = .0605
# with .605 dose, the odds of a liver tumor are 50/50.

