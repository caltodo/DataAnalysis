#Homework 6

ex1320
attach(ex1320)

#a
#plots for i) scores vs sex and ii) scores vs background
plot(Sex, Score)
plot(Background, Score)
#it appears background has a positive linear relationship with scores

#b: non-additive model
non_add <- lm(Score ~ Sex + Background + Sex:Background -1)
anova(non_add)
summary(non_add)

#c: 
interaction.plot(Sex,Background,Score)

#d: additive Model
additive <- lm(Score ~ Sex + Background)
anova(additive)
summary(additive)


#e 
#anova f test
anova(non_add, additive)
#p = .4629

#f: 95% CI for male, taking background into effect
confint(additive,2)
detach(ex1320)
