######################################
#                                    #
#   Mixed effects modeling in R     #
#                                    #
######################################

## authors: Gabriela K Hajduk, based on workshop developed by Liam Bailey
## contact details: gkhajduk.github.io; email: gkhajduk@gmail.com
## date: 2017-03-09
##

###---- Explore the data -----###

## load the data and have a look at it

load("dragons.RData")

head(dragons)

## Let's say we want to know how the body length affects test scores.

## Have a look at the data distribution:

hist(dragons$testScore)  # seems close to normal distribution - good!

## It is good practice to  standardise your explanatory variables before proceeding - you can use scale() to do that:

dragons$bodyLength2 <- scale(dragons$bodyLength)

## Back to our question: is test score affected by body length?

###---- Fit all data in one analysis -----###

## One way to analyse this data would be to try fitting a linear model to all our data, 
# ignoring the sites and the mountain ranges for now.
# install.packages("lme4")
library(lme4)

basic.lm <- lm(testScore ~ bodyLength2, data = dragons)

summary(basic.lm)

## Let's plot the data with ggplot2

library(ggplot2)

ggplot(dragons, aes(x = bodyLength, y = testScore)) +
  geom_point()+
  geom_smooth(method = "lm")


### Assumptions?

## Plot the residuals - the red line should be close to being flat, like the dashed grey line

plot(basic.lm, which = 1)  # not perfect, but look alright

## Have a quick look at the  qqplot too - point should ideally fall onto the diagonal dashed line

plot(basic.lm, which = 2)  # a bit off at the extremes, but that's often the case; again doesn't look too bad


## However, what about observation independence? Are our data independent?
## We collected multiple samples from eight mountain ranges
## It's perfectly plausible that the data from within each mountain range are more similar to each other than the data from different mountain ranges - they are correlated. Pseudoreplication isn't our friend.

## Have a look at the data to see if above is true
boxplot(testScore ~ mountainRange, data = dragons)  # certainly looks like something is going on here

## We could also plot it colouring points by mountain range
ggplot(dragons, aes(x = bodyLength, y = testScore, colour = mountainRange))+
  geom_point(size = 2)+
  theme_classic()+
    theme(legend.position = "none")

## From the above plots it looks like our mountain ranges vary both in the dragon body length and in their test scores. This confirms that our observations from within each of the ranges aren't independent. We can't ignore that.

## So what do we do?

###----- Run multiple analyses -----###


## We could run many separate analyses and fit a regression for each of the mountain ranges.

## Lets have a quick look at the data split by mountain range
## We use the facet_wrap to do that

ggplot(aes(bodyLength, testScore), data = dragons) + geom_point() +
    facet_wrap(~ mountainRange) +
    xlab("length") + ylab("test score")



##----- Modify the model -----###

## We want to use all the data, but account for the data coming from different mountain ranges

## let's add mountain range as a fixed effect to our basic.lm

mountain.lm <- lm(testScore ~ bodyLength2 + mountainRange, data = dragons)
summary(mountain.lm)

## now body length is not significant


###----- Mixed effects models -----###
## A Mixed model is a good choice here: it will allow us to use all the data we have (higher sample size) and 
## account for the correlations between data coming from the sites and mountain ranges. 
## We will also estimate fewer parameters and avoid problems with multiple comparisons that we would encounter 
## while using separate regressions.


##----- First mixed model -----##
## Now the underlying question changes to is there an association between dragon’s body length and the test score, 
## AFTER controlling for the variation in mountain ranges?

## We will fit the random effect using (1|variableName):  
### model
mixed.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)
summary(mixed.lmer)
### plots
plot(mixed.lmer)  # looks alright, no paterns evident
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))  # points fall nicely onto the line - good!

### summary
summary(mixed.lmer)

### variance accounted for by mountain ranges
## We can see the variance for mountainRange = 339.7. Mountain ranges are clearly important: they explain a lot of 
## variation. How do we know that? We can take the variance for the mountainRange and divide it by the total variance:
339.7/(339.7 + 223.8)  # ~60 %

## So the differences between mountain ranges explain ~60% of the variance. 
## Do keep in mind that’s 60% of variance “left over” after the variance explained by our fixed effects.

##-- implicit vs explicit nesting --##

head(dragons)  # we have site and mountainRange
str(dragons)  # we took samples from three sites per mountain range and eight mountain ranges in total

### create new "sample" variable
dragons <- within(dragons, sample <- factor(mountainRange:site))
str(dragons$sample)

##----- Second mixed model -----##

### model
mixed.WRONG <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|site), data = dragons)  
# treats the two random effects as if they are crossed

## Our question gets adjusted slightly again: Is there an association between body length and intelligence in dragons 
## after controlling for variation in mountain ranges and sites within mountain ranges?
mixed.lmer2 <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), data = dragons)  
# the syntax stays the same, but now the nesting is taken into account

## For the record, you could also use the below syntax, but I’d advise you to set out your variables properly and 
## make sure nesting is stated explicitly within them, that way you don’t have to remember to specify the nesting.

# (1|mountainRange/site) or even (1|mountainRange) + (1|mountainRange:site)

### summary
summary(mixed.lmer2)

### plot
ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
  facet_wrap(~mountainRange, nrow=3) +
  geom_point() +
  theme_classic() +
  geom_line(data = cbind(dragons, pred = predict(mixed.lmer2)), aes(y = pred)) +
  theme(legend.position = "none")

## install.packages("stargazer")
library(stargazer)
stargazer(mixed.lmer2, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

##----- Model selection for the keen -----##

### full model
full.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), 
                  data = dragons, REML = FALSE)

### reduced model
reduced.lmer <- lmer(testScore ~ 1 + (1|mountainRange) + (1|sample), 
                     data = dragons, REML = FALSE)
### comparison
anova(reduced.lmer, full.lmer)  # the two models are not significantly different
