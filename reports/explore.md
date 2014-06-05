
```r
library(ProjectTemplate)
load.project()
```

```
## Loading project configuration
```

```
## Error: You are missing a configuration file: config/global.dcf
```

```r

```


# Basic exploration

```r
str(rdyads)
```

```
## Error: object 'rdyads' not found
```

```r
names(rdyads)
```

```
## Error: object 'rdyads' not found
```

```r
rdyads$X
```

```
## Error: object 'rdyads' not found
```

```r

head(rdyads)
```

```
## Error: object 'rdyads' not found
```

```r
# General description of dataset
Hmisc::describe(rdyads[, 4:ncol(rdyads)])
```

```
## Error: object 'rdyads' not found
```

```r

# * Four leagues in England, France, Germany and Spain * Skim colour is
# skewed with on around 16% in the 4 or 5 zone; There's a question about how
# to code this core predictor * 11 different detailed positions and 4
# general positions

hist(rdyads$PlayerHeightCM)
```

```
## Error: object 'rdyads' not found
```

```r
# * Height is normally distributed


# check the one club/league/skincol/etc per player assumption (it's true)
for (i in names(rdyads)) {
    print(i)
    print(table(aggregate(rdyads[, i], list(rdyads$PlayerID), function(X) length(unique(X)))[, 
        2]))
}
```

```
## Error: object 'rdyads' not found
```

```r


# check ref features (refs referee in multiple leagues)
for (i in names(rdyads)) {
    print(i)
    print(table(aggregate(rdyads[, i], list(rdyads$RefereeID), function(X) length(unique(X)))[, 
        2]))
}
```

```
## Error: object 'rdyads' not found
```

```r


# number of matches per combination
hist(rdyads$Matches)
```

```
## Error: object 'rdyads' not found
```

```r
table(rdyads$Matches)
```

```
## Error: object 'rdyads' not found
```

```r
x <- data.frame(table(rdyads$Matches))
```

```
## Error: object 'rdyads' not found
```

```r
x$Var1 <- as.numeric(x$Var1)
```

```
## Error: object 'x' not found
```

```r
x$combined <- x$Var1 * x$Freq
```

```
## Error: object 'x' not found
```

```r
x$FreqProp <- x$Freq/sum(x$Freq)
```

```
## Error: object 'x' not found
```

```r
x$combinedProp <- x$combined/sum(x$combined)
```

```
## Error: object 'x' not found
```

```r
round(x, 3)
```

```
## Error: object 'x' not found
```

```r

# * distribution of matches is very skewed * Although it is less so the case
# at the match level; e.g., while the interaction rows are based on a single
# match, matches in general are involve player and refs who will eventually
# meet 2 or more times 82% of the time.

# What is a YellowRed?
table(YellowRed = rdyads$YellowRed, RedCards = rdyads$RedCards)
```

```
## Error: object 'rdyads' not found
```

```r
table(YellowRed = rdyads$YellowRed, YellowCards = rdyads$YellowCards)
```

```
## Error: object 'rdyads' not found
```

```r
# I thought it might have been a second yellow card that results in being
# asked to leave the ground However, this would only make sense if the first
# yellow card in didn't count in the yellowcard or redcard leger.
```






```r
# number of player IDs
length(unique(rdyads$PlayerID))
```

```
## Error: object 'rdyads' not found
```

```r

# number of dyads per player
length(unique(rdyads$RefereeID))
```

```
## Error: object 'rdyads' not found
```


There are a little over two thousand refs and two thousand players.



```r
# a quick look at the core research question
summary(lm(RedCardsProp ~ SkinCol, rdyads))
```

```
## Error: object 'rdyads' not found
```

```r
summary(lm(RedCardsProp ~ SkinCol, rdyads, weights = Matches))
```

```
## Error: object 'rdyads' not found
```

```r
summary(lm(YellowCardsProp ~ SkinCol, rdyads))
```

```
## Error: object 'rdyads' not found
```

```r
summary(lm(YellowRedProp ~ SkinCol, rdyads))
```

```
## Error: object 'rdyads' not found
```

```r

plot(aggregate(rdyads$RedCardsProp, list(rdyads$SkinCol), mean, na.rm = TRUE))
```

```
## Error: object 'rdyads' not found
```

```r
plot(aggregate(rdyads$YellowCardsProp, list(rdyads$SkinCol), mean, na.rm = TRUE))
```

```
## Error: object 'rdyads' not found
```

```r
plot(aggregate(rdyads$YellowRedProp, list(rdyads$SkinCol), mean, na.rm = TRUE))
```

```
## Error: object 'rdyads' not found
```

```r


aggregate(rplayers$RedCardsSumProp, list(rplayers$SkinCol), mean, na.rm = TRUE)
```

```
## Error: object 'rplayers' not found
```

```r
aggregate(rplayers$YellowCardsSumProp, list(rplayers$SkinCol), mean, na.rm = TRUE)
```

```
## Error: object 'rplayers' not found
```

```r
summary(lm(RedCardsSumProp ~ SkinCol, rplayers))
```

```
## Error: object 'rplayers' not found
```

```r

summary(lm(RedCardsSumProp ~ factor(SkinCol), rplayers[rplayers$MatchesSum > 
    100, ]))
```

```
## Error: object 'rplayers' not found
```


# Examination of the players dataset

```r
# general distribution of redcard proportions
plot(density(rplayers$MatchesSum))
```

```
## Error: object 'rplayers' not found
```

```r
plot(density(rplayers[rplayers$MatchesSum > 50, "RedCardsSumProp"]))
```

```
## Error: object 'rplayers' not found
```

```r
plot(density(rplayers[rplayers$MatchesSum > 100, "RedCardsSumProp"]))
```

```
## Error: object 'rplayers' not found
```

```r
plot(density(rplayers[rplayers$MatchesSum > 150, "RedCardsSumProp"]))
```

```
## Error: object 'rplayers' not found
```

```r
plot(density(rplayers[rplayers$MatchesSum > 200, "RedCardsSumProp"]))
```

```
## Error: object 'rplayers' not found
```

```r
# the distribution of redcards appears to be bimodal or possibly even
# trimodal some never get red flags, another mode exists around 0.5%, and a
# final small hump seems to be between 1.5 and 2%.

# one challenges is going to be the rare occurrence of red flags
plot(RedCardsSumProp ~ MatchesSum, rplayers)
```

```
## Error: object 'rplayers' not found
```

```r
# * the above plot shows lines indicating the effect of 1, 2, or 3 flags
# over match counts * I.e., what shrinkage should be used or how should this
# be incorporated into the model


# League by red cards
boxplot(RedCardsSumProp ~ League_Country, rplayers)
```

```
## Error: object 'rplayers' not found
```

```r
aggregate(RedCardsSumProp ~ League_Country, rplayers, mean)
```

```
## Error: object 'rplayers' not found
```

```r
summary(lm(RedCardsSumProp ~ League_Country, rplayers))
```

```
## Error: object 'rplayers' not found
```

```r
# * first impressions are that France gives out more red cards

# Position
boxplot(RedCardsSumProp ~ Position, rplayers[rplayers$MatchesSum > 100, ])
```

```
## Error: object 'rplayers' not found
```

```r
aggregate(RedCardsSumProp ~ Position, rplayers[rplayers$MatchesSum > 100, ], 
    mean)
```

```
## Error: object 'rplayers' not found
```

```r
summary(lm(RedCardsSumProp ~ Position, rplayers[rplayers$MatchesSum > 100, ]))
```

```
## Error: object 'rplayers' not found
```

```r
# Although defence looks like it gets more red flags than other areas but
# the difference might not be signficant or at least it would require
# further analysis.

boxplot(RedCardsSumProp ~ Position_Detailed, rplayers[rplayers$MatchesSum > 
    100, ])
```

```
## Error: object 'rplayers' not found
```

```r
aggregate(RedCardsSumProp ~ Position_Detailed, rplayers[rplayers$MatchesSum > 
    100, ], mean)
```

```
## Error: object 'rplayers' not found
```

```r
summary(lm(RedCardsSumProp ~ Position_Detailed, rplayers[rplayers$MatchesSum > 
    100, ]))
```

```
## Error: object 'rplayers' not found
```

```r

# year of birth/age
with(rplayers[rplayers$MatchesSum > 100, ], scatter.smooth(BirthY, RedCardsSumProp))
```

```
## Error: object 'rplayers' not found
```

```r
# * nothing much


with(rplayers[rplayers$MatchesSum > 100, ], scatter.smooth(BirthY, RedCardsSumProp))
```

```
## Error: object 'rplayers' not found
```

```r

with(rplayers[rplayers$MatchesSum > 100, ], scatter.smooth(YellowCardsSumProp, 
    RedCardsSumProp))
```

```
## Error: object 'rplayers' not found
```

```r

# There is an association between prop yellow cards and prop red cards

with(rplayers[rplayers$MatchesSum > 50, ], scatter.smooth(MatchesSum, RedCardsSumProp))
```

```
## Error: object 'rplayers' not found
```

```r
# * slight hint that the proprtion might go up for people with more matches

with(rplayers[rplayers$MatchesSum > 100, ], scatter.smooth(PlayerHeightCM, RedCardsSumProp))
```

```
## Error: object 'rplayers' not found
```

```r

# club
x <- aggregate(RedCardsSumProp ~ Club, rplayers[rplayers$MatchesSum > 100, ], 
    mean)
```

```
## Error: object 'rplayers' not found
```

```r
plot(density(x[, 2]))
```

```
## Error: object 'x' not found
```

```r
# density of cards per club is fairly normal with a couple of little bumps
# like the player distributions
summary(lm(RedCardsSumProp ~ Club, rplayers[rplayers$MatchesSum > 100, ]))
```

```
## Error: object 'rplayers' not found
```

```r


# examination of skin colour
plot(table(rplayers$SkinCol))
```

```
## Error: object 'rplayers' not found
```

```r
round(prop.table(table(rplayers$Position, rplayers$SkinCol), 1), 2)
```

```
## Error: object 'rplayers' not found
```

```r
# perhaps strikers are more likely to have darker skin
round(prop.table(table(rplayers$League_Country, rplayers$SkinCol), 1), 2)
```

```
## Error: object 'rplayers' not found
```

```r
# France has the most black players followed by England; Germany and S

# quick examination of the country by skin colour interaction
summary(lm(RedCardsSumProp ~ I(SkinCol > 3) * League_Country, rplayers[rplayers$MatchesSum > 
    100, ]))
```

```
## Error: object 'rplayers' not found
```

```r
summary(lm(RedCardsSumProp ~ I(SkinCol > 3) * League_Country, rplayers[rplayers$MatchesSum > 
    50, ]))
```

```
## Error: object 'rplayers' not found
```

```r
summary(lm(RedCardsSumProp ~ I(SkinCol > 3) * League_Country, rplayers))
```

```
## Error: object 'rplayers' not found
```

```r

# Matches
with(rplayers[rplayers$MatchesSum > 20, ], scatter.smooth(log(MatchesSum), RedCardsSumProp))
```

```
## Error: object 'rplayers' not found
```

```r

with(rplayers[rplayers$MatchesSum > 20, ], scatter.smooth(GoalsSumProp, RedCardsSumProp))
```

```
## Error: object 'rplayers' not found
```

```r

head(rrefs)
```

```
## Error: object 'rrefs' not found
```

```r
table(rrefs$RefCountryID, )
```

```
## Error: object 'rrefs' not found
```



### Ref database

```r
head(rrefs)
```

```
## Error: object 'rrefs' not found
```

```r

plot(density(log(rrefs$MatchesSum, 10)))
```

```
## Error: object 'rrefs' not found
```

```r
plot(density(rrefs[rrefs$MatchesSum > 100, "RedCardsSumProp"]))
```

```
## Error: object 'rrefs' not found
```

```r
plot(density(rrefs[rrefs$MatchesSum > 200, "RedCardsSumProp"]))
```

```
## Error: object 'rrefs' not found
```

```r
plot(density(rrefs[rrefs$MatchesSum > 300, "RedCardsSumProp"]))
```

```
## Error: object 'rrefs' not found
```

```r
plot(density(rrefs[rrefs$MatchesSum > 400, "RedCardsSumProp"]))
```

```
## Error: object 'rrefs' not found
```

```r
table(rrefs$RedCardsSum)
```

```
## Error: object 'rrefs' not found
```

```r
table(rrefs[rrefs$MatchesSum > 200, "RedCardsSum"])
```

```
## Error: object 'rrefs' not found
```

```r
table(rrefs[rrefs$MatchesSum > 100, "RedCardsSum"])
```

```
## Error: object 'rrefs' not found
```

```r
round(prop.table(table(rrefs[rrefs$MatchesSum > 100, "RedCardsSum"])), 3)
```

```
## Error: object 'rrefs' not found
```

```r

# * I guess the density reflects the fact that a lot of referees never give
# out a red card in their career * Including refs just with More games seems
# to move towards a more normal distribution

```


### Referee Country database

```r
# understanding of
table(rcountries$nIAT)
```

```
## Error: object 'rcountries' not found
```

```r
plot(density(na.omit(rcountries$seIAT)))
```

```
## Error: object 'rcountries' not found
```

```r
aplot(sqrt(rcountries$nIAT), rcountries$seIAT)
```

```
## Error: could not find function "aplot"
```

```r

plot(density(na.omit(rcountries$meanIAT_RefCountry)))
```

```
## Error: object 'rcountries' not found
```

```r
plot(density(na.omit(rcountries$meanExp_RefCountry)))
```

```
## Error: object 'rcountries' not found
```

```r
plot(meanIAT_RefCountry ~ meanExp_RefCountry, rcountries)
```

```
## Error: object 'rcountries' not found
```

```r


plot(density(log(rcountries$MatchesSum, 10)))
```

```
## Error: object 'rcountries' not found
```

```r
sum(rcountries$MatchesSum > 1000)
```

```
## Error: object 'rcountries' not found
```

```r
plot(density(rcountries[rcountries$MatchesSum > 1000, "RedCardsSumProp"]))
```

```
## Error: object 'rcountries' not found
```



# Very basic exploration of models

```r
fit <- glm(cbind(RedCards, Matches - RedCards) ~ SkinCol, family = binomial, 
    data = rdyads)
```

```
## Error: object 'rdyads' not found
```

```r
head(rdyads)
```

```
## Error: object 'rdyads' not found
```

```r
summary(fit)
```

```
## Error: object 'fit' not found
```

```r

logit2pi <- function(x) exp(x)/(1 + exp(x))
pred <- coef(fit)[1] + coef(fit)[2] * 1:5
```

```
## Error: object 'fit' not found
```

```r
logit2pi(pred)
```

```
## Error: object 'pred' not found
```

```r

head(rdyads)
```

```
## Error: object 'rdyads' not found
```

```r
sum(rdyads$RedCards)/sum(rdyads$Matches)
```

```
## Error: object 'rdyads' not found
```

```r

fit
```

```
## Error: object 'fit' not found
```

```r

```





