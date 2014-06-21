# Example preprocessing script.
cv <- list()
cv$player_variables <- c("playerShort", "player", "club", "leagueCountry", "birthday", 
                           "height", "weight", "position", "rater1", "rater2", "photoID")
cv$ref_variables <- c("refNum", "refCountry", "meanIAT",  "nIAT", "seIAT", "meanExp", "nExp", "seExp") 
cv$iat_measures <- c("meanIAT",  "nIAT", "seIAT", "meanExp", "nExp", "seExp")
cv$id_variables <- c(cplayers="playerShort", crefs="refNum", countries="refCountry")


cdyads$InteractionID <- nrow(cdyads)



# create player, referee, and referee country data frames
cplayers <-  cdyads[!duplicated(cdyads$playerShort), cv$player_variables]
crefs <- cdyads[!duplicated(cdyads$refNum), cv$ref_variables]
ccountries <- crefs[!duplicated(crefs$refCountry), c('refCountry', cv$iat_measures)]



# merge in totals to player
merge_sum <- function(variable='RedCards', id='playerShort', data=cplayers, dyads=cdyads) {
    x <- aggregate(dyads[, variable], list(dyads[, id]), sum)
    newvar <- paste0(variable, 'Sum')
    names(x) <- c(id, newvar)
    merge(data,x, all.x=TRUE, sort=FALSE)
}


for (variable in c("games", "victories", "ties", "defeats", "goals", "yellowCards", "yellowReds", "redCards")) {
    cplayers <- merge_sum(variable, 'playerShort', cplayers, cdyads)
    crefs <- merge_sum(variable, 'refNum', crefs, cdyads)
    ccountries <- merge_sum(variable, 'refCountry', ccountries, cdyads)
}

# create proportion variables
create_prop <- function(data=cdyads, variable='goals', divisor='games') {
    newvar <- paste0(variable, 'Prop')
    data[,newvar] <- data[,variable] / data[,divisor] 
    data
}

for (variable in c("victories", "ties", "defeats", "goals", "yellowCards", "yellowReds", "redCards")) {
    cdyads <- create_prop(cdyads, variable, 'games')
    variable_sum <- paste0(variable, 'Sum')
    cplayers <- create_prop(cplayers, variable_sum, 'gamesSum')
    crefs <- create_prop(crefs, variable_sum, 'gamesSum')
    ccountries <- create_prop(ccountries, variable_sum, 'gamesSum')
}

# create modified predictors
cplayers$gamesSum_log <- log(cplayers$gamesSum)
crefs$gamesSum_log <- log(crefs$gamesSum)



# Player and dyad variables
cplayers$skincol <- apply(cplayers[,c('rater1', 'rater2')], 1, mean)
cplayers$skincol_4or5 <- cplayers$skincol >= 4 # binary coding
cplayers$skincol_0to1 <- (cplayers$skincol-1) / 4 # continuous and rescaled
# process birthday
return_dates <- function(x) {
    temp <- strsplit(x, split='.', fixed=TRUE)
    temp <- data.frame(t(data.frame(temp)))
    names(temp) <- c('birthday_day', 'birthday_month', 'birthday_year')
    temp <- sapply(temp, as.numeric)
}
temp <- return_dates(cplayers$birthday)
cplayers <- cbind(cplayers, temp)
head(cplayers)


# merge in player variables
cdyads <- merge(cdyads, cplayers[,c("playerShort", "skincol", "skincol_4or5", "skincol_0to1")], all.x=TRUE)


# creeate interaction variables
make_interaction <- function(x1, x2, data=cdyads, separator="BY", center=TRUE) {
    interaction_variable_name <- paste0(x1, separator, x2)
    if (center) {
        data[ , interaction_variable_name] <- as.numeric(scale(data[,x1], center=TRUE, scale=FALSE) * 
                                                         scale(data[,x2], center=TRUE, scale=FALSE))
    } else {
        data[ , interaction_variable_name] <- data[,x1] * data[,x2]
    }
    data
}

cdyads <- make_interaction('skincol', 'meanIAT', center=TRUE)

cor(cdyads[,c('skincol', 'meanIAT', 'skincolBYmeanIAT')], use='pair')

