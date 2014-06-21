# Example preprocessing script.
v <- list()
v$player_variables <- c("PlayerID", "Player", "Club", "League_Country", "LeagueID", 
                        "SkinCol", "PhotoID", "PlayerHeightCM", "PlayerBirthd", "BirthD", 
                        "BirthM", "BirthY", "PositionID", "Position", "Position_Detailed")
v$ref_variables <- c("RefereeID", "RefCountryID", "RCperRef", "TotMatPerRef", "meanIAT_RefCountry", 
"nIAT", "seIAT", "meanExp_RefCountry", "nExp", "seExp")
v$iat_measures <- c("meanIAT_RefCountry",  "nIAT", "seIAT", "meanExp_RefCountry", "nExp", "seExp")



# Fix variable codings
v$make_numeric <- c("BirthD", "meanIAT_RefCountry",  "nIAT", "seIAT", "meanExp_RefCountry", "nExp", "seExp")
for (variable in v$make_numeric) {
    rdyads[,variable] <- as.numeric(rdyads[,variable])
}

rdyads[rdyads$Position == "", 'Position'] <- NA

# Player and dyad variables
rdyads$SkinCol_4or5 <- as.numeric(rdyads$SkinCol == 4 | rdyads$SkinCol == 5) # binary coding
rdyads$SkinCol_0to1 <- (rdyads$SkinCol -1) / 4 # continuous and rescaled


# create player, referee, and referee country data frames
rplayers <-  rdyads[!duplicated(rdyads$PlayerID), v$player_variables]
rrefs <- rdyads[!duplicated(rdyads$RefereeID), v$ref_variables]
rcountries <- rrefs[!duplicated(rrefs$RefCountryID), c('RefCountryID', v$iat_measures)]


# merge in totals to player
merge_sum <- function(variable='RedCards', id='PlayerID', data=rplayers, dyads=rdyads) {
    x <- aggregate(dyads[, variable], list(dyads[, id]), sum)
    newvar <- paste0(variable, 'Sum')
    names(x) <- c(id, newvar)
    merge(data,x, all.x=TRUE, sort=FALSE)
}

for (variable in c("Matches", "Goals", "YellowCards", "YellowRed", "RedCards")) {
    rplayers <- merge_sum(variable, 'PlayerID', rplayers, rdyads)
    rrefs <- merge_sum(variable, 'RefereeID', rrefs, rdyads)
    rcountries <- merge_sum(variable, 'RefCountryID', rcountries, rdyads)
}


# create proportion variables
create_prop <- function(data=rdyads, variable='Goals', divisor='Matches') {
    newvar <- paste0(variable, 'Prop')
    data[,newvar] <- data[,variable] / data[,divisor] 
    data
}

for (variable in c('Goals', 'YellowCards', 'YellowRed', 'RedCards')) {
    rdyads <- create_prop(rdyads, variable, 'Matches')
    variable_sum <- paste0(variable, 'Sum')
    rplayers <- create_prop(rplayers, variable_sum, 'MatchesSum')
    rrefs <- create_prop(rrefs, variable_sum, 'MatchesSum')
    rcountries <- create_prop(rcountries, variable_sum, 'MatchesSum')
}

# create modified predictors
rplayers$MatchesSum_log <- log(rplayers$MatchesSum)
rrefs$MatchesSum_log <- log(rrefs$MatchesSum)












# creeate interaction variables
make_interaction <- function(x1, x2, data=rdyads, separator="BY") {
    interaction_variable_name <- paste0(x1, separator, x2)
    data[ , interaction_variable_name] <- data[,x1] * data[,x2]
    data
}

rdyads <- make_interaction('SkinCol', 'meanIAT_RefCountry')



