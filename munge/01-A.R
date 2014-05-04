# Example preprocessing script.
v <- list()
v$player_variables <- c("PlayerID", "Player", "Club", "League_Country", "LeagueID", 
                        "SkinCol", "PhotoID", "PlayerHeightCM", "PlayerBirthd", "BirthD", 
                        "BirthM", "BirthY", "PositionID", "Position", "Position_Detailed")
v$ref_variables <- c("RefereeID", "RefCountryID", "RCperRef", "TotMatPerRef", "meanIAT_RefCountry", 
"nIAT", "seIAT", "meanExp_RefCountry", "nExp", "seExp")


# create player and referee data frames
rplayers <-  rdyads[!duplicated(rdyads$PlayerID), v$player_variables]
rrefs <- rdyads[!duplicated(rdyads$RefereeID), v$ref_variables]

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
}



# merge in totals to ref


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
}

