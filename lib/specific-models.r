get_data1 <- function(y='RedCards', 
                      n='Matches', 
                      ij='InteractionID', 
                      x_var=NULL, 
                      i='PlayerID', 
                      z_var=NULL, 
                      j='RefereeID', 
                      w_var=NULL, 
                      dyad_data=rdyads,
                      player_data=rplayers,
                      ref_data=rrefs) {
    # remove missing data
    if (!is.null(x_var)) {
        player_data <-     player_data[
            apply(player_data[,x_var, drop=FALSE], 1, function(X) sum(is.na(X)) == 0)
            , ]
    }
    
    if (!is.null(z_var)) {
        ref_data <-     ref_data[
            apply(ref_data[,z_var, drop=FALSE], 1, function(X) sum(is.na(X)) == 0)
            , ]
    }
    
    
    # resolve consistent ids
    ids <- list()
    ids$dyad_player <- dyad_data[, i]
    ids$dyad_ref <- dyad_data[,j]
    ids$player_player <- player_data[,i]
    ids$ref_ref <- ref_data[,j]

    # ensure consistent ids across dyad, player, and ref files
    dyad_data <- dyad_data[dyad_data[,i] %in% ids$player_player & dyad_data[,j] %in% ids$ref_ref, ]
    player_data <- player_data[player_data[,i] %in% ids$dyad_player, ] 
    ref_data <- ref_data[ref_data[,j] %in% ids$dyad_ref, ] 
    
    # update ids to be consecutive
    player_data$pid <- order(player_data[,i])
    ref_data$rid <-  order(ref_data[,j])
    dyad_data <- merge(dyad_data, player_data[,c(i, 'pid')], sort=FALSE)
    dyad_data <- merge(dyad_data, ref_data[,c(j, 'rid')], sort=FALSE)
    
    i <- paste0(i, '_jags')
    j <- paste0(j, '_jags')
    ij_old <- ij
    ij <- paste0(ij, '_jags')
    dyad_data[,c(i,j)] <- dyad_data[,c('pid', 'rid')]
    player_data[,i] <- player_data[,'pid']
    ref_data[,j] <- ref_data[,'rid']
    dyad_data[,ij] <- order(dyad_data[,ij_old])
    
    
    
    
    
    d <- list()
    d$y <- dyad_data[, y]
    d$n <- dyad_data[,n]
    d$ij <- dyad_data[,ij]
    d$d_i <- dyad_data[,i]
    d$d_j <- dyad_data[,j]
    d$N <- nrow(dyad_data)
    d$I <- nrow(player_data)
    d$J <- nrow(ref_data)
    #if (!is.null(x_var)) {
        d$x <- as.matrix(player_data[,x_var])
        d$p_i <- player_data[,i]
        d$P1 <- length(x_var)
    #}
    
    #if (!is.null(z_var)) {
        d$z  <- as.matrix(ref_data[,z_var])
        d$r_j <- ref_data[,j]
        d$P2 <- length(z_var)
    #}
    
    if (!is.null(w_var)) {
        d$w  <- as.matrix(dyad_data[,w_var])
        d$P3 <- length(w_var)
    }
    
    list(jags=d, data=list(dyad_data, player_data, ref_data))

}

m1 <- function() {
    "
model {
    # Model
    for (index in 1:N) {
        y[index] ~ dbin(pi[index], n[index])
        pi_prime[index] <- beta + beta1[d_i[index]] + beta2[d_j[index]] + beta3[index]
        pi[index] <- exp(pi_prime[index]) / (1 + exp(pi_prime[index]))
        beta3[index] ~ dnorm(mu_beta3[index], tau_beta3)
        mu_beta3[index] <- inprod(w[index, ], gamma_beta3[])
    }
    
    for (index in 1:length(p_i)) {
        beta1[index] ~ dnorm(mu_beta1[index], tau_beta1)
        mu_beta1[index] <- inprod(x[index, ], gamma_beta1[])
    }
    
    for (index in 1:length(r_j)) {
        beta2[index] ~ dnorm(mu_beta2[index], tau_beta2)
        mu_beta2[index] <- inprod(z[index, ], gamma_beta2[])
    }
    
    beta ~ dunif(-5, 5)
    sigma_beta1 ~ dunif(0, 5)
    sigma_beta2 ~ dunif(0, 5)
    sigma_beta3 ~ dunif(0, 5)
    
    for (index in 1:P1) {
        gamma_beta1[index] ~ dnorm(0, 0.1)
    }
    
    for (index in 1:P2) {
        gamma_beta2[index] ~ dnorm(0, 0.1)
    }

    for (index in 1:P3) {
        gamma_beta3[index] ~ dnorm(0, 0.1)
    }

    tau_beta1 <- pow(sigma_beta1, -2)
    tau_beta2 <- pow(sigma_beta2, -2)
    tau_beta3 <- pow(sigma_beta3, -2)
}
    "    
}


