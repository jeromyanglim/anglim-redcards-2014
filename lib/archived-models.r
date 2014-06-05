m_beta <- function() {
    "
model {
    # Model
    for (index in 1:N) {
    y[index] ~ dbin(pi[index], n[index])
    pi_prime[index] <- exp(beta) / (1 + exp(beta))
    pi[index] <- exp(pi_prime[index]) / (1 + exp(pi_prime[index]))
    }
    
    beta ~ dunif(-5, 5)
}
    "    
    }

m_randomplayer <- function() {
    "
    model {
    # Model
    for (index in 1:N) {
    y[index] ~ dbin(pi[index], n[index])
    pi_prime[index] <- beta + beta1[d_i[index]]
    pi[index] <- exp(pi_prime[index]) / (1 + exp(pi_prime[index]))
    }
    
    for (index in 1:length(p_i)) {
    beta1[index] ~ dnorm(0, tau_beta1)
    }
    
    beta ~ dunif(-5, 5)
    sigma_beta1 ~ dunif(0, 5)
    
    tau_beta1 <- pow(sigma_beta1, -2)
    }
    "    
}

m_randomplayer_randomref <- function() {
    "
    model {
    # Model
    for (index in 1:N) {
    y[index] ~ dbin(pi[index], n[index])
    pi_prime[index] <- beta + beta1[d_i[index]] + beta2[d_j[index]]
    pi[index] <- exp(pi_prime[index]) / (1 + exp(pi_prime[index]))
    }
    
    for (index in 1:length(p_i)) {
    beta1[index] ~ dnorm(0, tau_beta1)
    }
    
    for (index in 1:length(r_j)) {
    beta2[index] ~ dnorm(0, tau_beta2)
    }
    
    beta ~ dunif(-5, 5)
    sigma_beta1 ~ dunif(0, 5)
    sigma_beta2 ~ dunif(0, 5)
    
    tau_beta1 <- pow(sigma_beta1, -2)
    tau_beta2 <- pow(sigma_beta2, -2)
    }
    "    
}

m_player_pred <- function() {
    "
model {
    # Model
    for (index in 1:N) {
        y[index] ~ dbin(pi[index], n[index])
        pi_prime[index] <- beta + beta1[d_i[index]] + beta2[d_j[index]]
        pi[index] <- exp(pi_prime[index]) / (1 + exp(pi_prime[index]))
    }

    for (index in 1:length(p_i)) {
        beta1[index] ~ dnorm(mu_beta1[index], tau_beta1)
        mu_beta1[index] <- inprod(x[index, ], gamma_beta1[])
    }

    for (index in 1:length(r_j)) {
        beta2[index] ~ dnorm(0, tau_beta2)
    }

    beta ~ dunif(-5, 5)
    sigma_beta1 ~ dunif(0, 5)
    sigma_beta2 ~ dunif(0, 5)

    for (index in 1:P1) {
        gamma_beta1[index] ~ dnorm(0, 0.1)
    }

    tau_beta1 <- pow(sigma_beta1, -2)
    tau_beta2 <- pow(sigma_beta2, -2)
}
"    
}


m_player_ref_coef <- function() {
    "
model {
    # Model
    for (index in 1:N) {
    y[index] ~ dbin(pi[index], n[index])
    pi_prime[index] <- beta + beta1[d_i[index]] + beta2[d_j[index]]
    pi[index] <- exp(pi_prime[index]) / (1 + exp(pi_prime[index]))
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
    
    for (index in 1:P1) {
    gamma_beta1[index] ~ dnorm(0, 0.1)
    }
    
    for (index in 1:P2) {
    gamma_beta2[index] ~ dnorm(0, 0.1)
    }
    
    tau_beta1 <- pow(sigma_beta1, -2)
    tau_beta2 <- pow(sigma_beta2, -2)
}
    "    
    }


m_beta3_added <- function() {
    "
model {
    # Model
    for (index in 1:N) {
    y[index] ~ dbin(pi[index], n[index])
    pi_prime[index] <- beta + beta1[d_i[index]] + beta2[d_j[index]] + beta3[index]
    pi[index] <- exp(pi_prime[index]) / (1 + exp(pi_prime[index]))
    
    beta3[index] <- inprod(w[index, ], gamma_beta3[])
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
}
    "    
}


