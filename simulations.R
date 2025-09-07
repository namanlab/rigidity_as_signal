library(tidyverse)
library(mvtnorm)

# --- PARAMETERS ---
aDW = 0.4  # Pessimism Multiplier: Weak Type
aDS = 0.6  # Pessimism Multiplier: Strong Type
PD = 8     # Discounted Price
V0W = 10   # Mean for Buyer's Current Weak Type Valuations
V0S = 11   # Mean for Buyer's Current Strong Type Valuations
PH = 12    # Holding Price
aHW = 1.4  # Optimism Multiplier: Weak Type
aHS = 1.5  # Optimism Multiplier: Strong Type

p = 0.5       # Proportion of Strong Types
beta = 0.5    # Discount Factor
cS = 2        # Cost of Holding for Strong Type
cW = 3        # Cost of Holding for Weak Type
sigma0 = 1    # SD for Buyer's Current Valuations
sigma1 = 1    # SD for Buyer's Future Valuations

N = 10000     # Number of Simulations
set.seed(42)


phi_fn <- function(mu, sigma){
  mu*pnorm(mu/sigma) + sigma*dnorm(mu/sigma)
}

# --- MAIN SIMULATION FUNCTION ---
run_sim <- function(PH, PD, beta, p,
                    V0S, V0W,
                    aHS, aHW, aDS, aDW,
                    cS, cW,
                    sigma0, sigma1,
                    N, echo = F) {
  # 1. Period 1 valuation means
  V1HS = aHS * V0S
  V1HW = aHW * V0W
  V1DS = aDS * V0S
  V1DW = aDW * V0W
  
  if (echo){cat("==== Checking Assumptions and Derived Parameters ====\n")}
  stopifnot(
    V1DW < V1DS, V1DW < PD, V1DW < V0W, V1DW < V0S, V1DW < PH, V1DW < V1HW, V1DW < V1HS,
    V1DS < PD, V1DS < V0W, V1DS < V0S, V1DS < PH, V1DS < V1HW, V1DS < V1HS,
    PD < V0W, PD < V0S, PD < PH, PD < V1HW, PD < V1HS,
    V0W < V0S, V0W < PH, V0W < V1HW, V0W < V1HS,
    V0S < PH, V0S < V1HW, V0S < V1HS,
    PH < V1HW, PH < V1HS,
    V1HW < V1HS,
    cS < cW,
    cS < beta * PH
  )
  if (echo){cat("Technical Assumptions Satisfied ✔\n")}
  
  sHS = sqrt(sigma0^2*aHS^2 + sigma1^2)
  sHW = sqrt(sigma0^2*aHW^2 + sigma1^2)
  sDS = sqrt(sigma0^2*aDS^2 + sigma1^2)
  sDW = sqrt(sigma0^2*aDW^2 + sigma1^2)
  
  
  # 2. Normalized thresholds
  k_SH = (PH - V0S) / sigma0
  r_SH = (V1HS - PH) / sHS
  k_WH = (PH - V0W) / sigma0
  r_WH = (V1HW - PH) / sHW
  k_SD = (V0S - PD) / sigma0
  r_SD = (PD - V1DS) / sDS
  k_WD = (V0W - PD) / sigma0
  r_WD = (PD - V1DW) / sDW
  
  if (echo){print(data.frame(k_SH, r_SH, k_WH, r_WH, k_SD, r_SD, k_WD, r_WD))}
  
  # 3. Signal Coherence Checks
  SWD_bound = sDS * beta / (2 * sigma0)
  SWH_bound = sigma0 / (2 * sHW * beta)
  
  if (k_WD * r_SD <= SWD_bound) {
    if (echo){cat("!! Signal Coherence Failed (D Path):\n")
      cat("   k_WD * r_SD =", round(k_WD * r_SD, 4), "<= Threshold =", round(SWD_bound, 4), "\n")}
    stop()
  }
  if (k_SH * r_WH <= SWH_bound) {
    if (echo){cat("!! Signal Coherence Failed (H Path):\n")
      cat("   k_SH * r_WH =", round(k_SH * r_WH, 4), "<= Threshold =", round(SWH_bound, 4), "\n")}
    stop()
  }
  
  if (echo){cat("Signal Coherence Satisfied ✔\n")}
  
  # 4. Equilibrium Determination
  pool_H_thresh <- beta * PH * pnorm((p*r_SH*sHS + (1 - p)*r_WH*sHW)/(sqrt(p^2*sHS^2 + (1-p)^2*sHW^2))) - PD * pnorm(k_WD)
  pool_D_thresh <- beta * PH * pnorm(r_WH) - PD * pnorm((p*k_SD + (1 - p)*k_WD)/(sqrt(p^2 + (1-p)^2)))
  sep_HD_thresh <- beta * PH * pnorm(r_SH) - PD * pnorm(k_WD)
  
  res = list()
  res[["Pool_H"]] = NA
  res[["Pool_D"]] = NA
  res[["Sep_HD"]] = NA
  if (cW < pool_H_thresh){
    res[["Pool_H"]] = run_sim_eq(PH, PD, beta, p, V0S, V0W,
                                 aHS, aHW, aDS, aDW, cS, cW,
                                 sigma0, sigma1, N, echo,
                                 "Pooling (H, H)", pool_H_thresh, pool_D_thresh, sep_HD_thresh,
                                 k_SH, k_WH, k_SD, k_WD, r_SH, r_WH, r_SD, r_WD,
                                 sHS, sHW, sDS, sDW)
  }
  if (cS > pool_D_thresh){
    res[["Pool_D"]] = run_sim_eq(PH, PD, beta, p, V0S, V0W,
                                 aHS, aHW, aDS, aDW, cS, cW,
                                 sigma0, sigma1, N, echo,
                                 "Pooling (D, D)", pool_H_thresh, pool_D_thresh, sep_HD_thresh,
                                 k_SH, k_WH, k_SD, k_WD, r_SH, r_WH, r_SD, r_WD,
                                 sHS, sHW, sDS, sDW)
    
  }
  if (cS < sep_HD_thresh & cW > sep_HD_thresh){
    res[["Sep_HD"]] = run_sim_eq(PH, PD, beta, p, V0S, V0W,
                                 aHS, aHW, aDS, aDW, cS, cW,
                                 sigma0, sigma1, N, echo,
                                 "Separating (H, D)", pool_H_thresh, pool_D_thresh, sep_HD_thresh,
                                 k_SH, k_WH, k_SD, k_WD, r_SH, r_WH, r_SD, r_WD,
                                 sHS, sHW, sDS, sDW)
    
  }
  return(res)
}

run_sim_eq <- function(PH, PD, beta, p,
                       V0S, V0W,
                       aHS, aHW, aDS, aDW,
                       cS, cW,
                       sigma0, sigma1,
                       N, echo,
                       eqm, pool_H_thresh, pool_D_thresh, sep_HD_thresh,
                       k_SH, k_WH, k_SD, k_WD,
                       r_SH, r_WH, r_SD, r_WD,
                       sHS, sHW, sDS, sDW) {
  
  if (echo){cat("\n==== Equilibrium Determination ====\n")
    cat("Equilibrium:", eqm, "\n")
    cat("cS =", round(cS, 2), "\n")
    cat("cW =", round(cW, 2), "\n")
    cat("pool_H_thresh =", round(pool_H_thresh, 2), "\n")
    cat("pool_D_thresh =", round(pool_D_thresh, 2), "\n")
    cat("sep_HD_thresh =", round(sep_HD_thresh, 2), "\n")
  }
  
  # 5.1. Theoretical Vacancy Rate
  vacancy_theory <- switch(eqm,
                           "Pooling (H, H)" = pnorm((p * k_SH + (1 - p) * k_WH)/(sqrt(p^2 + (1-p)^2))),
                           "Pooling (D, D)" = pnorm((-p * k_SD - (1 - p) * k_WD)/(sqrt(p^2 + (1-p)^2))),
                           "Separating (H, D)" = p * pnorm(k_SH) + (1 - p) * pnorm(-k_WD)
  )
  # 5.2. Strong Seller Payoff
  strong_seller_payoff_theory <- 0
  if (eqm == "Pooling (H, H)"){
    alpha_S = (p * k_SH + (1 - p) * k_WH)/(sqrt(p^2 + (1-p)^2))
    beta_S =  (p*r_SH*sHS + (1 - p)*r_WH*sHW)/(sqrt(p^2*sHS^2 + (1-p)^2*sHW^2))
    r_S = sigma0*(p^2*aHS + (1 - p)^2*aHW)/(sqrt(p^2 + (1-p)^2)*sqrt(p^2*sHS^2 + (1-p)^2*sHW^2))
    prob_S = (as.numeric(pmvnorm(lower = c(-beta_S, -Inf), upper = c(Inf, alpha_S), sigma = matrix(c(1, r_S, r_S, 1), nrow=2))))
    temp_S = pnorm(alpha_S)
    val1_S = PH*(1 - temp_S)
    val2_S = beta*PH*prob_S 
    val3_S = cS*temp_S 
    strong_seller_payoff_theory <- val1_S + val2_S - val3_S
  } else if (eqm == "Pooling (D, D)"){
    alpha_S = (p * k_SD + (1 - p) * k_WD)/(sqrt(p^2 + (1-p)^2))
    beta_S =  (p*r_SD*sDS + (1 - p)*r_WD*sDW)/(sqrt(p^2*sDS^2 + (1-p)^2*sDW^2))
    r_S = sigma0*(p^2*aDS + (1 - p)^2*aDW)/(sqrt(p^2 + (1-p)^2)*sqrt(p^2*sDS^2 + (1-p)^2*sDW^2))
    prob_S = (as.numeric(pmvnorm(lower = c(beta_S, -Inf), upper = c(Inf, -alpha_S), sigma = matrix(c(1, r_S, r_S, 1), nrow=2))))
    temp_S = pnorm(alpha_S)
    val1_S = PD*temp_S
    val2_S = beta*PD*prob_S 
    val3_S = cS*(1 - temp_S) 
    strong_seller_payoff_theory <- val1_S + val2_S - val3_S
  } else {
    alpha_S = k_SH
    beta_S =  r_SH
    r_S = sigma0*aHS/sHS
    prob_S = (as.numeric(pmvnorm(lower = c(-beta_S, -Inf), upper = c(Inf, alpha_S), sigma = matrix(c(1, r_S, r_S, 1), nrow=2))))
    temp_S = pnorm(alpha_S)
    val1_S = PH*(1 - temp_S)
    val2_S = beta*PH*prob_S 
    val3_S = cS*temp_S 
    strong_seller_payoff_theory <- val1_S + val2_S - val3_S
  }
  # 5.3. Weak Seller Payoff
  weak_seller_payoff_theory <- 0
  if (eqm == "Pooling (H, H)"){
    alpha_S = (p * k_SH + (1 - p) * k_WH)/(sqrt(p^2 + (1-p)^2))
    beta_S =  (p*r_SH*sHS + (1 - p)*r_WH*sHW)/(sqrt(p^2*sHS^2 + (1-p)^2*sHW^2))
    r_S = sigma0*(p^2*aHS + (1 - p)^2*aHW)/(sqrt(p^2 + (1-p)^2)*sqrt(p^2*sHS^2 + (1-p)^2*sHW^2))
    prob_S = (as.numeric(pmvnorm(lower = c(-beta_S, -Inf), upper = c(Inf, alpha_S), sigma = matrix(c(1, r_S, r_S, 1), nrow=2))))
    temp_S = pnorm(alpha_S)
    val1_S = PH*(1 - temp_S)
    val2_S = beta*PH*prob_S 
    val3_S = cW*temp_S 
    weak_seller_payoff_theory <- val1_S + val2_S - val3_S
  } else if (eqm == "Pooling (D, D)"){
    alpha_S = (p * k_SD + (1 - p) * k_WD)/(sqrt(p^2 + (1-p)^2))
    beta_S =  (p*r_SD*sDS + (1 - p)*r_WD*sDW)/(sqrt(p^2*sDS^2 + (1-p)^2*sDW^2))
    r_S = sigma0*(p^2*aDS + (1 - p)^2*aDW)/(sqrt(p^2 + (1-p)^2)*sqrt(p^2*sDS^2 + (1-p)^2*sDW^2))
    prob_S = (as.numeric(pmvnorm(lower = c(beta_S, -Inf), upper = c(Inf, -alpha_S), sigma = matrix(c(1, r_S, r_S, 1), nrow=2))))
    temp_S = pnorm(alpha_S)
    val1_S = PD*temp_S
    val2_S = beta*PD*prob_S 
    val3_S = cW*(1 - temp_S) 
    weak_seller_payoff_theory <- val1_S + val2_S - val3_S
  } else {
    alpha_S = k_WD
    beta_S =  r_WD
    r_S = sigma0*aDW/sDW
    prob_S = (as.numeric(pmvnorm(lower = c(beta_S, -Inf), upper = c(Inf, -alpha_S), sigma = matrix(c(1, r_S, r_S, 1), nrow=2))))
    temp_S = pnorm(alpha_S)
    val1_S = PD*temp_S
    val2_S = beta*PD*prob_S 
    val3_S = cW*(1 - temp_S) 
    weak_seller_payoff_theory <- val1_S + val2_S - val3_S
  }
  
  # 6. Simulations
  seller_type <- sample(c("Strong", "Weak"), N, replace = TRUE, prob = c(p, 1 - p))
  # Strategy
  seller_action <- case_when(
    eqm == "Pooling (H, H)" ~ "H",
    eqm == "Pooling (D, D)" ~ "D",
    eqm == "Separating (H, D)" & seller_type == "Strong" ~ "H",
    eqm == "Separating (H, D)" & seller_type == "Weak" ~ "D"
  )
  # Valuations
  v0_actual_strong = rnorm(N, V0S, sigma0)
  v0_actual_weak = rnorm(N, V0W, sigma0)
  v0_actual <- ifelse(seller_type == "Strong", v0_actual_strong, v0_actual_weak)
  # v0_belief
  v0_belief <- if (eqm %in% c("Pooling (H, H)", "Pooling (D, D)")) {
    p * v0_actual_strong + (1 - p) * v0_actual_weak
  } else {
    v0_actual
  }
  # v1_actual
  v1_actual_strong_H <- rnorm(N, aHS * v0_actual_strong, sigma1)
  v1_actual_weak_H   <- rnorm(N, aHW * v0_actual_weak, sigma1)
  v1_actual_strong_D <- rnorm(N, aDS * v0_actual_strong, sigma1)
  v1_actual_weak_D   <- rnorm(N, aDW * v0_actual_weak, sigma1)
  # Use type to combine
  v1_actual_H <- ifelse(seller_type == "Strong", v1_actual_strong_H, v1_actual_weak_H)
  v1_actual_D <- ifelse(seller_type == "Strong", v1_actual_strong_D, v1_actual_weak_D)
  v1_actual <- ifelse(seller_action == "H", v1_actual_H, v1_actual_D)
  # Use v1_actual_H and v1_actual_D for beliefs
  v1_belief <- case_when(
    eqm == "Pooling (H, H)" ~ p * v1_actual_strong_H + (1 - p) * v1_actual_weak_H,
    eqm == "Pooling (D, D)" ~ p * v1_actual_strong_D + (1 - p) * v1_actual_weak_D,
    eqm == "Separating (H, D)" & seller_action == "H" ~ v1_actual_strong_H,
    eqm == "Separating (H, D)" & seller_action == "D" ~ v1_actual_weak_D
  )
  # Decision logic
  price <- ifelse(seller_action == "H", PH, PD)
  surplus_now <- v0_belief - price
  surplus_later <- beta * (v1_belief - price)
  trade_now <- ifelse(surplus_now >= 0, 1, 0)
  trade_later <- ifelse(surplus_later >= 0, 1, 0)
  typ_cost <- ifelse(seller_type == "Strong", cS, cW)
  seller_payoff <- ifelse(trade_now == 1, price,
                          ifelse(trade_later == 1, beta * price - typ_cost,
                                 ifelse(seller_action == "H", -typ_cost, 0)))
  buyer_utility <- ifelse(trade_now == 1, v0_actual - price,
                          ifelse(trade_later == 1, beta * (v1_actual - price), 0))
  # Aggregated results
  sim_vacancy_rate <- mean(trade_now == 0)
  avg_seller_payoff <- tapply(seller_payoff, seller_type, mean)
  strong_seller_payoff_simulated <- avg_seller_payoff["Strong"]
  weak_seller_payoff_simulated   <- avg_seller_payoff["Weak"]
  buyer_utility_simulated <- mean(buyer_utility)
  
  if (echo){
    cat("\n==== Theoretical Outcomes ====\n")
    cat("Theoretical Vacancy Rate:", round(100 * vacancy_theory, 2), "%\n")
    cat("Theoretical Avg Seller Payoff (Strong):", round(strong_seller_payoff_theory, 2), "\n")
    cat("Theoretical Avg Seller Payoff (Weak):", round(weak_seller_payoff_theory, 2), "\n")
  
    cat("\n==== Simulated Outcomes ====\n")
    cat("Simulated Vacancy Rate:", round(100 * sim_vacancy_rate, 2), "%\n")
    cat("Simulated Avg Seller Payoff (Strong):", round(strong_seller_payoff_simulated, 2), "\n")
    cat("Simulated Avg Seller Payoff (Weak):", round(weak_seller_payoff_simulated, 2), "\n")
    cat("Simulated Avg Buyer Surplus:", round(buyer_utility_simulated, 2), "\n")
  }
  
  return(list(
    equilibrium = eqm,
    vacancy_theory = vacancy_theory,
    strong_seller_payoff_theory = strong_seller_payoff_theory,
    weak_seller_payoff_theory = weak_seller_payoff_theory,
    vacancy_simulated = sim_vacancy_rate,
    strong_seller_payoff_simulated = strong_seller_payoff_simulated,
    weak_seller_payoff_simulated = weak_seller_payoff_simulated,
    buyer_utility_simulated = buyer_utility_simulated,
    thresholds = list(pool_H_thresh = pool_H_thresh, 
                      pool_D_thresh = pool_D_thresh,
                      sep_HD_thresh = sep_HD_thresh),
    normalized = list(k_SH = k_SH, r_SH = r_SH, k_WH = k_WH, r_WH = r_WH,
                      k_SD = k_SD, r_SD = r_SD, k_WD = k_WD, r_WD = r_WD)
  ))
}

# Example call
res = run_sim(PH, PD, beta, p, V0S, V0W, aHS, aHW, aDS, aDW, cS, cW, sigma0, sigma1, N, echo = T)



################################################################################
################################################################################
################################# --- Runs --- #################################
################################################################################
################################################################################

# --- PARAMETERS ---
V0W = 10   # Mean for Buyer's Current Weak Type Valuations
V0S = 11   # Mean for Buyer's Current Strong Type Valuations
aHW = 1.4  # Optimism Multiplier: Weak Type
aHS = 1.6  # Optimism Multiplier: Strong Type
cS = 2        # Cost of Holding for Strong Type
cW = 3        # Cost of Holding for Weak Type
sigma0 = 1    # SD for Buyer's Current Valuations
sigma1 = 1    # SD for Buyer's Future Valuations

N = 10000     # Number of Simulations
set.seed(42)

# --- Define Grid of Parameters to Vary ---
aHS_vals <- seq(1.2, 1.8, by = 0.05)
aHW_vals <- seq(1.2, 1.8, by = 0.05)
aDS_vals <- seq(0.4, 0.8, by = 0.05)
aDW_vals <- seq(0.2, 0.6, by = 0.05)
PH_vals  <- seq(11.5, 12.5, by = 0.25)
PD_vals  <- seq(7.5, 8.5, by = 0.25) 
beta_vals <- seq(0.8, 0.96, by = 0.02)
p_vals <- c(0.1, 0.3, 0.5, 0.7, 0.9)

# Expand grid of all parameter combinations
grid <- expand.grid(aHS = aHS_vals,
                    aHW = aHW_vals,
                    aDS = aDS_vals,
                    aDW = aDW_vals,
                    PH = PH_vals,
                    PD = PD_vals,
                    beta = beta_vals,
                    p = p_vals)

# Initialize result storage
results <- list()

# Expand grid of all parameter combinations
grid <- expand.grid(aHS = aHS_vals,
                    aHW = aHW_vals,
                    aDS = aDS_vals,
                    aDW = aDW_vals,
                    PH = PH_vals,
                    PD = PD_vals,
                    beta = beta_vals,
                    p = p_vals)

# Loop over p values separately
for (p_val in unique(grid$p)) {
  cat("Processing p =", p_val, "\n")
  # Filter grid for this p
  grid_p <- grid[grid$p == p_val, ]
  # Temporary results for this p
  results_p <- list()
  # Loop over rows of grid_p
  last_printed <- -1
  for (i in 1:nrow(grid_p)) {
    pct <- floor(i / nrow(grid_p) * 100)
    if (pct > last_printed) {
      cat("Progress for p =", p_val, ":", pct, "%\n")
      last_printed <- pct
    }
    params <- grid_p[i, ]
    tryCatch({
      sim_3 <- run_sim(PH = params$PH, PD = params$PD, beta = params$beta,
                       p = params$p,
                       V0S = V0S, V0W = V0W,
                       aHS = params$aHS, aHW = params$aHW,
                       aDS = params$aDS, aDW = params$aDW,
                       cS = cS, cW = cW,
                       sigma0 = sigma0, sigma1 = sigma1,
                       N = N)
      for (sim in sim_3){
        if (is.list(sim)) {
          results_p[[length(results_p) + 1]] <- data.frame(
            p = params$p,
            aHS = params$aHS, aHW = params$aHW,
            aDS = params$aDS, aDW = params$aDW,
            PH = params$PH, PD = params$PD, beta = params$beta,
            equilibrium = sim$equilibrium,
            vacancy_theory = sim$vacancy_theory,
            strong_seller_payoff_theory = sim$strong_seller_payoff_theory,
            weak_seller_payoff_theory = sim$weak_seller_payoff_theory,
            vacancy_simulated = sim$vacancy_simulated,
            strong_seller_payoff_simulated = sim$strong_seller_payoff_simulated,
            weak_seller_payoff_simulated = sim$weak_seller_payoff_simulated,
            buyer_utility_simulated = sim$buyer_utility_simulated
          )
        }
      }
    }, error = function(e) {
      # cat("Skipping combo due to error at row:", i, "for p =", p_val, "\n")
    })
  }
  
  # Save results for this p
  saveRDS(results_p, file = paste0("results/res_p_", p_val*10, ".RData"))
  df_p <- dplyr::bind_rows(results_p)
  write.csv(df_p, paste0("results/sim_results_p_", p_val*10, ".csv"), row.names = FALSE)
  
  cat("Finished and saved results for p =", p_val, "\n")
}
getwd()
