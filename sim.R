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
aHS = 1.6  # Optimism Multiplier: Strong Type

p = 0.5       # Proportion of Strong Types
beta = 0.9    # Discount Factor
cS = 2        # Cost of Holding for Strong Type
cW = 3        # Cost of Holding for Weak Type
sigma0 = 1    # SD for Buyer's Current Valuations
sigma1 = 1    # SD for Buyer's Future Valuations

N = 10000     # Number of Simulations
set.seed(42)

# --- MAIN SIMULATION FUNCTION ---
run_sim <- function(PH, PD, beta,
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
  
  # 2. Normalized thresholds
  k_SH = (PH - V0S) / sigma0
  r_SH = (V1HS - PH) / sigma1
  k_WH = (PH - V0W) / sigma0
  r_WH = (V1HW - PH) / sigma1
  k_SD = (V0S - PD) / sigma0
  r_SD = (PD - V1DS) / sigma1
  k_WD = (V0W - PD) / sigma0
  r_WD = (PD - V1DW) / sigma1
  
  if (echo){print(data.frame(k_SH, r_SH, k_WH, r_WH, k_SD, r_SD, k_WD, r_WD))}
  
  # 3. Signal Coherence Checks
  SWD_bound = sigma1 * beta / (2 * sigma0)
  SWH_bound = sigma0 / (2 * sigma1 * beta)
  
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
  cS_thresh = beta * PH * pnorm(r_SH) - PD * pnorm(k_SD)
  cW_thresh = beta * PH * pnorm(r_WH) - PD * pnorm(k_WD)
  
  eqm <- case_when(
    cS < cS_thresh & cW < cW_thresh ~ "Pooling (H, H)",
    cS > cS_thresh & cW > cW_thresh ~ "Pooling (D, D)",
    cS < cS_thresh & cW > cW_thresh ~ "Separating (H, D)",
    TRUE ~ "Separating (D, H)"
  )
  
  if (echo){cat("\n==== Equilibrium Determination ====\n")
    cat("Equilibrium:", eqm, "\n")
    cat("cS =", round(cS, 2), "vs threshold =", round(cS_thresh, 2), "\n")
    cat("cW =", round(cW, 2), "vs threshold =", round(cW_thresh, 2), "\n")}
  
  # 5. Theoretical Vacancy Rate
  vacancy_theory <- switch(eqm,
                           "Pooling (H, H)" = pnorm(p * k_SH + (1 - p) * k_WH),
                           "Pooling (D, D)" = pnorm(-p * k_SD - (1 - p) * k_WD),
                           "Separating (H, D)" = p * pnorm(k_SH) + (1 - p) * pnorm(-k_WD),
                           "Separating (D, H)" = p * pnorm(-k_SD) + (1 - p) * pnorm(k_WH)
  )
  if (echo){cat("\nTheoretical Vacancy Rate:", round(100 * vacancy_theory, 2), "%\n")}
  
  # 6. Simulations
  seller_type <- sample(c("Strong", "Weak"), N, replace = TRUE, prob = c(p, 1 - p))
  
  # Strategy
  seller_action <- case_when(
    eqm == "Pooling (H, H)" ~ "H",
    eqm == "Pooling (D, D)" ~ "D",
    eqm == "Separating (H, D)" & seller_type == "Strong" ~ "H",
    eqm == "Separating (H, D)" & seller_type == "Weak" ~ "D",
    eqm == "Separating (D, H)" & seller_type == "Strong" ~ "D",
    eqm == "Separating (D, H)" & seller_type == "Weak" ~ "H"
  )
  
  # Valuations
  v0_actual <- ifelse(seller_type == "Strong",
                      rnorm(N, V0S, sigma0),
                      rnorm(N, V0W, sigma0))
  
  # v0_belief
  v0_belief <- if (eqm %in% c("Pooling (H, H)", "Pooling (D, D)")) {
    p * rnorm(N, V0S, sigma0) + (1 - p) * rnorm(N, V0W, sigma0)
  } else {
    v0_actual
  }
  
  # v1_actual
  v1_actual_strong_H <- rnorm(N, aHS * v0_actual, sigma1)
  v1_actual_weak_H   <- rnorm(N, aHW * v0_actual, sigma1)
  v1_actual_strong_D <- rnorm(N, aDS * v0_actual, sigma1)
  v1_actual_weak_D   <- rnorm(N, aDW * v0_actual, sigma1)
  
  # Use type to combine
  v1_actual_H <- ifelse(seller_type == "Strong", v1_actual_strong_H, v1_actual_weak_H)
  v1_actual_D <- ifelse(seller_type == "Strong", v1_actual_strong_D, v1_actual_weak_D)
  v1_actual <- ifelse(seller_action == "H", v1_actual_H, v1_actual_D)
  
  # --- Use v1_actual_H and v1_actual_D for beliefs ---
  v1_belief <- case_when(
    eqm == "Pooling (H, H)" ~ p * v1_actual_strong_H + (1 - p) * v1_actual_weak_H,
    eqm == "Pooling (D, D)" ~ p * v1_actual_strong_D + (1 - p) * v1_actual_weak_D,
    eqm == "Separating (H, D)" & seller_action == "H" ~ v1_actual_H,
    eqm == "Separating (H, D)" & seller_action == "D" ~ v1_actual_D,
    eqm == "Separating (D, H)" & seller_action == "H" ~ v1_actual_H,
    eqm == "Separating (D, H)" & seller_action == "D" ~ v1_actual_D
  )
  
  # Decision logic
  price <- ifelse(seller_action == "H", PH, PD)
  surplus_now <- v0_belief - price
  surplus_later <- beta * (v1_belief - price)
  trade <- ifelse(surplus_now >= 0, 1, 0)
  typ_cost <- ifelse(seller_type == "Strong", cS, cW)
  seller_payoff <- ifelse(trade == 1, price,
                          ifelse(surplus_later >= 0, beta * price - typ_cost,
                                 ifelse(seller_action == "H", -typ_cost, 0)))
  
  buyer_utility <- ifelse(trade == 1, v0_actual - price,
                          ifelse(surplus_later >= 0, beta * (v1_actual - price), 0))
  
  # Aggregated results
  sim_vacancy_rate <- mean(trade == 0)
  avg_seller_payoff <- tapply(seller_payoff, seller_type, mean)
  avg_buyer_util <- mean(buyer_utility)
  
  if (echo){
    cat("\n==== Simulated Outcomes ====\n")
    cat("Simulated Vacancy Rate:", round(100 * sim_vacancy_rate, 2), "%\n")
    cat("Avg Seller Payoff (Strong):", round(avg_seller_payoff["Strong"], 2), "\n")
    cat("Avg Seller Payoff (Weak):", round(avg_seller_payoff["Weak"], 2), "\n")
    cat("Avg Buyer Surplus:", round(avg_buyer_util, 2), "\n")}
  
  return(list(
    equilibrium = eqm,
    vacancy_theory = vacancy_theory,
    vacancy_simulated = sim_vacancy_rate,
    avg_payoffs = avg_seller_payoff,
    avg_buyer_utility = avg_buyer_util,
    thresholds = list(cS_thresh = cS_thresh, cW_thresh = cW_thresh),
    normalized = list(k_SH = k_SH, r_SH = r_SH, k_WH = k_WH, r_WH = r_WH,
                      k_SD = k_SD, r_SD = r_SD, k_WD = k_WD, r_WD = r_WD)
  ))
}

# Example call
res = run_sim(PH, PD, beta, V0S, V0W, aHS, aHW, aDS, aDW, cS, cW, sigma0, sigma1, N, echo = T)



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
p = 0.5       # Proportion of Strong Types
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

# Expand grid of all parameter combinations
grid <- expand.grid(aHS = aHS_vals,
                    aHW = aHW_vals,
                    aDS = aDS_vals,
                    aDW = aDW_vals,
                    PH = PH_vals,
                    PD = PD_vals,
                    beta = beta_vals)

# Initialize result storage
results <- list()

# Loop over grid and run simulations
for (i in 1:nrow(grid)) {
  print(i/nrow(grid)*100)
  params <- grid[i, ]
  tryCatch({
    sim <- run_sim(PH = params$PH, PD = params$PD, beta = params$beta,
                   V0S = V0S, V0W = V0W,
                   aHS = params$aHS, aHW = params$aHW,
                   aDS = params$aDS, aDW = params$aDW,
                   cS = cS, cW = cW,
                   sigma0 = sigma0, sigma1 = sigma1,
                   N = N)
    
    # Append results to list
    results[[length(results) + 1]] <- data.frame(
      aHS = params$aHS, aHW = params$aHW,
      aDS = params$aDS, aDW = params$aDW,
      PH = params$PH, PD = params$PD, beta = params$beta,
      equilibrium = sim$equilibrium,
      vacancy_theory = sim$vacancy_theory,
      vacancy_simulated = sim$vacancy_simulated,
      seller_payoff_strong = sim$avg_payoffs["Strong"],
      seller_payoff_weak = sim$avg_payoffs["Weak"],
      buyer_surplus = sim$avg_buyer_utility
    )
  }, error = function(e) {
    cat("Skipping combo due to error:", i, "\n")
  })
}

# Combine all into one data frame
df <- bind_rows(results)

write.csv(df, "sim_results.csv", row.names = FALSE)
