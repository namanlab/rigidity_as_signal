library(tidyverse)
library(patchwork)
library(ggridges)
library(latex2exp)
library(purrr)

# dft = read.csv("results/sim_results_p_5.csv") 
df = read.csv("results/sim_results_p_1.csv")
for (p_val in seq(0.3, 0.9, by = 0.2)){
  fp = paste0("results/sim_results_p_", p_val*10, ".csv")
  df = rbind(df, read.csv(fp))
}
colnames(df)

################################################################################
################################################################################
# THEORETICAL VS ACTUAL VACANCY: NEED ALL DATA
################################################################################
################################################################################


df %>% select(equilibrium, `Simulated Vacancy` = vacancy_simulated, `Theoretical Vacancy` = vacancy_theory) %>% 
  pivot_longer(2:3, names_to = "Type", values_to = "Vacancy") %>%
  ggplot(aes(x = Vacancy, 
             fill = Type)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(
    title = TeX("Distribution Comparison of Simulated and Theoretical Vacancy Rates"),
    x = "Vacancy Rate",
    y = "Density"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )  +
  facet_wrap(~equilibrium, scales = "free")


################################################################################
################################################################################
# BOXPLOT: NEED ALL DATA/ 3 DIFFERNT PLOTS WITH 2 IN APPENDIX
################################################################################
################################################################################

# BASE
df %>% select(equilibrium, `Simulated Vacancy Rate (%)` = vacancy_simulated, 
              `Strong Seller Payoff` = strong_seller_payoff_simulated, 
              `Weak Seller Payoff` = weak_seller_payoff_simulated, 
              `Buyer Surplus` = buyer_utility_simulated) %>%
  mutate(`Simulated Vacancy Rate (%)` = 100*`Simulated Vacancy Rate (%)`) %>%
  pivot_longer(2:5, names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(x = equilibrium, y = Value)) +
  geom_boxplot(outliers = F, fill = "lightblue") +
  facet_wrap(~Metric, scales = "free") +
  labs(title = "Variation of Key Metrics Across Equilibrium Types", x = "Equilibrium", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7)) +
  scale_fill_viridis_d() +
  guides(fill = "none")

# WITH P
df %>% select(equilibrium, p, `Strong Seller Payoff` = strong_seller_payoff_simulated, 
              `Weak Seller Payoff` = weak_seller_payoff_simulated) %>%
  mutate(p = as.factor(p)) %>% 
  pivot_longer(3:4, names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(x = p, y = Value, fill = equilibrium)) +
  geom_boxplot(outliers = F) +
  facet_wrap(~Metric, ncol = 1) +
  labs(title = "Variation of Key Metrics Across p and Equilibrium Types", x = "p", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7)) +
  scale_fill_viridis_d()

df %>% select(equilibrium, p, `Strong Seller Payoff` = strong_seller_payoff_simulated, 
              `Weak Seller Payoff` = weak_seller_payoff_simulated) %>%
  mutate(p = as.factor(p)) %>% 
  pivot_longer(3:4, names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(x = p, y = Value, fill = Metric)) +
  geom_boxplot(outliers = F) +
  facet_wrap(~equilibrium, ncol = 1) +
  labs(title = "Variation of Key Metrics Across p and Equilibrium Types", x = "p", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7)) +
  scale_fill_viridis_d()

################################################################################
################################################################################
# PH PD PLOT
################################################################################
################################################################################

# TILE
df %>%
  group_by(PH, PD, equilibrium) %>%
  summarise(
    `Mean Vacancy` = mean(vacancy_simulated, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  split(.$equilibrium) %>%
  imap(~{
    ggplot(.x, aes(x = PH, y = PD, fill = `Mean Vacancy`)) +
      geom_raster(interpolate = TRUE) +
      theme_minimal() +
      scale_fill_gradient2(
        low = "steelblue",
        mid = "white",
        high = "red",
        midpoint = mean(.x$`Mean Vacancy`, na.rm = TRUE),
        breaks = scales::pretty_breaks(n = 4),  # fewer legend ticks
      ) +
      labs(
        title = paste(.y),
        x = TeX("$P_{H}$"),
        y = TeX("$P_{D}$"),
        fill = NULL
      ) +
      theme(
        legend.position = "bottom",
        legend.key.width = unit(1.2, "cm"),
        legend.key.height = unit(0.4, "cm")
      )
  }) -> plots
wrap_plots(plots)

# Contour
plots <- df %>%
  group_by(PH, PD, equilibrium) %>%
  summarise(
    `Mean Vacancy` = mean(vacancy_simulated, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  split(.$equilibrium) %>%
  imap(~{
    range_vals <- range(.x$`Mean Vacancy`, na.rm = TRUE)
    range_text <- paste0("Range: ", round(range_vals[1], 3), " – ", round(range_vals[2], 3))
    
    ggplot(.x, aes(x = PH, y = PD, z = `Mean Vacancy`)) +
      geom_contour_filled(bins = 20) +
      theme_minimal() +
      scale_fill_manual(
        values = colorRampPalette(c("steelblue", "white", "red"))(20)
      ) +
      labs(
        title = paste(.y),
        x = TeX("$P_{H}$"),
        y = TeX("$P_{D}$"),
        fill = NULL,
        subtitle = range_text   # 👈 show range directly under the title
      ) +
      theme(
        legend.position = "none" # 👈 no legend
      )
  })

# Combine plots and add one caption at bottom
wrap_plots(plots) +
  plot_annotation(
    caption = "Note: Legends are omitted to keep the plots clean, as many contour bands stack and overlap. Ranges are shown under each panel."
  )



# DENSITY
p1 = df %>% 
  filter(p == 0.5) %>%
  mutate(beta = factor(PH)) %>% 
  ggplot(aes(x = vacancy_simulated, 
             y = beta)) +
  geom_density_ridges(fill = "lightblue") +
  scale_y_discrete(limits = rev) +  # reverse y-axis order
  theme_minimal() +
  labs(
    title = TeX("Distribution of Vacancy Rates by $\\P_H$"),
    x = "Simulated Vacancy Rate",
    y = TeX("$\\P_H$"),
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )

p2 = df %>% 
  filter(p == 0.5) %>%
  mutate(beta = factor(PD)) %>% 
  ggplot(aes(x = vacancy_simulated, 
             y = beta)) +
  geom_density_ridges(fill = "lightblue") +
  scale_y_discrete(limits = rev) +  # reverse y-axis order
  theme_minimal() +
  labs(
    title = TeX("Distribution of Vacancy Rates by $\\P_D$"),
    x = "Simulated Vacancy Rate",
    y = TeX("$\\P_D$"),
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )

wrap_plots(p1, p2)



################################################################################
################################################################################
# BETA PLOT
################################################################################
################################################################################

# DENSITY
df %>% 
  filter(p == 0.5) %>%
  mutate(beta = factor(beta)) %>% 
  ggplot(aes(x = vacancy_simulated, 
             y = beta)) +
  geom_density_ridges(fill = "lightblue") +
  scale_y_discrete(limits = rev) +  # reverse y-axis order
  theme_minimal() +
  labs(
    title = TeX("Distribution of Vacancy Rates Across Different $\\beta$ Values"),
    x = "Simulated Vacancy Rate",
    y = TeX("$\\beta$"),
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )
  


################################################################################
################################################################################
# MULTIPLIER PLOT
################################################################################
################################################################################

# DENSITY
p1 = df %>% 
  filter(p == 0.5) %>%
  filter(aHS %in% seq(1.25, 1.55, by = 0.05)) %>%
  mutate(beta = factor(aHS)) %>% 
  ggplot(aes(x = vacancy_simulated, 
             y = beta)) +
  geom_density_ridges(fill = "lightblue") +
  scale_y_discrete(limits = rev) +  # reverse y-axis order
  theme_minimal() +
  labs(
    title = TeX("Distribution of Vacancy Rates by $\\alpha_H^S$"),
    x = "Simulated Vacancy Rate",
    y = TeX("$\\alpha_H^S$"),
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )

p2 = df %>% 
  filter(p == 0.5) %>%
  filter(aHW %in% seq(1.25, 1.55, by = 0.05)) %>%
  mutate(beta = factor(aHW)) %>% 
  ggplot(aes(x = vacancy_simulated, 
             y = beta)) +
  geom_density_ridges(fill = "lightblue") +
  scale_y_discrete(limits = rev) +  # reverse y-axis order
  theme_minimal() +
  labs(
    title = TeX("Distribution of Vacancy Rates by $\\alpha_H^W$"),
    x = "Simulated Vacancy Rate",
    y = TeX("$\\alpha_H^W$"),
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )

wrap_plots(p1, p2)


# DENSITY
p3 = df %>% 
  filter(p == 0.5) %>%
  mutate(beta = factor(aDS)) %>% 
  ggplot(aes(x = vacancy_simulated, 
             y = beta)) +
  geom_density_ridges(fill = "lightblue") +
  scale_y_discrete(limits = rev) +  # reverse y-axis order
  theme_minimal() +
  labs(
    title = TeX("Distribution of Vacancy Rates by $\\alpha_D^S$"),
    x = "Simulated Vacancy Rate",
    y = TeX("$\\alpha_D^S$"),
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )

p4 = df %>% 
  filter(p == 0.5) %>%
  filter(aDW %in% seq(0.3, 0.7, by = 0.05)) %>%
  mutate(beta = factor(aDW)) %>% 
  ggplot(aes(x = vacancy_simulated, 
             y = beta)) +
  geom_density_ridges(fill = "lightblue") +
  scale_y_discrete(limits = rev) +  # reverse y-axis order
  theme_minimal() +
  labs(
    title = TeX("Distribution of Vacancy Rates by $\\alpha_D^W$"),
    x = "Simulated Vacancy Rate",
    y = TeX("$\\alpha_D^W$"),
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )

wrap_plots(p3, p4)





################################################################################
################################################################################
# p PLOT
################################################################################
################################################################################


df %>% 
  mutate(p = factor(p)) %>% 
  ggplot(aes(x = vacancy_simulated, 
             y = p)) +
  geom_density_ridges(fill = "lightblue") +
  scale_y_discrete(limits = rev) +  # reverse y-axis order
  theme_minimal() +
  labs(
    title = TeX("Distribution of Vacancy Rates Across $p$"),
    x = "Simulated Vacancy Rate",
    y = TeX("$p$"),
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )



