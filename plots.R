library(tidyverse)
library(patchwork)
library(ggridges)
library(latex2exp)
library(purrr)

df = read.csv("sim_results.csv")
colnames(df)

df %>% select(`Simulated Vacancy` = vacancy_simulated, `Theoretical Vacancy` = vacancy_theory) %>% 
  pivot_longer(1:2, names_to = "Type", values_to = "Vacancy") %>%
  ggplot(aes(x = Vacancy, 
             fill = Type)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(
    title = TeX("Distribution Comparison of Simulated and Theoretical Vacancy Rates"),
    x = "Vacancy Rate",
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  ) 


################################################################################
################################################################################
# BOXPLOT
################################################################################
################################################################################


df %>% select(equilibrium, `Simulated Vacancy Rate (%)` = vacancy_simulated, 
              `Strong Seller Payoff` = seller_payoff_strong, 
              `Weak Seller Payoff` = seller_payoff_weak, 
              `Buyer Surplus` = buyer_surplus) %>%
  mutate(`Simulated Vacancy Rate (%)` = 100*`Simulated Vacancy Rate (%)`) %>%
  pivot_longer(2:5, names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(x = equilibrium, y = Value)) +
  geom_boxplot(outliers = F, fill = "lightblue") +
  facet_wrap(~Metric, scales = "free") +
  labs(title = "Variation of Key Metrics Across Equilibrium Types", x = "Equilibrium", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7)) +
  scale_fill_viridis_d() +
  guides(fill = "none")

################################################################################
################################################################################
# PH PD PLOT
################################################################################
################################################################################


df %>%
  group_by(PH, PD, equilibrium) %>%
  summarise(
    `Mean Vacancy` = mean(vacancy_simulated, na.rm = TRUE),
    `Mean Strong Seller Payoff` = mean(seller_payoff_strong, na.rm = TRUE),
    `Mean Weak Seller Payoff` = mean(seller_payoff_weak, na.rm = TRUE),
    `Mean Buyer Surplus` = mean(buyer_surplus, na.rm = TRUE),
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
        legend.key.width = unit(1.2, "cm"),   # makes legend bar longer
        legend.key.height = unit(0.4, "cm")   # thinner bar
      )
  }) -> plots
wrap_plots(plots)



df %>%
  group_by(PH, PD, equilibrium) %>%
  summarise(
    `Mean Vacancy` = mean(vacancy_simulated, na.rm = TRUE),
    `Mean Strong Seller Payoff` = mean(seller_payoff_strong, na.rm = TRUE),
    `Mean Weak Seller Payoff` = mean(seller_payoff_weak, na.rm = TRUE),
    `Mean Buyer Surplus` = mean(buyer_surplus, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  split(.$equilibrium) %>%
  imap(~{
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
        fill = NULL
      ) +
      theme(
        legend.position = "bottom",
        legend.key.width = unit(1.2, "cm"),
        legend.key.height = unit(0.4, "cm")
      ) +
      guides(fill = "none")
  }) -> plots

wrap_plots(plots)





p1 = df %>% 
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

df %>% 
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

p1 = df %>% 
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



p3 = df %>% 
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


