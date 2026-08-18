# Analiza A: Podsumowanie wyników

library(tidyverse)
library(scales)
library(ggplot2)

options(scipen=999)

# Wyniki
file_A_2 <- "/Users/zuza/Desktop/studia/magisterka/Data/A_results_df_2.RDS"
file_A_1 <- "/Users/zuza/Desktop/studia/magisterka/Data/A_results_df_400.RDS"

A_final_df_2 <- readRDS(file_A_2)
A_final_df_1 <- readRDS(file_A_1)

A_final_df <- rbind(A_final_df_1, A_final_df_2)

A_final_df %>% head(10)

data_summ <- A_final_df %>% 
group_by(fun, method, n) %>%
  summarise(
          # 1. Jak model ocenia złożoność i jak się myli?
    mean_edf = mean(edf),
    sd_edf = sd(edf),           
    mean_term_mse = mean(term_mse),
          
          # 2. Diagnostyka bazy
    prop_k_rejected = mean(k_p_value < 0.05), 
          
          # 3. Kwestie techniczne algorytmu
    prop_converged = mean(converged))

# Mechanizmy kontrolne:

data_summ %>%
  mutate(n = as.factor(n)) %>% 
  ggplot(aes(x = n, y = prop_k_rejected, color = method, group = method)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", alpha = 0.9) +
  facet_wrap(~ fun, scales = "fixed") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(
    labels = c(
      "50"   = "n=50\n(k=8)",
      "100"  = "n=100\n(k=12)",
      "200"  = "n=200\n(k=20)",
      "1000" = "n=1000\n(k=40)",
      "5000" = "n=5000\n(k=40)"
    )
  ) +
  scale_color_discrete(labels = function(method_name) ifelse(method_name == "GCV.Cp", "GCV", method_name)) +
  labs(
    title = "Analiza A: Odsetek odrzuceń hipotezy o wystarczalności bazy k",
    subtitle = "w zależności od kryterium wygładzania i rozmiaru próby",
    x = "Rozmiar próby",
    y = "Odsetek odrzuceń k-index (k-pvalue < 0.05)",
    color = "Kryterium"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.subtitle = element_text(hjust = 0.5, size=14),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5, face = "bold", size=16),
        axis.title.x = element_text(margin = margin(t = 15), size = 12),
        axis.title.y = element_text(margin = margin(r = 15), size = 12),
        axis.text = element_text(size = 11, color = "black"),
        legend.text = element_text(size = 12),           
        legend.key.size = unit(1, "cm"),              
        legend.key.width = unit(1, "cm"))


ggplot(A_final_df, aes(x = factor(n), y = k_index, color = method)) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 1) + 
  facet_wrap(~ fun, ncol = 2, scales = "free_y") + 
  scale_x_discrete(
    labels = c(
      "50"   = "n=50\n(k=8)",
      "100"  = "n=100\n(k=12)",
      "200"  = "n=200\n(k=20)",
      "1000" = "n=1000\n(k=40)",
      "5000" = "n=5000\n(k=40)"
    )
  ) +
  scale_color_discrete(labels = function(method_name) 
    ifelse(method_name == "GCV.Cp", "GCV", method_name)) +
  theme_bw() +
  labs(
    title = "Analiza A: Analiza autokorelacji reszt (k-index)",
    subtitle = "Zależność k-index od rozmiaru próby, metody estymacji i funkcji.\nWartości poniżej czerwonej przerywanej linii sugerują niewystarczający wymiar bazy k.",
    x = "Rozmiar próby",
    y = "Wartość statystyki k-index",
    color = "Kryterium"
  ) +
  theme(legend.position = "top",
        plot.subtitle = element_text(hjust = 0.5, size=14),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5, face = "bold", size=16),
        axis.title.x = element_text(margin = margin(t = 15), size = 12),
        axis.title.y = element_text(margin = margin(r = 15), size = 12),
        axis.text = element_text(size = 11, color = "black"),
        legend.text = element_text(size = 12),           
        legend.key.size = unit(1, "cm"),              
        legend.key.width = unit(1, "cm"))

# MSE

ggplot(A_final_df, aes(x = factor(n), y = term_mse, color=method)) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 1) +
  scale_y_log10() + # Obowiązkowe dla MSE!
  facet_wrap(~ fun,  ncol = 2) + # ncol=1 ułoży je w pionie
  theme_bw() +
  scale_x_discrete(
    labels = c(
      "50"   = "n=50\n(k=8)",
      "100"  = "n=100\n(k=12)",
      "200"  = "n=200\n(k=20)",
      "1000" = "n=1000\n(k=40)",
      "5000" = "n=5000\n(k=40)"
    )
  ) +
  scale_color_discrete(labels = function(method_name) 
    ifelse(method_name == "GCV.Cp", "GCV", method_name)) +
  labs(
    title = "Analiza A: Wpływ kryterium wygładzania na dokładność odwzorowania kształtu funkcji",
    subtitle = "Porównanie na podstawie wartości MSE dla różnej wielkości próby",
    x = "Rozmiar próby",
    y = expression(log[10](MSE)),
    color="Kryterium"
  ) +
  theme(legend.position = "top",
        plot.subtitle = element_text(hjust = 0.5, size=14),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5, face = "bold", size=16),
        axis.title.x = element_text(margin = margin(t = 15), size = 12),
        axis.title.y = element_text(margin = margin(r = 15), size = 12),
        axis.text = element_text(size = 11, color = "black"),
        legend.text = element_text(size = 12),           
        legend.key.size = unit(1, "cm"),              
        legend.key.width = unit(1, "cm"))

# EDF

ggplot(A_final_df, aes(x = factor(n), y = edf, color = method)) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 1) +
  facet_wrap(~ fun,  ncol = 2) + # ncol=1 ułoży je w pionie
  scale_x_discrete(
    labels = c(
      "50"   = "n=50\n(k=8)",
      "100"  = "n=100\n(k=12)",
      "200"  = "n=200\n(k=20)",
      "1000" = "n=1000\n(k=40)",
      "5000" = "n=5000\n(k=40)"
    )
  ) +
  scale_color_discrete(labels = function(method_name) 
    ifelse(method_name == "GCV.Cp", "GCV", method_name)) +
  theme_bw() +
  labs(
    title = "Analiza A: Wpływ kryterium wygładzania na estymacje efektywnych stopni swobody (EDF)",
    subtitle = "Porównanie dla różnej wielkości prób",
    x = "Rozmiar próby",
    y = "EDF",
    color = "Kryterium"
  ) +
  theme(legend.position = "top",
        plot.subtitle = element_text(hjust = 0.5, size=14),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5, face = "bold", size=16),
        axis.title.x = element_text(margin = margin(t = 15), size = 12),
        axis.title.y = element_text(margin = margin(r = 15), size = 12),
        axis.text = element_text(size = 11, color = "black"),
        legend.text = element_text(size = 12),           
        legend.key.size = unit(1, "cm"),              
        legend.key.width = unit(1, "cm"))

