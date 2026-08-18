rm(list = ls())

library(mgcv)
library(tidyverse)
library(scales)
library(parallel)
library(ggplot2)

# Analiza wpływu k i gamma na wyniki

B_file <- "/Users/zuza/Desktop/studia/magisterka/Data/B_results_df.RDS"
B_final_df <- readRDS(B_file)

# Sprawdzenie warunków kontrolnych

mean(B_final_df$converged) # 1

plot_data <- B_final_df %>%
  group_by(k, gamma) %>%
  summarise(
    mean_edf = mean(edf),
    mean_mse = median(term_mse),
    prop_k_rejected = mean(k_p_value < 0.05), 
    .groups = 'drop'
  )


ggplot(B_final_df, aes(x = factor(k), y = k_index, color=factor(k))) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 1, linewidth = 0.9) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 1) + 
  facet_wrap(~ fun, ncol = 1, scales = "free_y") +
  theme_bw() +
  labs(
    title = expression(paste(bold("Analiza B: Analiza autokorelacji reszt (k-index) dla funkcji "), bold(f[3]))),
    subtitle = "Wartość k-index w zależności od testowanego wymiaru bazy splajna (k).\nWartości poniżej czerwonej przerywanej linii sugerują niewystarczający wymiar bazy k.",
    x = "Testowany wymiar splajna (k)",
    y = "Wartość statystyki k-index"
  ) +
  theme(legend.position = "none",
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





ggplot(plot_data, aes(x = factor(k), y = factor(gamma), fill = prop_k_rejected)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(prop_k_rejected, 3)), color = "white", fontface = "bold") +
  scale_fill_distiller(palette = "RdYlGn", direction = -1, limits = c(0, 1)) +
  theme_minimal() +
  labs(
    title = expression(paste(bold("Analiza B: Wpływ wymiaru bazy (k) i mnożnika ("), bold(gamma), bold(") na proporcję odrzuceń"))),
    x = "Wymiar bazy splajna (k)",
    y = expression(paste("Wartość mnożnika (", gamma, ")")),
    fill = "Średnia proporcja odrzuceń"
  ) +
  theme(legend.position = "none",
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

k_labels <- c(
  "10" = "k==10",
  "20" = "k==20",
  "30" = "k==30",
  "40" = "k==40",
  "50" = "k==50"
)

ggplot(B_final_df, aes(x = factor(gamma), y = edf, color = factor(gamma))) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 1) +
  theme_bw() +
  facet_wrap(~ k, ncol = 5, 
             labeller = as_labeller(k_labels, default = label_parsed)) +
  labs(
    x = expression(paste(bold("Wartość mnożnika ("), bold(gamma), ")")),
    y = "Efektywne Stopnie Swobody (EDF)",
    color = expression(gamma),
    title = expression(paste(bold("Analiza B: Wpływ wymiaru bazy splajna (k) oraz mnożnika ("), bold(gamma), bold(") na EDF"))),
    subtitle = expression(paste("Zestawienie dla funkcji ", f[3]," (krzywa trygonometryczna)")),
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

ggplot(B_final_df, aes(x = factor(gamma), y = term_mse, color = factor(gamma))) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 1, linewidth=0.9) +
  scale_y_log10() +
  theme_bw() +
  facet_wrap(~ k, ncol = 5, 
             labeller = as_labeller(k_labels, default = label_parsed)) +
  labs(
    x = expression(paste("Wartość (", gamma, ")")),
    y = expression(paste(log[10], "(MSE)")),
    color = expression(gamma),
    title = expression(paste(bold("Analiza B: Wpływ wymiaru bazy splajna (k) oraz mnożnika ("), bold(gamma), bold(") na odwzorowanie kształtu funkcji" ))),
    subtitle = expression(paste("Zestawienie dla funkcji ", f[3], " (krzywa trygonometryczna) na podstawie wartości MSE")),
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


