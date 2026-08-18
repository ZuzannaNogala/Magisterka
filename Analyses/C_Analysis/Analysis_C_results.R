rm(list = ls())

library(mgcv)
library(tidyverse)
library(scales)
library(ggplot2)

# Analizy do C
# Wpływ bazy splajna

options(scipen=999)
df_C <- list() 

for (chunk in 1:10){
  df_C[[chunk]] <- readRDS(paste0("/Users/zuza/Desktop/studia/magisterka/Data/C_results/C_results_chunk_", chunk, ".RDS"))
}

df_C_full <- bind_rows(df_C)
df_C_full %>% head(20)

# 0. Czy wszyskie modele zbiegły?
mean(df_C_full$converged) # 1


# mechanizmy kontrolne: 

# k-index
ggplot(df_C_full%>% filter(n == 200), aes(x = factor(k), y = k_index, color= base)) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 1) + 
  facet_wrap(~ fun, ncol = 2) + 
  theme_bw() +
  labs(
    title = "Analiza C: Diagnoza autokorelacji reszt (k-index) dla n=200",
    subtitle = "Zależność k-index od rozmiaru bazy, rodzaju bazy i złożoności funkcji.\nWartości poniżej czerwonej przerywanej linii sugerują niewystarczający wymiar bazy k.",
    x = "Wymiar bazy splajna (k)",
    y = "Wartość statystyki k-index",
    color = "Typ bazy splajna"
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

ggplot(df_C_full%>% filter(n == 2000), aes(x = factor(k), y = k_index, color= base)) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 1) + 
  facet_wrap(~ fun, ncol = 2) + 
  ylim(c(0.25, 1.3)) +
  theme_bw() +
  labs(
    title = "Analiza C: Diagnoza autokorelacji reszt (k-index) dla n=2000",
    subtitle = "Zależność k-index od rozmiaru bazy, rodzaju bazy i złożoności funkcji.\nWartości poniżej czerwonej przerywanej linii sugerują niewystarczający wymiar bazy k.",
    x = "Wymiar bazy splajna (k)",
    y = "Wartość statystyki k-index",
    color = "Typ bazy splajna"
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

# k prop rejection
data_summ <-df_C_full %>%
  group_by(base, n, k, fun) %>%
  summarise(prop_k_rejected = mean(k_p_value <= 0.05))

data_summ %>% filter(n==2000) %>% 
  ggplot(aes(x = as.factor(k), y = prop_k_rejected, color = base, group = base)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", alpha = 0.9) +
  facet_wrap(~ fun, scales = "fixed") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Analiza C: Odsetek odrzuceń hipotezy o wystarczalności bazy k dla n=2000",
    subtitle = "w zależności od rodzaju i wymiaru bazy",
    x = "Wymiar bazy splajna (k)",
    y = "Wartość statystyki k-index",
    color = "Typ bazy splajna"
  ) +
  theme_bw() +
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


data_summ %>% filter(n==200) %>% 
  ggplot(aes(x = as.factor(k), y = prop_k_rejected, color = base, group = base)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", alpha = 0.9) +
  facet_wrap(~ fun, scales = "fixed") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Analiza C: Odsetek odrzuceń hipotezy o wystarczalności bazy k  dla n=200",
    subtitle = "w zależności od rodzaju i wymiaru bazy",
    x = "Wymiar bazy splajna (k)",
    y = "Odsetek odrzuceń k-index (k_pvalue < 0.05)",
    color = "Typ bazy splajna"
  ) +
  theme_bw() +
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

# 2. Czas estymacji

ggplot(df_C_full) +
  geom_boxplot(aes(x = as.factor(k), y = fit_time_sec, color = base), outlier.alpha = 0.3, outlier.size = 1) +
  facet_grid(n ~ fun, labeller = label_both) +
  theme_bw() +
  labs(title = "Analiza C: Rozkład czasu zbieżności modelu w zależności od wielkości próby, typu bazy",
       subtitle = expression(paste("Zestawienie dla funkcji ", f[1],"-", f[5])),
       x = "Wymiar bazy splajna (k)", y = "Czas (s)", color = "Typ bazy splajna") +
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

# 3. EDF
ggplot(df_C_full) +
  geom_boxplot(aes(x = as.factor(k), y = edf, color = base), outlier.alpha = 0.3, outlier.size = 1) +
  facet_grid(n ~ fun, labeller = label_both) +
  theme_bw() +
  labs(title = "Analiza C: Wpływ typu i wymiaru bazy splajna na EDF",
       subtitle = expression(paste("Zestawienie dla funkcji ", f[1],"-", f[5])),
       x = "Wymiar bazy splajna (k)", y = "EDF", color = "Typ bazy splajna") +
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


# ggplot(df_C_full%>% 
#          filter(n == 2000)) +
#   geom_boxplot(outlier.alpha = 0.3, outlier.size = 1, aes(x= as.factor(k), y=edf, color = base)) +
#   facet_wrap(~ fun, ncol = 2) +
#   theme_bw() +
#   labs(title = "Analiza C: Wpływ typu i wymiaru bazy splajna na EDF dla próby o wielkości n=2000",
#        subtitle = expression(paste("Zestawienie dla funkcji ", f[1],"-", f[5])),
#        x = "Wymiar bazy splajna (k)", y = "EDF", color = "Typ bazy splajna") +
#   theme(legend.position = "bottom",
#         plot.title = element_text(hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5))
# 
# 
# ggplot(df_C_full%>% 
#          filter(n == 200)) +
#   geom_boxplot(outlier.alpha = 0.3, outlier.size = 1, aes(x= as.factor(k), y=edf, color = base)) +
#   facet_wrap(~ fun, ncol = 2) +
#   theme_bw() +
#   labs(title = "Analiza C: Wpływ typu i wymiaru bazy splajna na EDF dla próby o wielkości n=200",
#        subtitle = expression(paste("Zestawienie dla funkcji ", f[1],"-", f[5])),
#        x = "Wymiar bazy splajna (k)", y = "EDF", color = "Typ bazy splajna") +
#   theme(legend.position = "bottom",
#         plot.title = element_text(hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5))


# 4. MSE

# ggplot(df_C_full) +
#   geom_boxplot(aes(x = as.factor(k), y = term_mse, color = base), outlier.alpha = 0.3, outlier.size = 1) +
#   facet_grid(n ~ fun, labeller = label_both, scales = "free_y") +
#   #scale_y_log10() +
#   # ylim(c(0, 0.15)) +
#   theme_bw() +
#   labs(title = "Analiza C: Wpływ typu i wymiaru bazy splajna na odwzorowanie kształtu funkcji dla próby o wielkości",
#        subtitle = expression(paste("Zestawienie dla funkcji ", f[1],"-", f[5], " na podstawie wartości MSE")),
#        x = "Wymiar bazy splajna (k)", y = "log10(MSE)", color = "Typ bazy splajna") +
#   theme(legend.position = "bottom",
#         plot.subtitle = element_text(hjust = 0.5, size=14),
#         strip.text = element_text(size = 12),
#         axis.text.x = element_text(angle = 0, hjust = 0.5),
#         plot.title = element_text(hjust = 0.5, face = "bold", size=16),
#         axis.title.x = element_text(margin = margin(t = 15), size = 12),
#         axis.title.y = element_text(margin = margin(r = 15), size = 12),
#         axis.text = element_text(size = 11, color = "black"),
#         legend.text = element_text(size = 12),           
#         legend.key.size = unit(1, "cm"),              
#         legend.key.width = unit(1, "cm"))


ggplot(df_C_full%>% 
         filter(n == 2000)) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 1, aes(x= as.factor(k), y=term_mse, color = base)) +
  facet_wrap(~ fun, ncol = 2, scale="free_y") +
  theme_bw() +
  labs(title = "Analiza C: Wpływ typu i wymiaru bazy splajna na odwzorowanie kształtu funkcji dla próby o wielkości n=2000",
       subtitle = expression(paste("Zestawienie dla funkcji ", f[1],"-", f[5], " na podstawie wartości MSE")),
       x = "Wymiar bazy splajna (k)", y = "MSE", color = "Typ bazy splajna") +
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


ggplot(df_C_full%>% 
         filter(n == 200)) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 1, aes(x= as.factor(k), y=term_mse, color = base)) +
  facet_wrap(~ fun, ncol = 2, scale="free_y") +
  theme_bw() +
  labs(title = "Analiza C: Wpływ typu i wymiaru bazy splajna na odwzorowanie kształtu funkcji dla próby o wielkości n=200",
       subtitle = expression(paste("Zestawienie dla funkcji ", f[1],"-", f[5], " na podstawie wartości MSE")),
       x = "Wymiar bazy splajna (k)", y = "MSE", color = "Typ bazy splajna") +
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



