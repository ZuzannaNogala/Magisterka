rm(list = ls())

library(tidyverse)
library(scales)
library(ggplot2)
library(ggh4x)

# Analizy do D
# Wpływ zmiennych nieistotnych

options(scipen=999)
df_D_1 <- list() 
df_D_2 <- list() 

for (chunk in 1:10){
  df_D_1[[chunk]] <- readRDS(paste0("/Users/zuza/Desktop/studia/magisterka/Data/D_results/D_results_chunk_", chunk, ".RDS"))
}

df_D_full_1 <- bind_rows(df_D_1)
df_D_full_1 <- df_D_full_1 %>% filter(noise_correlated_status==FALSE)

for (chunk in 1:10){
  df_D_2[[chunk]] <- readRDS(paste0("/Users/zuza/Desktop/studia/magisterka/Data/D_results/D_results_2_chunk_", chunk, ".RDS"))
}

df_D_full_2 <- bind_rows(df_D_2)

df_D_full_2 %>% nrow()
df_D_full_1 %>% nrow()

df_D_full <- rbind(df_D_full_1, df_D_full_2)

df_D_full %>% nrow()
df_D_full%>% group_by(noise_correlated_status) %>% count()


rm(df_D_full_1, df_D_full_2, df_D_1, df_D_2)
rm(chunk)

# Dane

df_D_full %>% head(10)

df_D_full  <- df_D_full %>%
  mutate(
    effect_type = case_when(
      var_name %in% c("x1", "x2", "x3", "x4", "x5") & noise_correlated_status == FALSE ~ "Prawdziwe sygnały (szum nieskorelowany)",
      var_name %in% c("x1", "x2", "x3", "x4", "x5") & noise_correlated_status == TRUE  ~ "Prawdziwe sygnały (szum skorelowany)",
      
      var_name %in% c("x6", "x7", "x8") & noise_correlated_status == FALSE ~ "Zaszumione efekty (nieskorelowane)",
      var_name %in% c("x6", "x7", "x8") & noise_correlated_status == TRUE  ~ "Zaszumione efekty (skorelowane)",
      
      TRUE ~ "Inne"
    )
  )


# mechanizmy kontrolne
mean(df_D_full$converged) # 1
unique(df_D_full$var_name)

var_labels <- c(
  "x1" = "f1",
  "x2" = "f2",
  "x3" = "f3",
  "x4" = "f4",
  "x5" = "f5",
  "x6" = "g6",
  "x7" = "g7",
  "x8" = "g8"
)

# k-index
ggplot(df_D_full, aes(x = factor(n), y = k_index, color =  effect_type)) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 1, linewidth = 0.9, 
               position = position_dodge(width = 0.75)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 1) + 
  facet_wrap(~ var_name, ncol = 2, 
             labeller = as_labeller(var_labels, default = label_parsed)) + 
  scale_x_discrete(labels = c("200" = "k=20 (n=200)", "2000" = "k=40 (n=2000)")) +
  scale_color_manual(
    values = c(
      "Prawdziwe sygnały (szum nieskorelowany)" = "#74add1", 
      "Prawdziwe sygnały (szum skorelowany)"    = "#2c7bb6", 
      "Zaszumione efekty (nieskorelowane)"      = "#fdae61",
      "Zaszumione efekty (skorelowane)"         = "#d7191c"  
    )
  ) +
  theme_bw() +
  labs(
    title = "Analiza D: Diagnoza autokorelacji reszt (k-index)",
    subtitle = "Zależność k-index od rozmiaru bazy, złożoności funkcji i zmiennych nieistotnych.\nWartości poniżej czerwonej przerywanej linii sugerują niewystarczający wymiar bazy k.",
    x = "Wymiar bazy splajna (k)",
    y = "Wartość statystyki k-index",
    color = "Rodzaj efektu"
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
        legend.key.width = unit(1, "cm")) +
  guides(color = guide_legend(ncol = 2, byrow = TRUE))


# k prop rejection 

data_summ <-df_D_full %>%
  group_by(n, var_name, effect_type) %>%
  summarise(prop_k_rejected = mean(k_p_value <= 0.05))


ggplot(data_summ, aes(x = factor(n), y = var_name, fill = prop_k_rejected)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(prop_k_rejected, 3)), color = "white", fontface = "bold") +
  scale_fill_distiller(palette = "RdYlGn", direction = -1, limits = c(0, 1)) +
  facet_grid(effect_type ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_discrete(labels = c("200" = "k=20 (n=200)", "2000" = "k=40 (n=2000)")) +
  scale_y_discrete(limits = rev, labels = var_labels) +
  theme_minimal() +
  labs(
    title = "Analiza D: Odsetek odrzuceń hipotezy o wystarczalności bazy",
    subtitle = "w zależności od rodzaju efektu",
    x = "Wymiar bazy splajna (k)",
    y = "Badana funkcja\ni rodzaj efektu", 
    fill = "Średnia proporcja odrzuceń"
  ) +
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
        legend.key.width = unit(1, "cm")) +
guides(color = guide_legend(ncol = 2, byrow = TRUE))

## Istotność zmiennych

data_signif <- df_D_full %>%
  mutate(
    effect_type = case_when(
      var_name %in% c("x1", "x2", "x3", "x4", "x5") & noise_correlated_status == FALSE ~ "Prawdziwe sygnały (szum nieskorelowany)",
      var_name %in% c("x1", "x2", "x3", "x4", "x5") & noise_correlated_status == TRUE  ~ "Prawdziwe sygnały (szum skorelowany)",
      var_name %in% c("x6", "x7", "x8") & noise_correlated_status == FALSE ~ "Zaszumione efekty (nieskorelowane)",
      var_name %in% c("x6", "x7", "x8") & noise_correlated_status == TRUE  ~ "Zaszumione efekty (skorelowane)",
      TRUE ~ "Inne"
    )
  ) %>%
  mutate(effect_type = factor(effect_type, levels = c(
    "Prawdziwe sygnały (szum nieskorelowany)",
    "Prawdziwe sygnały (szum skorelowany)",
    "Zaszumione efekty (nieskorelowane)",
    "Zaszumione efekty (skorelowane)"
  ))) %>%
  group_by(n, var_name, effect_type) %>%
  summarise(prop_signif = mean(signif_var <= 0.05, na.rm = TRUE), .groups = "drop")


ggplot(data_signif, aes(x = factor(n), y = var_name, fill = prop_signif)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(prop_signif, 3)), color = "black", fontface = "bold") +
  scale_fill_distiller(palette = "RdYlGn", direction = 1, limits = c(0, 1)) +
  facet_grid(
    effect_type ~ ., 
    scales = "free_y", 
    space = "free_y", 
    switch = "y"
  ) +
  scale_x_discrete(labels = c("200" = "k=20 (n=200)", "2000" = "k=40 (n=2000)")) +
  scale_y_discrete(limits = rev, labels = var_labels) +
  theme_minimal() +
  labs(
    title = "Odsetek istotnych statystycznie efektów (p-value \u2264 0.05)",
    subtitle = "Moc testu dla prawdziwych sygnałów vs Błąd I rodzaju dla szumu",
    x = "Wymiar bazy splajna (k)",
    y = "Badana funkcja\ni rodzaj efektu",
    fill = "Odsetek odrzuceń H0"
  ) +
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
     legend.key.width = unit(1, "cm"),
    strip.placement = "outside", 
    panel.spacing = unit(0.4, "lines"),
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 11, color = "black"),
    strip.background = element_rect(fill = "#e0e0e0", color = "black", linewidth = 0.8)
  )

## EDF

ggplot(df_D_full, aes(x = factor(n), y = edf,  color = effect_type)) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 1, linewidth = 0.8,
               position = position_dodge(width = 0.75)) +
  scale_x_discrete(labels = c("200" = "k=20 (n=200)", "2000" = "k=40 (n=2000)")) +
  facet_wrap(. ~ var_name, 
             labeller = 
               as_labeller(var_labels, default = label_parsed),
             , ncol = 2, scales = "free_y") +
  
  scale_color_manual(
    values = c(
      "Prawdziwe sygnały (szum nieskorelowany)" = "#74add1", # Jasny pastelowy błękit
      "Prawdziwe sygnały (szum skorelowany)"    = "#2c7bb6", # Ciemny, mocny niebieski
      "Zaszumione efekty (nieskorelowane)"      = "#fdae61", # Jasny pastelowy pomarańcz
      "Zaszumione efekty (skorelowane)"         = "#d7191c"  # Mocna czerwień
    )
  ) +
  
  theme_bw() +
  labs(
    title = "Analiza D: Wpływ zmiennych nieistotnych na EDF",
    subtitle = expression(paste("Zestawienie dla funkcji ", f[1],"-", f[5], " oraz ", g[6], "-", g[8])),
    x = "Wymiar bazy splajna (k)", 
    y = "Efektywne Stopnie Swobody (EDF)", 
    color = "Rodzaj efektu"
  ) +
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
        legend.key.width = unit(1, "cm")) +
  guides(color = guide_legend(ncol = 2, byrow = TRUE))

# Zwrócić uwagę na EDF jak czesto blisko 0, z jakim odchyleniem

# MSE

ggplot(df_D_full, aes(x = factor(n), y = term_mse,  color = effect_type)) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 1, linewidth = 0.8,
               position = position_dodge(width = 0.75)) +
  scale_x_discrete(labels = c("200" = "k=20 (n=200)", "2000" = "k=40 (n=2000)")) +
  
  facet_wrap(. ~ var_name, 
             labeller = 
               as_labeller(var_labels, default = label_parsed),
             , ncol = 2, scales = "free_y") +
  scale_color_manual(
    values = c(
      "Prawdziwe sygnały (szum nieskorelowany)" = "#74add1", 
      "Prawdziwe sygnały (szum skorelowany)"    = "#2c7bb6", 
      "Zaszumione efekty (nieskorelowane)"      = "#fdae61", 
      "Zaszumione efekty (skorelowane)"         = "#d7191c"  
    )
  ) +
  theme_bw() +
  labs(
    title = "Analiza D: Wpływ zmiennych nieistotnych na odwzorowanie kształtu funkcji",
    subtitle = expression(paste("Zestawienie dla funkcji ", f[1],"-", f[5], " oraz ", g[6],"-", g[8], " na podstawie wartości MSE")),
    x = "Wymiar bazy splajna (k)", 
    y = "MSE", 
    color = "Rodzaj efektu"
  ) +
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
        legend.key.width = unit(1, "cm")) +
  guides(color = guide_legend(ncol = 2, byrow = TRUE))


# Tabela z outlierami

df_D_full %>% filter(var_name == "x8" & noise_correlated_status==FALSE & n == 2000) %>% nrow()

df_D_full %>%
  filter(var_name %in% c("x1", "x6", "x7", "x8")) %>%
  group_by(var_name, n, noise_correlated_status) %>%
  summarise(
    "prop_less_0.005" = sum(edf < 0.005) / 500,
    "prop_less_0.05" = sum(edf < 0.05) / 500,
    "prop_less_0.5" = sum(edf < 0.5) / 500,
    "prop_less_1" = sum(edf < 1) / 500
  ) 


df_D_full %>%
  filter(var_name %in% c("x6", "x7", "x8")) %>%
  group_by(var_name, n, noise_correlated_status) %>%
  summarise(
    "mean_edf" = mean(edf),
    "sd_edf" = sd(edf)
  )


df_D_full %>%
  group_by(var_name, n, noise_correlated_status) %>%
  summarise(
    q1  = quantile(edf, 0.25, na.rm = TRUE),
    q3  = quantile(edf, 0.75, na.rm = TRUE),
    iqr = IQR(edf, na.rm = TRUE),
    
    n_lower_outliers = sum(edf < (q1 - 1.5 * iqr), na.rm = TRUE) / n(),
    n_upper_outliers = sum(edf > (q3 + 1.5 * iqr), na.rm = TRUE) / n(),
    .groups = "drop"
  ) %>% print(n=40)



