# Wizualizacja badanych funkcji

library(tidyverse)
library(ggplot2)

# Dane
n <- 1000

f1 <- function(x) sapply(x, function(x_i) 12 * x_i - 5)
f2 <- function(x) sapply(x, function(x_i) exp( -(x_i^2 - 20)/100 - 3 * x_i) * 10) # - x_i^2 + x_i
f3 <- function(x) sapply(x, function(x_i) sin(- 0.5 * x_i * pi) * 5 + cos(pi * x_i))
f4 <- function(x) sapply(x, function(x_i) ifelse(x_i > 2.5, ifelse(x_i < 4, 4, 2), x_i^2))
f5 <- function(x) sapply(x, function(x_i) 0.9 * x_i^3 - 8 * x_i^2 - 4.8 * x_i + 6)

x_1 <- runif(n, 0 , 10)
x_2 <- runif(n, 0 , 10)
x_3 <- runif(n, 0 , 10)
x_4 <- runif(n, 0 , 10)
x_5 <- runif(n, 0 , 10)
x_6 <- runif(n, 0 , 10)

data_df <- data.frame(x_1 = x_1,
                      x_2 = x_2,
                      x_3 = x_3,
                      x_4 = x_4,
                      x_5 = x_5,
                      f1_x = f1(x_1),
                      f2_x = f2(x_2),
                      f3_x = f3(x_3),
                      f4_x = f4(x_4),
                      f5_x = f5(x_5))

df_y <- data_df %>%
  select(starts_with("f")) %>%
  pivot_longer(everything(), names_to = "function_name", values_to = "y")

df_x <- data_df %>%
  select(starts_with("x")) %>%
  pivot_longer(everything(), names_to = "x_name", values_to = "x")

data_df_long <- bind_cols(df_x, df_y) %>%
  select(-x_name) %>%
  arrange(function_name, x)

# Wykres
ggplot(data_df_long, aes(x = x, y = y,)) +
  geom_line(linewidth = 1, alpha = 0.6) + 
  facet_wrap(~function_name, scales = "free_y") + 
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold", size=18),
    axis.title.x = element_text(margin = margin(t = 15), size = 14),
    axis.title.y = element_text(margin = margin(r = 15), size = 14),
    axis.text = element_text(size = 11, color = "black"),
    legend.text = element_text(size = 14),           
    legend.key.size = unit(1.5, "cm"),              
    legend.key.width = unit(2, "cm")
  ) +
  labs(title = "Zestawienie funkcji f1 - f5",
       x = "Wartość x",
       y = "f(x)") 
