library(ggplot2)
library(splines)

set.seed(42)

# Część teoretyczna: Obrazowanie funkcji wygładzającej

base_dim <- 12
names_in_legends <- c("Dane", "Funkcje bazowe", "Funkcja wygładzająca")

# Generowanie danych
x_points <- runif(60, 0, 6.2)
y_points <- sin(x_points) + rnorm(60, 0, 0.4)
df_points <- data.frame(x = x_points, y = y_points)

df_points$label <- factor("Dane", levels = names_in_legends) 

# Funkcja wygładzająca
B_train <- bs(df_points$x, df = base_dim, intercept = TRUE)
model <- lm(df_points$y ~ B_train - 1)
betas <- coef(model) 

x_grid <- seq(0, 6.2, length.out = 100)
B_grid <- bs(x_grid, df = base_dim, intercept = TRUE)

y_est <- B_grid %*% betas
df_true <- data.frame(x = x_grid, y = as.vector(y_est))
df_true$label <- factor("Funkcja wygładzająca", levels = names_in_legends) 

df_dashed <- data.frame()
my_palette <- colorRampPalette(c("black", "green4", "dodgerblue", "magenta", "firebrick", "orange"))
colors_dashed <- my_palette(base_dim)

# Generowanie funkcji bazowych
for (k in 1:base_dim) {
  y_basis <- B_grid[, k] * betas[k]
  temp_df <- data.frame(x = x_grid, y = y_basis, group = as.character(k))

  temp_df$label <- factor("Funkcje bazowe", levels = names_in_legends)
  df_dashed <- rbind(df_dashed, temp_df)
}

# Wykres
ggplot() +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  geom_point(data = df_points,
             aes(x = x, y = y, shape = label),
             size = 2.5, color = "black", stroke = 0.8) +
  
  # Funkcja wygładzająca
  geom_line(data = df_true,
            aes(x = x, y = y, linetype = label),
            color = "red", linewidth = 2) +
  # Funkcje bazowe
  geom_line(data = df_dashed,
            aes(x = x, y = y, group = group, color = group, linetype = label),
            linewidth = 1.2, alpha = 0.8) +

  scale_color_manual(values = colors_dashed, guide = "none") +
  scale_shape_manual(
    name = NULL,
    values = c("Dane" = 1, "Funkcje bazowe" = NA, "Funkcja wygładzająca" = NA),
    drop = FALSE 
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c("Dane" = "blank", "Funkcje bazowe" = "dashed", "Funkcja wygładzająca" = "solid"),
    drop = FALSE 
  ) +
  guides(
    shape = guide_legend(override.aes = list(
      color = c("black", "grey40", "red"),
      linewidth = c(0, 0.6, 1.2),
      size = c(3, NA, NA)
    )),
    linetype = guide_legend(override.aes = list(
      color = c("black", "grey40", "red"),
      linewidth = c(0, 0.6, 1.2),
      size = c(3, NA, NA)
    ))
  ) +
  scale_x_continuous(breaks = 0:6) +
  scale_y_continuous(limits = c(-2.5, 2.5), breaks = seq(-2.0, 2.0, 1.0)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size=18),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 15), size = 14),
    axis.title.y = element_text(margin = margin(r = 15), size = 14),
    axis.text = element_text(size = 11, color = "black"),
    legend.text = element_text(size = 14),           
    legend.key.size = unit(1.5, "cm"),              
    legend.key.width = unit(2, "cm")
  ) +
  labs(x = "x", y = "y", title="Wizualizacja postaci funkcji wygładzającej dla K=12")
