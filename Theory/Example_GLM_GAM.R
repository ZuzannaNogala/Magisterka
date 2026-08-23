#install.packages("ISLR")
library(ISLR)
library(tidyverse)
library(mgcv)

# Cześć teoretyczna: Przykład GLM vs GAM
# Modelowanie średnich zarobków w zależności od wieku

# Dane
data(Wage)
?Wage 

plot(Wage$age, Wage$wage)

Wage <- as.data.frame(Wage)
Wage %>% head(10)

# Analiza liczności 
Wage %>%
  group_by(year) %>% count()

df_plot_1 <- Wage %>% group_by(age) %>% count() %>% print(n=60) 

plot(df_plot_1$age, df_plot_1$n) # Mało młodych (<=20) i straszych (>=70)

# Łączenie grup,  sortowanie i liczenie średnich zarobków
(df_plot <-Wage %>%
  mutate(age_group = case_when(
    age <= 20 ~ "20-",                    
    age >= 70 ~ "70+",                    
    TRUE ~ as.character(age)
  )) %>%
  mutate(age_group = factor(age_group, levels = c("20-", as.character(21:69), "70+"))) %>%
  group_by(age_group) %>%
  summarise(mean_wage = mean(wage)))

chosen_labels <- c("20-", as.character(seq(25, 69, by = 5)), "70+")

# Wykres danych
ggplot(df_plot) +
  geom_point(aes(x = age_group, y = mean_wage), color = "darkblue", size = 4) +
  scale_x_discrete(breaks = chosen_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        plot.title = element_text(hjust = 0.5, size=18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size=16),
        axis.title.x = element_text(margin = margin(t = 15), size = 14),
        axis.title.y = element_text(margin = margin(r = 15), size = 14),
        axis.text = element_text(size = 11, color = "black"))+
  labs(
    title = "Kształtowanie się średnich zarobków w zależności od wieku",
    subtitle = "Dane dla 3000 pracujących mężczyzn w USA (region Mid-Atlantic), lata 2003-2009",
    x = "Wiek pracownika",
    y = "Średnie wynagrodzenie (tys. USD)"
  )

# Kolumna z wiekiem, ale liczbowo (do wykresu)
df_plot <- df_plot %>%
  mutate(age_num = case_when(
    age_group == "20-" ~ 20,
    age_group == "70+" ~ 70,
    TRUE ~ as.numeric(as.character(age_group))
  ))

Wage <- Wage %>%
  mutate(age_group = case_when(
    age <= 20 ~ "20-",
    age >= 70 ~ "70+",
    TRUE ~ as.character(age)
  )) %>%
  mutate(age_group = factor(age_group, levels = c("20-", as.character(21:69), "70+")))


Wage_to_modelling <-  Wage %>%
  mutate(age_num = case_when(
    age_group == "20-" ~ 20,
    age_group == "70+" ~ 70,
    TRUE ~ as.numeric(as.character(age_group))
  ))

# Modele
model_glm <- glm(wage ~ age_num, data = Wage_to_modelling)
model_glm_2 <- glm(wage ~ age_num + I(age_num^2), data = Wage_to_modelling)
model_gam <- gam(wage ~ s(age_num), data = Wage_to_modelling)

summary(model_gam)

as.numeric(df_plot$age_group)

# # 3. Sprawdzamy wyniki
# summary(model_glm)

df_plot$predicted_wage_GLM <- predict(model_glm, newdata = df_plot)
df_plot$predicted_wage_GLM_2 <- predict(model_glm_2, newdata = df_plot)
df_plot$predicted_wage_GAM <- predict(model_gam, newdata = df_plot)


# 1. podstawowy wariant GLM
ggplot(df_plot) +
  geom_line(aes(x = age_group, y = predicted_wage_GLM, group = 1, color = "Model GLM"), linewidth = 2) +
  geom_point(aes(x = age_group, y = mean_wage, color = "Średnie empiryczne"), size = 4) +
  scale_color_manual(
    name = "Legenda", 
    values = c(
      "Model GLM" = "steelblue",
      "Model GAM" = "red", 
      "Średnie empiryczne" = "darkblue"
    )
  ) +
  
  scale_x_discrete(breaks = chosen_labels) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold", size=18),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30", size=16),
    legend.position = "bottom",
    axis.title.x = element_text(margin = margin(t = 15), size = 14),
    axis.title.y = element_text(margin = margin(r = 15), size = 14),
    axis.text = element_text(size = 11, color = "black"),
    legend.text = element_text(size = 14),           
    legend.key.size = unit(1.5, "cm"),              
    legend.key.width = unit(2, "cm")
  ) +
  labs(
    title = "Kształtowanie się średnich zarobków w zależności od wieku",
    subtitle = "Dane dla 3000 pracujących mężczyzn w USA (region Mid-Atlantic), lata 2003-2009",
    x = "Wiek pracownika",
    y = "Średnie wynagrodzenie (tys. USD)"
  )


# 2. wariant GLM z składnikiem kwadratowym
ggplot(df_plot) +
  geom_line(aes(x = age_group, y = predicted_wage_GLM_2, group = 1, color = "Model GLM (z zarobkami podniesionymi do kwadratu)"), linewidth = 2) +
  geom_point(aes(x = age_group, y = mean_wage, color = "Średnie empiryczne"), size = 4) +
  scale_color_manual(
    name = "Legenda", 
    values = c(
      "Model GLM (z zarobkami podniesionymi do kwadratu)" = "steelblue",
      "Model GAM" = "red", 
      "Średnie empiryczne" = "darkblue"
    )
  ) +
  
  scale_x_discrete(breaks = chosen_labels) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold", size=18),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30", size=16),
    legend.position = "bottom",
    axis.title.x = element_text(margin = margin(t = 15), size = 14),
    axis.title.y = element_text(margin = margin(r = 15), size = 14),
    axis.text = element_text(size = 11, color = "black"),
    legend.text = element_text(size = 14),           
    legend.key.size = unit(1.5, "cm"),              
    legend.key.width = unit(2, "cm")
  ) +
  labs(
    title = "Kształtowanie się średnich zarobków w zależności od wieku",
    subtitle = "Dane dla 3000 pracujących mężczyzn w USA (region Mid-Atlantic), lata 2003-2009",
    x = "Wiek pracownika",
    y = "Średnie wynagrodzenie (tys. USD)"
  )


# 3. GLM vs GAM
ggplot(df_plot) +
  geom_line(aes(x = age_group, y = predicted_wage_GLM, group = 1, color = "Model GLM"), linewidth = 2) +
  geom_line(aes(x = age_group, y = predicted_wage_GAM, group = 1, color = "Model GAM"), linewidth = 2) +
  geom_point(aes(x = age_group, y = mean_wage, color = "Średnie empiryczne"), size = 4) +
  scale_color_manual(
    name = "Legenda", 
    values = c(
      "Model GLM" = "steelblue",
      "Model GAM" = "red", 
      "Średnie empiryczne" = "darkblue"
    )
  ) +
  
  scale_x_discrete(breaks = chosen_labels) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold", size=18),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30", size=16),
    legend.position = "bottom",
    axis.title.x = element_text(margin = margin(t = 15), size = 14),
    axis.title.y = element_text(margin = margin(r = 15), size = 14),
    axis.text = element_text(size = 11, color = "black"),
    legend.text = element_text(size = 14),           
    legend.key.size = unit(1.5, "cm"),              
    legend.key.width = unit(2, "cm")
  ) +
  labs(
    title = "Kształtowanie się średnich zarobków w zależności od wieku",
    subtitle = "Dane dla 3000 pracujących mężczyzn w USA (region Mid-Atlantic), lata 2003-2009",
    x = "Wiek pracownika",
    y = "Średnie wynagrodzenie (tys. USD)"
  )



# 4. GLM ze składnikiem kwadratowym vs GAM
ggplot(df_plot) +
  geom_line(aes(x = age_group, y = predicted_wage_GLM_2, group = 1, color = "Model GLM (z zarobkami podniesionymi do kwadratu)"), linewidth = 2) +
  geom_line(aes(x = age_group, y = predicted_wage_GAM, group = 1, color = "Model GAM"), linewidth = 2) +
  geom_point(aes(x = age_group, y = mean_wage, color = "Średnie empiryczne"), size = 4) +
  scale_color_manual(
    name = "Legenda",
    values = c(
      "Model GLM (z zarobkami podniesionymi do kwadratu)" = "steelblue",
      "Model GAM" = "red", 
      "Średnie empiryczne" = "darkblue"
    )
  ) +
  
  scale_x_discrete(breaks = chosen_labels) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold", size=18),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30", size=16),
    legend.position = "bottom",
    axis.title.x = element_text(margin = margin(t = 15), size = 14),
    axis.title.y = element_text(margin = margin(r = 15), size = 14),
    axis.text = element_text(size = 11, color = "black"),
    legend.text = element_text(size = 14),           
    legend.key.size = unit(1.5, "cm"),              
    legend.key.width = unit(2, "cm")
  ) +
  labs(
    title = "Kształtowanie się średnich zarobków w zależności od wieku",
    subtitle = "Dane dla 3000 pracujących mężczyzn w USA (region Mid-Atlantic), lata 2003-2009",
    x = "Wiek pracownika",
    y = "Średnie wynagrodzenie (tys. USD)"
  )
