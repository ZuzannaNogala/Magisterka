library(mgcv)
library(tidyverse)
library(scales)
library(parallel)

# install.packages("pbmcapply")
library(pbmcapply)

# ANALIZA B 

# Zwycięsca w A: REML. Taką metodę wybieramy do dalszych analiz.

# Analiza najciekawszej funkcji f3 (sinusoidalna), by zobaczyć 
# jak $\gamma$ leczy overfitting na gładkiej krzywej


# Funkcje do analiz

f1 <- function(x) 12 * x - 5
f2 <- function(x) exp( -(x^2 - 20)/100 - 3 * x) * 10
f3 <- function(x) sin(- 0.5 * x * pi) * 5 + cos(pi * x)
f4 <- function(x) ifelse(x > 2.5, ifelse(x < 4, 4, 2), x^2)
f5 <- function(x) 0.9 * x^3 - 8 * x^2 - 4.8 * x + 6


rescale_vec <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if(rng[1] == rng[2]) return(x)
  (x - rng[1]) / (rng[2] - rng[1])
}

# Główna funkcja
simulation_B_function <- function(gamma_val, k_val, iter, sigma=0.2){
  N <- 500
  
  X <- matrix(runif(N * 5, 0, 10), ncol = 1)
  colnames(X) <- paste0("x_", 3)
  df <- as.data.frame(X)
  
  f_vals <- f3(df$x_3)
  
  f_norm <- rescale_vec(f_vals)
  f_norm_centered <- scale(f_norm, scale = FALSE)
  
  df$y <- f_norm +  rnorm(N, sd = sigma)
  
  gam_model <- mgcv::gam(y~s(x_3, k=k_val),
                         data = df, 
                         method = 'REML', 
                         gamma = gamma_val)
  
  
  s_mod <- summary(gam_model)
  k_vals <- mgcv::k.check(gam_model)
  
  preds_terms <- predict(gam_model, type = "terms")
  mse_terms <- mean((preds_terms - f_norm_centered)^2)
  
  return(data.frame(
    num_iter = iter,
    k = k_val,     
    gamma = gamma_val, 
    fun = 'f3',
    edf = s_mod$edf,
    term_mse = mse_terms,
    k_p_value = k_vals[, "p-value"],
    k_index = k_vals[, "k-index"],
    converged = gam_model$converged
  ))
}

# Parametry
k_list <- c(10, 20, 30, 40, 50)
gamma_list <- c(1.0, 1.2, 1.4, 1.6, 1.8, 2.0)

K <- 500

params_B <- expand.grid(
  iter = 1:K,
  k_val = c(10, 20, 30, 40, 50),
  gamma_val = c(1.0, 1.2, 1.4, 1.6, 1.8, 2.0)
)

# Generowanie wyników
message(paste("Rozpoczynam obliczenia dla", nrow(params_B), "kombinacji..."))

B_results_list <- pbmclapply(1:nrow(params_B), function(i) {
  row <- params_B[i, ]
  simulation_B_function(gamma_val = row$gamma_val, k_val = row$k_val, iter = row$iter)
}, mc.cores = detectCores() - 1)

B_final_df <- do.call(rbind, compact(B_results_list))

saveRDS(B_final_df, "/Users/zuza/Desktop/studia/magisterka/Data/B_results_df.RDS")



