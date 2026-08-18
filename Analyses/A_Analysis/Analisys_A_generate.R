library(mgcv)
library(tidyverse)
library(scales)
library(parallel)

# ANALIZA A - metoda estymacji stopni swobody.

# Funkcje
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

# Główna funkcja do analizy A
simulation_A_function <- function(N, iter, sigma=0.2, method="REML"){
  k_val <- case_when(
    N == 50 ~ 8,
    N == 100 ~ 12,
    N == 200 ~ 20,
    N >= 1000 ~ 40
  )
  
  X <- matrix(runif(N * 5, 0, 10), ncol = 5)
  colnames(X) <- paste0("x_", 1:5)
  df <- as.data.frame(X)
  
  f_vals <- cbind(
    f1(df$x_1), f2(df$x_2), f3(df$x_3), f4(df$x_4), f5(df$x_5)
  )
  f_norm <- apply(f_vals, 2, rescale_vec)
  f_norm_centered <- scale(f_norm, scale = FALSE)

  df$y <- rowSums(f_norm) + rnorm(N, sd = sigma)
  
  gam_model <- mgcv::gam(y~s(x_1, k=k_val) +
                           s(x_2, k=k_val) + 
                           s(x_3, k=k_val) + 
                           s(x_4, k=k_val) + 
                           s(x_5, k=k_val),
                         data = df, method = method)
  
  
  s_mod <- summary(gam_model)
  k_vals <- mgcv::k.check(gam_model)
  
  preds_terms <- predict(gam_model, type = "terms")
  mse_terms <- colMeans((preds_terms - f_norm_centered)^2)
  
  return(data.frame(
    num_iter = iter,
    method = rep(method, 5),
    fun = paste0('f', 1:5),
    n = rep(N, 5),
    edf = s_mod$edf,
    term_mse = mse_terms,
    k_p_value = k_vals[, "p-value"],
    k_index = k_vals[, "k-index"],
    converged = rep(gam_model$converged, 5)
  ))
}

# Parametry
n_list <- c(50, 100, 200, 1000, 5000)
K <- 400

params <- expand.grid(
  iter = 1:K, 
  method = c("REML", "GCV.Cp", "ML"),
  N = n_list
)


# Generowanie wyników
message(paste("Rozpoczynam obliczenia dla", nrow(params), "kombinacji..."))

A_results_list <- pbmclapply(1:nrow(params), function(i) {
  row <- params[i, ]
  res <- simulation_A_function(N = row$N, iter = row$iter, sigma = 0.2, method = row$method)
  return(res)
}, mc.cores = 3)

A_final_df <- do.call(rbind, A_results_list)


# saveRDS(A_final_df, "/Users/zuza/Desktop/studia/magisterka/Magisterka_kod/A_results_df_400.RDS")
# saveRDS(A_final_df, "/Users/zuza/Desktop/studia/magisterka/Magisterka_kod/A_results_df.RDS")

A_final_df <- readRDS("/Users/zuza/Desktop/studia/magisterka/Magisterka_kod/A_results_df.RDS")






