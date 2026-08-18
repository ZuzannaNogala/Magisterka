
library(mgcv)
library(tidyverse)
library(scales)
library(parallel)
library(pbmcapply)

# ANALIZA D - SELEKCJA ZMIENNYCH
# Zwycięsca w A: REML. Taką metodę wybieramy do dalszych analiz.

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

# Główna funkcja
simulation_D_function <- function(n_val, iter, noise_feature_correlation_status = FALSE, sigma=0.2){
  
  k_val <- case_when(
    n_val <= 200 ~ 20,
    n_val > 200 ~ 40
  )
  
  X <- matrix(runif(n_val * 5, 0, 10), ncol = 5)
  colnames(X) <- paste0("x_", 1:5)
  df <- as.data.frame(X)
  
  f_vals <- cbind(
    f1(df$x_1), f2(df$x_2), f3(df$x_3), f4(df$x_4), f5(df$x_5)
  )
  
  f_norm <- apply(f_vals, 2, rescale_vec)
  f_norm_centered <- scale(f_norm, scale = FALSE)
  
  true_f_matrix <- cbind(f_norm_centered, matrix(0, nrow = n_val, ncol = 3))
  colnames(true_f_matrix) <- paste0("x_", 1:8)
  
  df$y <- rowSums(f_norm) + rnorm(n_val, sd = sigma)
  
  # zmienne niezależne zaszumione - niezalezne
  if(noise_feature_correlation_status == FALSE){
    X_noisy <-  matrix(c(runif(n_val, 0, 0.5),
                         runif(n_val, 0, 10),
                         runif(n_val, 0, 50)), 
                       ncol = 3)
  }
  # zmienne niezależne zaszumione - skorelowane z istotnymi
  else{
    X_noisy <- matrix(c(df$x_1 + rnorm(n_val, sd = 0.5), # x_6: Bardzo silna korelacja (prawie kopia x_1)
                        df$x_2 + rnorm(n_val, sd = 3.0), # x_7: Umiarkowana korelacja (szum wielkości sygnału x_2)
                        df$x_3 + rnorm(n_val, sd = 8.0)),
                      ncol = 3)
  }
  
  colnames(X_noisy) <- paste0("x_", 6:8)
  df_noisy <- cbind(df, as.data.frame(X_noisy))
 
  gam_model <- mgcv::gam(y ~ s(x_1, k=k_val) + s(x_2, k=k_val) + s(x_3, k=k_val) + 
                           s(x_4, k=k_val) + s(x_5, k=k_val) + s(x_6, k = k_val) + 
                           s(x_7, k = k_val) + s(x_8, k = k_val),
                         data = df_noisy, method = 'REML', select = TRUE)
  
  s_mod <- summary(gam_model)
  k_vals <- mgcv::k.check(gam_model)
  
  preds_terms <- predict(gam_model, type = "terms")
  mse_terms <- colMeans((preds_terms - true_f_matrix)^2)
  
  return(data.frame(
    num_iter = iter,
    n = n_val,
    noise_correlated_status = noise_feature_correlation_status, 
    var_name = c(paste0('x', 1:8)),
    edf = s_mod$edf,
    term_mse = mse_terms,
    k_p_value = k_vals[, "p-value"],
    k_index =k_vals[, "k-index"],
    aic = AIC(gam_model), 
    bic = BIC(gam_model),
    signif_var = s_mod$s.table[, "p-value"],
    converged = gam_model$converged
  ))
}

# Parametry 

n_list <- c(200, 2000)
K <- 500

# Generowanie wyników w partiach

chunk_size <- 50 # Zapisujemy na dysk co 50 iteracji z K
num_chunks <- ceiling(K / chunk_size)

message(paste("Rozpoczęcie obliczeń - łączna liczba partii do wykonania:", num_chunks))

for (chunk in 7:num_chunks) {
  iter_start <- (chunk - 1) * chunk_size + 1
  iter_end <- min(chunk * chunk_size, K)
  
  params_D_chunk <- expand.grid(
    iter = iter_start:iter_end,
    n_val = n_list,
    noise_feature_correlation_status = c(TRUE)
  )
  
  message(paste("Partia", chunk, "z", num_chunks, "| Iteracje:", iter_start, "-", iter_end))
  
  chunk_results_list <- pbmclapply(1:nrow(params_D_chunk), function(i) {
    row <- params_D_chunk[i, ]
    simulation_D_function(n_val = row$n_val, 
                          noise_feature_correlation_status = row$noise_feature_correlation_status,
                          iter = row$iter)
  }, mc.cores = detectCores() - 1)
  
  chunk_df <- bind_rows(chunk_results_list)
  saveRDS(chunk_df, file = paste0("/Users/zuza/Desktop/studia/magisterka/Data/D_results/D_results_2_chunk_", chunk, ".RDS"))
  
  rm(chunk_results_list, chunk_df, params_D_chunk)
  gc()
}

message("Koniec!")

