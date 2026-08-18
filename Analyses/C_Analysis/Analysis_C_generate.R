library(mgcv)
library(tidyverse)
library(scales)
library(parallel)
library(pbmcapply)

# ANALIZA C - TYP BAZY SPLINE NA WYNIKI:
# Zwycięsca w A: REML. Taką metodę wybieramy do dalszych analiz.
# Nie uwzględniamy wyników z B, by gamma nie zaważyła o wyniku

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
simulation_C_function <- function(k_val, n_val, base_val, iter, sigma=0.2){
  X <- matrix(runif(n_val * 5, 0, 10), ncol = 5)
  colnames(X) <- paste0("x_", 1:5)
  df <- as.data.frame(X)
  
  f_vals <- cbind(
    f1(df$x_1), f2(df$x_2), f3(df$x_3), f4(df$x_4), f5(df$x_5)
  )
  
  f_norm <- apply(f_vals, 2, rescale_vec)
  f_norm_centered <- scale(f_norm, scale = FALSE)
  
  df$y <- rowSums(f_norm) + rnorm(n_val, sd = sigma)
  
  start_time <- Sys.time()
    
  gam_model <- tryCatch({
    mgcv::gam(y~s(x_1, k=k_val, bs=base_val) + 
                s(x_2, k=k_val, bs=base_val) + 
                s(x_3, k=k_val, bs=base_val) + 
                s(x_4, k=k_val, bs=base_val) + 
                s(x_5, k=k_val, bs=base_val),
              data = df, method = 'REML')
  }, error = function(e) {
    return(NULL) 
  })
  
  end_time <- Sys.time()
  
  fit_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  if (is.null(gam_model)) {
    return(data.frame(
      num_iter = iter, n = n_val, k = k_val, base = base_val,
      fun = paste0('f', 1:5), fit_time_sec = NA, edf = NA, term_mse = NA,
      k_p_value = NA, k_index = NA, converged = FALSE
    ))
  }
  
  s_mod <- summary(gam_model)
  k_vals <- mgcv::k.check(gam_model)
  preds_terms <- predict(gam_model, type = "terms")
  mse_terms <- colMeans((preds_terms - f_norm_centered)^2)
  
  conv_status <- gam_model$converged 
  
  rm(gam_model)
  
  return(data.frame(
    num_iter = iter, 
    n = n_val,
    k = k_val, 
    base = base_val,
    fun = c(paste0('f', 1:5)), 
    fit_time_sec = fit_time, 
    edf = s_mod$edf,
    term_mse = mse_terms,
    k_p_value = k_vals[, "p-value"],
    k_index = k_vals[, "k-index"], 
    converged = conv_status
  ))
}

# Parametry

k_list <- c(5, 10, 20, 30, 39) # 39 max by macierz planu nie była osobniwa num_par = 5 * (k - 1) + 1
base_list <- c('tp', 'cr', 'ps')
n_list <- c(200, 2000)

K <- 500

# Generowanie wyników w partiach

chunk_size <- 50 # Zapisujemy na dysk co 50 iteracji z K
num_chunks <- ceiling(K / chunk_size)

message(paste("Rozpoczęcie obliczeń - łączna liczba partii do wykonania:", num_chunks))

for (chunk in 1:num_chunks) {
  iter_start <- (chunk - 1) * chunk_size + 1
  iter_end <- min(chunk * chunk_size, K)
  
  params_C_chunk <- expand.grid(
    iter = iter_start:iter_end,
    k_val = k_list,
    base_val = base_list,
    n_val = n_list
  )
  
  message(paste("Partia", chunk, "z", num_chunks, "| Iteracje:", iter_start, "-", iter_end))
  
  chunk_results_list <- pbmclapply(1:nrow(params_C_chunk), function(i) {
    row <- params_C_chunk[i, ]
    simulation_C_function(k_val = row$k_val, 
                          n_val = row$n_val, 
                          base_val = row$base_val,
                          iter = row$iter)
  }, mc.cores = detectCores() - 1)
  
  chunk_df <- bind_rows(chunk_results_list)
  saveRDS(chunk_df, file = paste0("/Users/zuza/Desktop/studia/magisterka/Data/C_results/C_results_chunk_", chunk, ".RDS"))
  
  rm(chunk_results_list, chunk_df, params_C_chunk)
  gc() 
}

message("Koniec!")
