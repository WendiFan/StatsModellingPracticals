LinMod_sim_F <- function(X, G0 = 1:ncol(X), Beta = rep(0, p), errors_gen = rnorm, B = 10000) {
  n <- nrow(X)
  p <- ncol(X)
  p0 <- length(G0)
  X0 <- X[,G0]
  y_mat <- as.vector(X %*% Beta) + matrix(errors_gen(n*B), n, B)
  inv_gram <- solve(t(X) %*% X)
  P <- X %*% inv_gram %*% t(X)
  P0 <- X0 %*% solve(t(X0) %*% X0)  %*% t(X0)
  Beta_hat_mat <- inv_gram %*% t(X) %*% y_mat
  resid_mat <- y_mat - P %*% y_mat
  resid_mat0 <- (P-P0) %*% y_mat
  sigma_tilde_vec <- colSums(resid_mat^2) / (n - p)
  t_stat_mat <- Beta_hat_mat / sqrt(diag(inv_gram))
  t_stat_mat <- t_stat_mat / rep(sqrt(sigma_tilde_vec), each = p)
  F_vec <- colSums(resid_mat0^2) / (p-p0) / sigma_tilde_vec
  return(list("t"=t_stat_mat,"F"=F_vec))
}

qqplot_F <- function(f_stat, df1, df2, conf_level = 0.95) {
  f_stat <- sort(f_stat)
  B <- length(f_stat)
  theoretical_quantiles <- qf((1:B) / (B + 1), df1, df2)
  plot(theoretical_quantiles, f_stat)
  abline(0, 1, col = "red")
  return(mean(f_stat <= qf(conf_level, df1, df2)))
}