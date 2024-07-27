# Ustawienia symulacji
set.seed(1234567)
n_sim <- 1000  # liczba prób
n_samples <- c(10, 100, 1000, 10000)  # różne rozmiary próbek

true_lambda <- 5

# Funkcja do estymacji MLE parametru lambda z próbki
mle_lambda <- function(sample) {
  1 / mean(sample)  # MLE dla lambda w rozkładzie wykładniczym
}

# Funkcja normalizująca estymatory MLE
normalize_mle <- function(mle_estimates, true_lambda, n_sample) {
  std_error <- sqrt(true_lambda^2 / n_sample)
  normalized_mle_estimates <- (mle_estimates - true_lambda) / std_error
  return(normalized_mle_estimates)
}

# Tworzenie pustego wykresu
plot_title <- "Asymptotyczna normalność MLE dla rozkładu wykładniczego dla różnych rozmiarów próbek n"
plot(NULL, main = plot_title, xlab = expression((hat(lambda) - lambda) / sqrt(lambda^2 / n)), ylab = "Density", xlim = c(-3, 3), ylim = c(0, 0.45))

# Dodawanie kolejnych gęstości estymatorów MLE do wykresu
for (n_sample in n_samples) {
  mle_estimates <- numeric(n_sim)
  for (i in 1:n_sim) {
    sample <- rexp(n_sample, rate = true_lambda)
    mle_estimates[i] <- mle_lambda(sample)
  }
  normalized_mle_estimates <- normalize_mle(mle_estimates, true_lambda, n_sample)
  density_est <- density(normalized_mle_estimates)
  lines(density_est$x, density_est$y, col = rainbow(length(n_samples))[which(n_samples == n_sample)], lwd = 2)
  cat("Średnia znormalizowanych estymatorów MLE dla n =", n_sample, ":", mean(normalized_mle_estimates), "\n")
  cat("Odchylenie standardowe znormalizowanych estymatorów MLE dla n =", n_sample, ":", sd(normalized_mle_estimates), "\n\n")
}

# Dodanie krzywej gęstości rozkładu normalnego
curve(dnorm(x, mean = 0, sd = 1), col = "black", lwd = 2, lty = 2, add = TRUE)

# Dodanie linii pionowej w miejscu znormalizowanej wartości lambda
abline(v = 0, col = "blue", lwd = 2, lty = 2)

# Legenda
legend("topright", legend = paste("n =", n_samples), col = rainbow(length(n_samples)), lwd = 2)

