# Załaduj niezbędne pakiety
library(ggplot2)
library(dplyr)
library(gridExtra)

X <- read.csv("X.csv", sep = ",")
Y <- read.csv("Y.csv", sep = ",")

combined_data <- bind_rows(X, Y)

plot_combined_XY <- ggplot(combined_data, aes(x = dataset, y = inflacja)) +
  geom_boxplot(aes(fill = dataset)) +
  labs(title = "Wykresy pudełkowe wskaźnika inflacji studenckiej i oficjalnej",
       x = NULL,
       y = "Inflacja (CPI)",
       fill = "próba") +
  scale_fill_manual(values = c("Studencka" = "lightblue", "Oficjalna" = "lightgreen")) +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12, face = "bold")
  )

print(plot_combined_XY)

# Merge X and Y datasets based on "okres"
merged_data <- merge(X, Y, by="okres")

# Calculate the difference in inflation
merged_data$inflacja_difference <- merged_data$inflacja.x - merged_data$inflacja.y

# Create a new dataset with the same "okres" values as in X and Y
XY_difference <- data.frame(okres = merged_data$okres, inflacja = merged_data$inflacja_difference)

plot_XY_difference <- ggplot(XY_difference, aes(x = "różnica inflacji", y = inflacja)) +
  geom_boxplot(fill = "purple") +
  labs(title = "Wykres pudełkowy różnic inflacji (Studencka X - Oficjalna Y)",
       x = NULL,
       y = "Różnica inflacji (CPI)") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12, face = "bold")
  )

print(plot_XY_difference)


breaks <- seq(0, 200, by = 5)

histogram_overlapped <- ggplot(combined_data, aes(x = inflacja, fill = dataset)) +
  geom_histogram(binwidth = 5, alpha = 0.5, position = "identity", color = "black") +
  scale_x_continuous(breaks = breaks, labels = breaks) +
  labs(x = "Wartość inflacji (CPI)", y = "Liczba wystąpień", title = "Histogram wartości z próby studenckiej i oficjalnej inflacji", fill = "Próba") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12, face = "bold")
  )

print(histogram_overlapped)

breaks <- seq(-100, 100, by = 2)

histogram_inflacja <- ggplot(XY_difference, aes(x = inflacja)) +
  geom_histogram(binwidth = 2, color = "black", fill = "lightblue") +
  scale_x_continuous(breaks = breaks, labels = breaks) +
  labs(x = "Różnica", y = "Liczba wystąpień", title = "Histogram różnic inflacji studenckiej i oficjalnej", fill = "Próba") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12, face = "bold")
  )


print(histogram_inflacja)

inflacja_X <- X %>% pull(inflacja)
inflacja_Y <- Y %>% pull(inflacja)
wilcox.test(inflacja_X, inflacja_Y, paired = FALSE, alternative = "greater")
