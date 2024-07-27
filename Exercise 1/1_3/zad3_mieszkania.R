library(ggplot2)
library(dplyr)
library(scales)
library(ggrepel)
library(patchwork)

plot_l_mieszkan <- ggplot(data_l_mieszkan, aes(x = as.factor(okres), y = wartosc)) +
  geom_line(aes(group = 1), size = 1, color = "blue") +
  geom_point(size = 3, color = "blue") +
  geom_text_repel(
    aes(label = paste0(format(round(wartosc / 1000000, 2), big.mark = ",", scientific = FALSE), " mln")), 
    box.padding = 0.5,
    segment.size = 0.2,
    segment.color = "grey50",
    point.padding = 0.5,
    direction = "y",
    force = 2
  ) +
  labs(x = "Rok", y = "Miliony sztuk", title = "Liczba mieszkań w Polsce") +
  scale_x_discrete(breaks = unique(data_l_mieszkan$okres)) +
  scale_y_continuous(labels = function(x) x / 1000000) + # Scale values by 1 million
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot_pow_mieszkan <- ggplot(data_pow_mieszkan, aes(x = as.factor(okres), y = wartosc)) +
  geom_line(aes(group = 1), size = 1, color = "blue") +
  geom_point(size = 3, color = "blue") +
  labs(x = "Rok", y = expression("m"^"2"~""), title = "Przeciętna powierzchnia użytkowa mieszkania na jedną osobę") +
  scale_x_discrete(breaks = unique(data_pow_mieszkan$okres)) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot_bar_pow_mieszkan <- ggplot(data_pow_mieszkan, aes(x = as.factor(okres), y = wartosc)) +
  geom_bar(stat = "identity", fill = "chocolate4") +
  geom_text(aes(label = wartosc), vjust = 2, size = 5, color = "black", fontface = "bold") +
  labs(x = "Rok", y = expression("m"^"2"~""), title = "Przeciętna powierzchnia użytkowa mieszkania na jedną osobę") +
  scale_x_discrete(breaks = unique(data_pow_mieszkan$okres)) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot_bar_mediana <- ggplot(data_mediana_cen_mieszkan, aes(x = as.factor(okres), y = wartosc)) +
  geom_bar(stat = "identity", fill = "brown2") +
  geom_text(aes(label = wartosc), vjust = 2, size = 5, color = "black", fontface = "bold") +
  labs(x = "Rok", y = expression("m"^"2"~""), title = "Mediana cen za 1 m2 mieszkań") +
  scale_x_discrete(breaks = unique(data_pow_mieszkan$okres)) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot_bar_srednia <- ggplot(data_srednia_cen_mieszkan, aes(x = as.factor(okres), y = wartosc)) +
  geom_bar(stat = "identity", fill = "seagreen4") +
  geom_text(aes(label = wartosc), vjust = 2, size = 5, color = "black", fontface = "bold") +
  labs(x = "Rok", y = "[PLN]", title = "Średnia cen za 1 m2 mieszkań") +
  scale_x_discrete(breaks = unique(data_pow_mieszkan$okres)) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Combine the data frames
combined_data <- bind_rows(
  mutate(data_mediana_cen_mieszkan, type = "Mediana"),
  mutate(data_srednia_cen_mieszkan, type = "Średnia")
)

combined_plot <- ggplot(combined_data, aes(x = as.factor(okres), y = wartosc, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  geom_text(aes(label = wartosc), vjust = 0, hjust = -0.25, angle= 90, size = 5, color = "black", fontface = "bold", position = position_dodge(width = 0.9)) +
  labs(x = "Rok", y = "[PLN]", title = "Porównanie mediany i średniej cen za 1 m2 mieszkań") +
  scale_x_discrete(breaks = unique(combined_data$okres)) +
  scale_fill_manual(values = c("Mediana" = "brown2", "Średnia" = "seagreen4"), 
                    guide = guide_legend(override.aes = list(fill = c("brown2", "seagreen4")),
                                         title = NULL, name = NULL)) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ylim(0, 8000)


plot_bar_l_osob <- ggplot(data_l_osob, aes(x = as.factor(okres), y = wartosc)) +
  geom_bar(stat = "identity", fill = "purple2") +
  geom_text(aes(label = wartosc), vjust = 2, size = 5, color = "black", fontface = "bold") +
  labs(x = "Rok", y = "Liczba osób", title = "Przeciętna liczba osób w mieszkaniu") +
  scale_x_discrete(breaks = unique(data_pow_mieszkan$okres)) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot_populacja <- plot_l_mieszkan <- ggplot(data_populacja, aes(x = as.factor(okres), y = wartosc)) +
  geom_line(aes(group = 1), size = 1, color = "blue") +
  geom_point(size = 3, color = "blue") +
  geom_text_repel(
    aes(label = paste0(format(round(wartosc / 1000000, 2), big.mark = ",", scientific = FALSE), " mln")), 
    box.padding = 0.5,
    segment.size = 0.2,
    segment.color = "grey50",
    point.padding = 0.5,
    direction = "y",
    force = 2
  ) +
  labs(x = "Rok", y = "Miliony", title = "Liczba osób w Polsce") +
  scale_x_discrete(breaks = unique(data_l_mieszkan$okres)) +
  scale_y_continuous(labels = function(x) x / 1000000) + # Scale values by 1 million
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot_bar_mieszkania_na_osobe <- ggplot(data_miesz_pop[data_miesz_pop$zmienna == "Liczba mieszkań na jedną osobę", ], 
                                       aes(x = as.factor(okres), y = wartosc)) +
  geom_bar(stat = "identity", fill = "aquamarine3") +
  geom_text(aes(label = round(wartosc, 2)), vjust = 2, size = 5, color = "black", fontface = "bold") +  # Round the values to 2 digits
  labs(x = "Rok", y = "Mieszkania", title = "Liczba mieszkań na osobę w Polsce") +
  scale_x_discrete(breaks = unique(data_pow_mieszkan$okres)) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(plot_l_mieszkan)
print(plot_bar_pow_mieszkan)
print(plot_bar_mediana)
print(plot_bar_srednia)
print(combined_plot)
print(plot_bar_l_osob)

print(plot_populacja)

print(plot_bar_mieszkania_na_osobe)







