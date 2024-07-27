library(ggplot2)
library(dplyr)

products_to_remove <- c("szynka wieprzowa gotowana za 1 kg",
                        "olej rzepakowy produkcji krajowej za 1 l",
                        "cukier biały kryształ za 1 kg",
                        "jabłka za 1 kg")

# Filter data to make it more visible
filtered_data <- subset(data, nazwa %in% products_to_remove)
filtered_data <- filtered_data %>% filter(okres >= as.Date("2011-01-01"))

plot1 <- ggplot(filtered_data, aes(x = okres, y = wartosc, group = nazwa, color = nazwa)) +
  geom_line(size = 1) +
  labs(x = "Data", y = "Cena [PLN]", title = "Ceny towarów żywnościowych w poszczególnych miesiącach") +
  scale_color_discrete(name = "Produkt") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(legend.position = "right",
        legend.box = "vertical",
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"))

plot2 <- ggplot(filtered_data, aes(x = okres, y = inflacja, group = nazwa, color = nazwa)) +
  geom_line(size = 1) +
  labs(x = "Data", y = "CPI [analogiczny okres roku poprzedniego = 100]", title = "CPI towarów żywnościowych w poszczególnych miesiącach") +
  scale_color_discrete(name = "Produkt") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(legend.position = "right",
        legend.box = "vertical",
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold")) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black")


# Shopping basket multipliers
multipliers <- c(
  "chleb pszenno-żytni za 0.5 kg" = 1,
  "mąka pszenna za 1 kg" = 1,
  "szynka wieprzowa gotowana za 1 kg" = 0.3,
  "mleko krowie spożywcze o zawartości tłuszczu 3-3,5%, sterylizowane za 1 l" = 3,
  "jaja kurze świeże za 1 szt" = 6,
  "olej rzepakowy produkcji krajowej za 1 l" = 1,
  "jabłka za 1 kg" = 0.5,
  "ziemniaki za 1 kg" = 1,
  "cukier biały kryształ za 1 kg" = 1
)

aggregated_data <- data %>%
  filter(nazwa %in% names(multipliers)) %>%
  group_by(okres) %>%
  summarize(wartosc = sum(wartosc * multipliers[nazwa])) %>%
  mutate(inflacja = (wartosc) / lag(wartosc, 12) * 100)

aggregated_data_2011 <- aggregated_data %>% filter(okres >= as.Date("2011-01-01"))

plot3 <- ggplot(aggregated_data, aes(x = okres, y = wartosc)) +
  geom_line(size = 1.5, color = "black") +
  labs(x = "Data", y = "Cena [PLN]", title = "Cena koszyka z zakupami w poszczególnych miesiącach") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(20, 55, by = 5)) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  )


plot4 <- ggplot(aggregated_data_2011, aes(x = okres, y = inflacja)) +
  geom_line(size = 1.5, color = "black") +
  labs(x = "Data", y = "CPI [analogiczny okres roku poprzedniego = 100]", title = "CPI koszyka z zakupami w poszczególnych miesiącach") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#  scale_y_continuous(breaks = seq(20, 55, by = 5)) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  )+
  geom_hline(yintercept = 100, linetype = "dashed", color = "black")

#-----------------------Inflation---------------------------#

products_to_plot <- c("Ogółem", "żywność i napoje bezalkoholowe")

inf_data_filtered <- inf_data %>% filter(nazwa %in% products_to_plot)

plot5 <- ggplot(inf_data_filtered, aes(x = okres, y = wartosc, color = nazwa)) +
  geom_line(size = 1.5) +
  labs(x = "Data", y = "CPI [analogiczny okres roku poprzedniego = 100]", title = "CPI w poszczególnych miesiącach dla towarów żywnościowych", color = "Zakres inflacji") +
  scale_color_manual(values = c("Ogółem" = "blue", "żywność i napoje bezalkoholowe" = "red"),
                      labels = c("Ogółem" = "Ogółem dla towarów i usług konsumpcyjnych", "żywność i napoje bezalkoholowe" = "Żywność i napoje bezalkoholowe")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold")) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black")

#-----------------------Ratio---------------------------#

merged_data_filtered <- merged_data %>%
  filter(nazwa != "Ogółem")

plot6 <- ggplot(merged_data_filtered, aes(x = okres, y = stosunek)) +
  geom_line(size = 1.2, color = "black") +
  labs(x = "Data", y = "Stosunek", title = "Stosunek wyliczonego CPI koszyka do CPI żywności i napojów bezalkoholowych") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black")


print(plot1)
print(plot2)
print(plot3)
print(plot4)
print(plot5)
print(plot6)


