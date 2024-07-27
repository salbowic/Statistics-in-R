library(ggplot2)
library(dplyr)

# Set the working directory to the directory containing your CSV files
setwd("D:/dokumenty/Szkola/STUDIA/MAGISTERSKIE/SEMESTR_2/SAD/projekt/data")

data1 <- read.csv("tow_zywn_2010-2011.csv", sep = ";")
data2 <- read.csv("tow_zywn_2012-2014.csv", sep = ";")
data3 <- read.csv("tow_zywn_2015-2017.csv", sep = ";")
data4 <- read.csv("tow_zywn_2018-2020.csv", sep = ";")
data5 <- read.csv("tow_zywn_2021-2024.csv", sep = ";")


data <- rbind(data1, data2, data3, data4, data5)

data <- data[c("nazwa_pozycja_2", "opis_okres", "wartosc")]

data <- data %>%
  mutate(nazwa = ifelse(nazwa_pozycja_2 == "mleko krowie spożywcze o zawartości tłuszczu 3-3,5%, sterylizowane za 1 l", "mleko krowie (3-3,5%) za 1 l", nazwa_pozycja_2)) %>%
  select(-nazwa_pozycja_2)

# Rename columns
names(data)[which(names(data) == "nazwa_pozycja_2")] <- "nazwa"
names(data)[which(names(data) == "opis_okres")] <- "okres"
names(data)[which(names(data) == "wartosc")] <- "wartosc"

# Convert "okres" to Date format
data$okres <- as.Date(paste0(substr(data$okres, 1, 4), "-", substr(data$okres, 7, 8), "-01"))

data$wartosc <- gsub(",", ".", data$wartosc)
data$wartosc <- as.numeric(data$wartosc)

# Add a new column 'inflacja' to the data
data <- data %>%
  group_by(nazwa) %>%
  arrange(nazwa, okres) %>%
  mutate(inflacja = (wartosc) / lag(wartosc, 12) * 100) %>%
  ungroup()

write.csv(data, "tow_zywn_rel.csv", row.names = FALSE)

# Read data
inf_data1 <- read.csv("wskaznik_infl_zywn_2010-2011.csv", sep = ";")
inf_data2 <- read.csv("wskaznik_infl_zywn_2012-2014.csv", sep = ";")
inf_data3 <- read.csv("wskaznik_infl_zywn_2015-2017.csv", sep = ";")
inf_data4 <- read.csv("wskaznik_infl_zywn_2018-2020.csv", sep = ";")
inf_data5 <- read.csv("wskaznik_infl_zywn_2021-2024.csv", sep = ";")


inf_data <- rbind(inf_data1, inf_data2, inf_data3, inf_data4, inf_data5)

inf_data <- inf_data[c("nazwa_pozycja_2", "opis_okres", "wartosc")]

# Rename columns
names(inf_data)[which(names(inf_data) == "nazwa_pozycja_2")] <- "nazwa"
names(inf_data)[which(names(inf_data) == "opis_okres")] <- "okres"
names(inf_data)[which(names(inf_data) == "wartosc")] <- "wartosc"

write.csv(inf_data, "wskaznik_infl_zywn_rel.csv", row.names = FALSE)

# Convert "okres" to Date format
inf_data$okres <- as.Date(paste0(substr(inf_data$okres, 1, 4), "-", substr(inf_data$okres, 7, 8), "-01"))

inf_data$wartosc <- as.numeric(gsub(",", ".", inf_data$wartosc))

# Ratio
merged_data <- merge(aggregated_data_2011, inf_data_filtered, by = "okres", suffixes = c("_aggregated", "_filtered"))

# Selecting and renaming columns
merged_data <- merged_data %>%
  select(okres, inflacja, nazwa, wartosc_filtered) %>%
  rename(
    okres = okres,
    inflacja_wyliczona = inflacja,
    nazwa = nazwa,
    wartosc_cpi = wartosc_filtered
  )

merged_data <- merged_data %>%
  mutate(stosunek = inflacja_wyliczona / wartosc_cpi)

