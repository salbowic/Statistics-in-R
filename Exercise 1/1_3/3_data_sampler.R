library(dplyr)

# Set the working directory to the directory containing your CSV files
setwd("D:/dokumenty/Szkola/STUDIA/MAGISTERSKIE/SEMESTR_2/SAD/projekt/data")

# Read data
data_mieszkania <- read.csv("mieszkania.csv", sep = ";")

# Remove rows with empty values in the "wartosc" column
data_mieszkania <- data_mieszkania %>%
  filter(!is.na(wartosc) & wartosc != "")

data_mieszkania <- data_mieszkania[c("zmienna", "typ_informacji", "opis_okres", "wartosc")]

names(data_mieszkania)[which(names(data_mieszkania) == "typ_informacji")] <- "jednostka"
names(data_mieszkania)[which(names(data_mieszkania) == "opis_okres")] <- "okres"

data_mieszkania$okres <- factor(data_mieszkania$okres)

# Replace "," with "." in the "wartosc" column
data_mieszkania$wartosc <- gsub(",", ".", data_mieszkania$wartosc)
data_mieszkania$wartosc <- as.numeric(data_mieszkania$wartosc)

data_l_mieszkan <- data_mieszkania[data_mieszkania$zmienna == "Mieszkania", ]

data_mediana_cen_mieszkan <- data_mieszkania[data_mieszkania$zmienna == "Mediana cen za 1 m2 lokali mieszkalnych sprzedanych w ramach transakcji rynkowych", ]

data_srednia_cen_mieszkan <- data_mieszkania[data_mieszkania$zmienna == "Średnie ceny za 1 m2 lokali mieszkalnych sprzedanych w ramach transakcji rynkowych", ]

data_l_osob <- data_mieszkania[data_mieszkania$zmienna == "Przeciętna liczba osób w mieszkaniu", ]

data_pow_mieszkan <- data_mieszkania[data_mieszkania$zmienna == "Przeciętna powierzchnia użytkowa mieszkania", ]

data_populacja <- read.csv("populacja.csv", sep = ";")
data_populacja <- data_populacja[c("zmienna", "opis_okres", "wartosc")]
names(data_populacja)[which(names(data_populacja) == "opis_okres")] <- "okres"
data_populacja$wartosc <- as.numeric(data_populacja$wartosc)
data_populacja$okres <- as.integer(data_populacja$okres)


data_miesz_pop <- data.frame(zmienna = character(),
                             okres = integer(),
                             wartosc = numeric())

years <- as.integer(2010:2022)

for (zm in c("Liczba osób na jedno mieszkanie", "Liczba mieszkań na jedną osobę")) {
  for (rok in years) {
    if (zm == "Liczba osób na jedno mieszkanie") {
      wart <- data_populacja$wartosc[data_populacja$okres == rok] / data_l_mieszkan$wartosc[data_l_mieszkan$okres == rok]
    } else {
      wart <- data_l_mieszkan$wartosc[data_l_mieszkan$okres == rok] / data_populacja$wartosc[data_populacja$okres == rok]
    }
    data_miesz_pop <- rbind(data_miesz_pop, data.frame(zmienna = zm, okres = rok, wartosc = wart))
  }
}






