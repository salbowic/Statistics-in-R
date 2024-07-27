library(ggplot2)
library(scales)

br_s <- read.csv("F:/PW/SAD/br_s.csv", sep = ";", header = TRUE)

br_s <- br_s[br_s$typ_informacji == "[tys. zł]", ]

br_s_bes <- br_s[br_s$nazwa_pozycja_2 == "Sektor przedsiębiorstw (BES)", ]
br_s_gov <- br_s[br_s$nazwa_pozycja_2 == "Sektor rządowy (GOV)", ]
br_s_hes <- br_s[br_s$nazwa_pozycja_2 == "Sektor szkolnictwa wyższego (HES)", ]

ggplot() +
  geom_line(data = br_s_bes, aes(x = id_daty, y = wartosc, color = "Sektor przedsiębiorstw (BES)")) +
  geom_point(data = br_s_bes, aes(x = id_daty, y = wartosc, color = "Sektor przedsiębiorstw (BES)")) +
  geom_line(data = br_s_gov, aes(x = id_daty, y = wartosc, color = "Sektor rządowy (GOV)")) +
  geom_point(data = br_s_gov, aes(x = id_daty, y = wartosc, color = "Sektor rządowy (GOV)")) +
  geom_line(data = br_s_hes, aes(x = id_daty, y = wartosc, color = "Sektor szkolnictwa wyższego (HES)")) +
  geom_point(data = br_s_hes, aes(x = id_daty, y = wartosc, color = "Sektor szkolnictwa wyższego (HES)")) +
  labs(title = "Struktura nakładów na działalność B+R według sektorów wykonawczych - kwoty", x = "Rok", y = "[tys. zł]") +
  scale_color_manual(
    name = "Sektory",
    values = c("Sektor przedsiębiorstw (BES)" = "blue", "Sektor rządowy (GOV)" = "red", "Sektor szkolnictwa wyższego (HES)" = "green"),
    labels = c("Sektor przedsiębiorstw (BES)", "Sektor rządowy (GOV)", "Sektor szkolnictwa wyższego (HES)")
  )
