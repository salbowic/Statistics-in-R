library(ggplot2)
library(scales)

br <- read.csv("br.csv", sep = ";", header = TRUE)
br <- br[c("id_daty", "wartosc")]

cpi <- read.csv("cpi.csv", sep = ";", header = TRUE)
cpi <- cpi[c("id_daty", "wartosc")]
cpi <- cpi[cpi$id_daty <= 2022, ]

br1 <- br[br$id_daty >= 2009 & br$id_daty <= 2021, ]
br2 <- br[br$id_daty >= 2010 & br$id_daty <= 2022, ]

br2$wartosc <- br2$wartosc / br1$wartosc * 100 # wzrosty roczne
print(br2)

result <- br2
result$wartosc <- br2$wartosc / cpi$wartosc * 100 # wzrosty roczne w stosunku do CPI
print(result)

ggplot(result, aes(x = id_daty, y = wartosc)) +
  geom_line() +
  geom_point() +
  labs(title = "Roczny procentowy wzrost nakładów na Badania i Rozwój w stosunku do CPI", x = "Rok", y = "Stosunek [%]") +
  geom_hline(yintercept = 100, linetype = "dashed") +
  scale_x_continuous(breaks = pretty_breaks())
