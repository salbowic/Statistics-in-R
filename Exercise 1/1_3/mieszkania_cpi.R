library(ggplot2)
library(scales)
library(ggrepel)

mi <- read.csv("mieszkania.csv", sep = ";", header = TRUE)
mi <- mi[mi$zmienna == "Mediana cen za 1 m2 lokali mieszkalnych sprzedanych w ramach transakcji rynkowych", ]
mi <- mi[c("id_daty", "wartosc")]
mi <- mi[mi$wartosc >= 0, ]
#print(mi)

cpi <- read.csv("cpi.csv", sep = ";", header = TRUE)
cpi <- cpi[c("id_daty", "wartosc")]
cpi <- cpi[cpi$id_daty >= 2011 & cpi$id_daty <= 2022, ]
#print(cpi)

mi1 <- mi[mi$id_daty >= 2010 & mi$id_daty <= 2021, ]
mi2 <- mi[mi$id_daty >= 2011 & mi$id_daty <= 2022, ]

print(mi1)
print(mi2)

mi2$wartosc <- as.numeric(mi2$wartosc) / as.numeric(mi1$wartosc) * 100 # wzrosty roczne
print(mi2)

result <- mi2
result$wartosc <- mi2$wartosc / cpi$wartosc * 100 # wzrosty roczne w stosunku do CPI
print(result)

ggplot(result, aes(x = id_daty, y = wartosc)) +
  geom_line() +
  geom_point() +
  labs(title = "Roczny procentowy wzrost mediany cen mieszkań w stosunku do CPI", x = "Rok", y = "Stosunek [%]") +
  geom_hline(yintercept = 100, linetype = "dashed") +
  scale_x_continuous(breaks = pretty_breaks()) +
  geom_text_repel(
    aes(label = paste0(format(round(wartosc, 2), big.mark = ",", scientific = FALSE), "%")), 
    box.padding = 0.5,
    segment.size = 0.2,
    segment.color = "grey50",
    point.padding = 0.5,
    direction = "y",
    force = 2
  )
