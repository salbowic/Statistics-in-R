library(ggplot2)
library(scales)

bud <- read.csv("bud.csv", sep = ";", header = TRUE)
bud <- bud[c("id_daty", "wartosc")]
bud <- bud[bud$id_daty <= 2022, ]
print(bud)

ggplot(bud, aes(x=id_daty, y=wartosc)) + geom_line() + geom_point() +
  labs(title = "Wskaźnik cen produkcji budowlano-montażowej w skali rocznej", x = "Rok", y = "bud") +
  geom_hline(yintercept=100, linetype="dashed") + 
  scale_x_continuous(breaks = pretty_breaks())